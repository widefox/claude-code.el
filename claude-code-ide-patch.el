;;; claude-code-ide-patch.el --- Patch to add Python bridge to claude-code.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains the additions needed to integrate Python bridge into claude-code.el
;; Add this code to claude-code.el after the WebSocket server functions

;;; Code:

;;;; Python Bridge Integration
;; Add these variables after the existing IDE variables (around line 120)

(defvar claude-code--python-server-process nil
  "The Python IDE server process.")

(defvar claude-code--python-server-port nil
  "The port the Python server is running on.")

(defvar claude-code--python-server-buffer "*claude-python-server*"
  "Buffer for Python server output.")

(defvar claude-code--python-server-script nil
  "Path to the Python server script.")

(defvar claude-code--selection-websocket nil
  "WebSocket connection for selection updates.")

(defvar claude-code--selection-timer nil
  "Timer for debouncing selection updates.")

(defvar claude-code--last-selection nil
  "Last selection data to avoid duplicates.")

;; Add this function to find the Python server script
(defun claude-code--find-python-server-script ()
  "Find the Python server script."
  (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
         (scripts '("claude-ide-server-with-selection.py"
                   "claude-ide-server-simple.py")))
    (cl-loop for script in scripts
             for path = (expand-file-name script this-dir)
             when (file-exists-p path)
             return path)))

;; Add Python server management functions
(defun claude-code--start-python-server ()
  "Start the Python IDE server.
Returns the port number on success, nil on failure."
  ;; Stop any existing server
  (claude-code--stop-python-server)
  
  ;; Find the server script
  (unless claude-code--python-server-script
    (setq claude-code--python-server-script (claude-code--find-python-server-script)))
  
  (unless claude-code--python-server-script
    (error "Python IDE server script not found"))
  
  (let* ((script-dir (file-name-directory claude-code--python-server-script))
         (workspace (or (when (project-current)
                         (project-root (project-current)))
                       default-directory))
         (venv-path (expand-file-name ".venv" script-dir))
         (has-venv (file-directory-p venv-path))
         (python-cmd (if has-venv
                        (format "cd %s && source .venv/bin/activate && python"
                               (shell-quote-argument script-dir))
                      "python3")))
    
    ;; Start the server
    (let ((process-connection-type nil)) ; Use pipes
      (setq claude-code--python-server-process
            (start-process "claude-python-server"
                          claude-code--python-server-buffer
                          "bash" "-c"
                          (format "%s %s %s"
                                 python-cmd
                                 (shell-quote-argument claude-code--python-server-script)
                                 (shell-quote-argument workspace)))))
    
    (if claude-code--python-server-process
        (progn
          ;; Set up process sentinel
          (set-process-sentinel 
           claude-code--python-server-process
           #'claude-code--python-server-sentinel)
          
          ;; Wait for server to start and extract port
          (message "Starting Python IDE server...")
          
          ;; Wait for output with timeout
          (let ((max-wait 10) ; 10 seconds max
                (waited 0)
                port-found)
            (while (and (< waited max-wait) (not port-found))
              (sit-for 0.5)
              (setq waited (+ waited 0.5))
              
              ;; Check for port in output
              (with-current-buffer claude-code--python-server-buffer
                (goto-char (point-min))
                ;; Look for port in server output - try multiple patterns
                (when (or (re-search-forward "server listening on [^:]+:\\([0-9]+\\)" nil t)
                          (re-search-forward "Port: \\([0-9]+\\)" nil t)
                          (re-search-forward "WebSocket Port: \\([0-9]+\\)" nil t))
                  (setq claude-code--python-server-port 
                        (string-to-number (match-string 1)))
                  (setq port-found t))))
            
            (if port-found
                (progn
                  (message "Python IDE server started on port %d!" 
                          claude-code--python-server-port)
                  
                  ;; Write lock file for Claude discovery
                  (claude-code--write-lock-file claude-code--python-server-port)
                  
                  ;; Set up selection tracking if available
                  (when (string-match-p "with-selection" claude-code--python-server-script)
                    (claude-code--setup-selection-tracking))
                  
                  ;; Update the global port variable
                  (setq claude-code--websocket-port claude-code--python-server-port)
                  
                  claude-code--python-server-port)
              
              ;; Show the buffer contents for debugging
              (display-buffer claude-code--python-server-buffer)
              (error "Failed to get server port"))))
      (error "Failed to start Python IDE server"))))

(defun claude-code--stop-python-server ()
  "Stop the Python IDE server."
  ;; Close selection WebSocket if open
  (when (and claude-code--selection-websocket
             (featurep 'websocket))
    (ignore-errors (websocket-close claude-code--selection-websocket))
    (setq claude-code--selection-websocket nil))
  
  ;; Stop server process
  (when claude-code--python-server-process
    (delete-process claude-code--python-server-process)
    (setq claude-code--python-server-process nil)
    (setq claude-code--python-server-port nil))
  
  ;; Remove selection tracking
  (remove-hook 'post-command-hook #'claude-code--schedule-selection-update)
  
  ;; Cancel timer
  (when claude-code--selection-timer
    (cancel-timer claude-code--selection-timer)
    (setq claude-code--selection-timer nil)))

(defun claude-code--python-server-sentinel (process event)
  "Handle PROCESS EVENT for Python server."
  (when (string-match "\\(finished\\|exited\\|killed\\)" event)
    (message "Python IDE server %s" event)
    (claude-code--stop-python-server)))

;; Selection tracking functions
(defun claude-code--setup-selection-tracking ()
  "Set up selection tracking with the Python server."
  (when (and claude-code--python-server-port
             (featurep 'websocket))
    ;; Connect WebSocket for selection updates
    (condition-case err
        (progn
          (setq claude-code--selection-websocket
                (websocket-open
                 (format "ws://127.0.0.1:%d" claude-code--python-server-port)
                 :on-message (lambda (_ws frame)
                             (when claude-code-websocket-debug
                               (message "Selection WS response: %s" 
                                       (websocket-frame-text frame))))
                 :on-error (lambda (_ws type err)
                           (message "Selection WS error: %s %s" type err))
                 :on-close (lambda (_ws)
                           (setq claude-code--selection-websocket nil))
                 :protocols '("mcp")))
          
          ;; Add selection tracking hook
          (add-hook 'post-command-hook #'claude-code--schedule-selection-update)
          (message "Selection tracking enabled"))
      (error
       (message "Failed to set up selection tracking: %s" err)))))

(defun claude-code--schedule-selection-update ()
  "Schedule a selection update with debouncing."
  (when claude-code--selection-websocket
    ;; Cancel any pending timer
    (when claude-code--selection-timer
      (cancel-timer claude-code--selection-timer))
    ;; Schedule new update
    (setq claude-code--selection-timer
          (run-with-timer 0.1 nil #'claude-code--send-selection-update))))

(defun claude-code--send-selection-update ()
  "Send current selection to Python server via WebSocket."
  (when (and claude-code--selection-websocket
             (websocket-openp claude-code--selection-websocket))
    (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
           (end (if (use-region-p) (region-end) (line-end-position)))
           (text (buffer-substring-no-properties start end))
           (file (or (buffer-file-name) ""))
           (start-line (1- (line-number-at-pos start))) ; 0-indexed
           (start-char (save-excursion (goto-char start) (current-column)))
           (end-line (1- (line-number-at-pos end)))     ; 0-indexed
           (end-char (save-excursion (goto-char end) (current-column)))
           (selection-data `((text . ,text)
                           (filePath . ,file)
                           (startLine . ,start-line)
                           (startChar . ,start-char)
                           (endLine . ,end-line)
                           (endChar . ,end-char))))
      
      ;; Only send if selection has changed
      (unless (equal selection-data claude-code--last-selection)
        (setq claude-code--last-selection selection-data)
        
        ;; Send via WebSocket using custom method
        (let ((message (json-encode
                       `((jsonrpc . "2.0")
                         (method . "updateSelection")
                         (params . ,selection-data)))))
          (websocket-send-text claude-code--selection-websocket message))))))

;; Replace the existing claude-code--start-websocket-server function with this:
(defun claude-code--start-websocket-server ()
  "Start the WebSocket server for IDE integration.

This enhanced version automatically falls back to a Python bridge
if the native websocket.el server fails to handle the mcp protocol.

Returns the port number on success, nil on failure."
  (when (and claude-code-enable-ide-integration
             (featurep 'websocket))
    (condition-case err
        ;; First try the native approach
        (let ((port nil)
              (max-attempts 10)
              (attempt 0))
          ;; Try to find an available port
          (while (and (< attempt max-attempts) (not port))
            (setq attempt (1+ attempt))
            (let ((try-port (claude-code--random-port)))
              (condition-case err
                  (progn
                    (setq claude-code--websocket-server
                          (let ((server (websocket-server
                                        try-port
                                        :host "127.0.0.1"
                                        :on-open #'claude-code--websocket-on-open
                                        :on-message #'claude-code--websocket-on-message
                                        :on-close #'claude-code--websocket-on-close
                                        :on-error #'claude-code--websocket-on-error
                                        :extensions nil)))
                            ;; Set protocols on the server process
                            (when server
                              (process-put server :protocols '("mcp")))
                            server))
                    (when claude-code-websocket-debug
                      (when (boundp 'websocket-debug)
                        (setq websocket-debug t))
                      (when (boundp 'websocket-callback-debug-on-error)
                        (setq websocket-callback-debug-on-error t)))
                    (setq port try-port)
                    (setq claude-code--websocket-port port)
                    (message "WebSocket server created on port %d" port))
                (error
                 ;; Port might be in use, try another
                 (message "Failed to bind to port %d: %s" try-port (error-message-string err))))))
          
          (if port
              (progn
                (message "Claude Code WebSocket server started on port %d" port)
                ;; Write the lock file
                (claude-code--write-lock-file port)
                ;; Add cleanup hook
                (add-hook 'kill-emacs-hook #'claude-code--cleanup-ide-integration)
                port)
            ;; If native websocket failed, try Python bridge
            (message "Native WebSocket server failed, trying Python bridge...")
            (claude-code--start-python-server)))
      
      (error
       ;; On any error, try Python bridge as fallback
       (message "Error starting native WebSocket: %s" (error-message-string err))
       (message "Falling back to Python IDE server...")
       (claude-code--start-python-server)))))

;; Update the stop function to handle both servers
(defun claude-code--stop-websocket-server ()
  "Stop the WebSocket server and clean up."
  ;; Stop native server if running
  (when (and claude-code--websocket-server
             (featurep 'websocket))
    ;; Close all client connections
    (dolist (client claude-code--websocket-clients)
      (ignore-errors (websocket-close client)))
    (setq claude-code--websocket-clients nil)
    ;; Stop the server
    (ignore-errors (websocket-server-close claude-code--websocket-server))
    (setq claude-code--websocket-server nil))
  
  ;; Stop Python server if running
  (claude-code--stop-python-server)
  
  ;; Clean up common state
  (setq claude-code--websocket-port nil)
  (when claude-code--lock-file-path
    (ignore-errors (delete-file claude-code--lock-file-path))
    (setq claude-code--lock-file-path nil))
  (message "IDE integration stopped"))

;; Add interactive commands
(defun claude-code-show-python-server-log ()
  "Show the Python server output buffer."
  (interactive)
  (display-buffer claude-code--python-server-buffer))

(defun claude-code-restart-ide-server ()
  "Restart the IDE server (native or Python)."
  (interactive)
  (claude-code--stop-websocket-server)
  (let ((port (claude-code--start-websocket-server)))
    (if port
        (message "IDE server restarted on port %d" port)
      (message "Failed to restart IDE server"))))

;;; claude-code-ide-patch.el ends here