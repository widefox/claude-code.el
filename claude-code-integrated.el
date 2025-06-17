;;; claude-code-integrated.el --- Integrated Python bridge for Claude IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; This integrates the Python WebSocket bridge into claude-code.el
;; It automatically uses the Python bridge when websocket.el can't handle protocols

;;; Code:

(require 'json)

;;;; Python Bridge Variables

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

;;;; Python Bridge Functions

(defun claude-code--find-python-server-script ()
  "Find the Python server script."
  (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
         (scripts '("claude-ide-server-with-selection.py"
                   "claude-ide-server-simple.py")))
    (cl-loop for script in scripts
             for path = (expand-file-name script this-dir)
             when (file-exists-p path)
             return path)))

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
                  
                  ;; Set up selection tracking if available
                  (when (string-match-p "with-selection" claude-code--python-server-script)
                    (claude-code--setup-selection-tracking))
                  
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

;;;; Selection Tracking Functions

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

;;;; Modified start function that tries Python bridge as fallback

(defun claude-code--start-ide-server-with-fallback ()
  "Start IDE server, using Python bridge if websocket.el fails.
Returns the port number on success, nil on failure."
  (when claude-code-enable-ide-integration
    (condition-case err
        ;; First try the native websocket.el approach
        (let ((port (claude-code--start-websocket-server)))
          (if port
              (progn
                (message "Using native WebSocket server on port %d" port)
                port)
            ;; If that fails, try Python bridge
            (message "Native WebSocket failed, trying Python bridge...")
            (claude-code--start-python-server)))
      (error
       ;; On any error, try Python bridge
       (message "Error with native WebSocket: %s" (error-message-string err))
       (message "Falling back to Python bridge...")
       (claude-code--start-python-server)))))

;;;; Override the original function
(defun claude-code--start-websocket-server ()
  "Start the WebSocket server for IDE integration.
This version automatically falls back to Python bridge if needed."
  (claude-code--start-ide-server-with-fallback))

;;;; Interactive commands

(defun claude-code-show-python-server-log ()
  "Show the Python server output buffer."
  (interactive)
  (display-buffer claude-code--python-server-buffer))

(defun claude-code-restart-ide-server ()
  "Restart the IDE server (native or Python)."
  (interactive)
  ;; Stop everything
  (claude-code--stop-websocket-server)
  (claude-code--stop-python-server)
  ;; Start again
  (let ((port (claude-code--start-ide-server-with-fallback)))
    (if port
        (message "IDE server restarted on port %d" port)
      (message "Failed to restart IDE server"))))

(provide 'claude-code-integrated)

;;; claude-code-integrated.el ends here