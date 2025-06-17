;;; claude-code-python-bridge.el --- Use Python server for Claude IDE integration -*- lexical-binding: t; -*-

;;; Commentary:
;; This uses the working Python WebSocket server as a bridge for Claude integration

;;; Code:

(defvar claude-code-python-server-process nil
  "The Python IDE server process.")

(defvar claude-code-python-server-port nil
  "The port the Python server is running on.")

(defvar claude-code-python-http-port nil
  "The HTTP port for sending updates to Python server.")

(defvar claude-code-python-selection-timer nil
  "Timer for debouncing selection updates.")

(defvar claude-code-python-last-selection nil
  "Last selection sent to avoid duplicates.")

(defvar claude-code-python-server-buffer "*claude-python-server*"
  "Buffer for Python server output.")

;;;###autoload
(defun claude-code-start-python-ide-server ()
  "Start the Python IDE server for Claude integration."
  (interactive)
  
  ;; Stop any existing server
  (claude-code-stop-python-ide-server)
  
  ;; Find the Python server script
  (let* ((script-dir "/Users/stephenmolitor/repos/claude-code.el/")
         (server-script (expand-file-name "claude-ide-server-complete.py" script-dir))
         (workspace (or (when (project-current)
                         (project-root (project-current)))
                       default-directory)))
    
    (unless (file-executable-p server-script)
      (error "Python IDE server script not found or not executable: %s" server-script))
    
    ;; Start the server
    (let ((process-connection-type nil)) ; Use pipes
      (setq claude-code-python-server-process
            (start-process "claude-python-server"
                          claude-code-python-server-buffer
                          "bash" "-c"
                          (format "cd %s && source .venv/bin/activate && python %s %s"
                                 script-dir server-script workspace))))
    
    (if claude-code-python-server-process
        (progn
          ;; Set up process sentinel
          (set-process-sentinel 
           claude-code-python-server-process
           #'claude-code-python-server-sentinel)
          
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
              (with-current-buffer claude-code-python-server-buffer
                (goto-char (point-min))
                ;; Look for port in server output - try multiple patterns
                (when (or (re-search-forward "server listening on [^:]+:\\([0-9]+\\)" nil t)
                          (re-search-forward "Port: \\([0-9]+\\)" nil t)
                          (re-search-forward "WebSocket Port: \\([0-9]+\\)" nil t))
                  (setq claude-code-python-server-port 
                        (string-to-number (match-string 1)))
                  (setq port-found t))))
            
            (if port-found
                (progn
                  (message "Python IDE server started on port %d!" 
                          claude-code-python-server-port)
                  (message "To connect Claude:")
                  (message "  export CLAUDE_CODE_SSE_PORT=%d" 
                          claude-code-python-server-port)
                  (message "  export ENABLE_IDE_INTEGRATION=true")
                  (message "  claude")
                  
                  ;; For now, no selection tracking with simple server
                  ;; (add-hook 'post-command-hook #'claude-code-python-schedule-selection-update)
                  claude-code-python-server-port)
              
              ;; Show the buffer contents for debugging
              (display-buffer claude-code-python-server-buffer)
              (error "Failed to get server port. Check *claude-python-server* buffer for errors"))))
      (error "Failed to start Python IDE server"))))

;;;###autoload
(defun claude-code-stop-python-ide-server ()
  "Stop the Python IDE server."
  (interactive)
  (when claude-code-python-server-process
    (delete-process claude-code-python-server-process)
    (setq claude-code-python-server-process nil)
    (setq claude-code-python-server-port nil)
    (setq claude-code-python-http-port nil))
  (remove-hook 'post-command-hook #'claude-code-python-schedule-selection-update)
  (when claude-code-python-selection-timer
    (cancel-timer claude-code-python-selection-timer)
    (setq claude-code-python-selection-timer nil))
  (message "Python IDE server stopped"))

(defun claude-code-python-server-sentinel (process event)
  "Handle PROCESS EVENT for Python server."
  (when (string-match "\\(finished\\|exited\\|killed\\)" event)
    (message "Python IDE server %s" event)
    (setq claude-code-python-server-process nil)
    (setq claude-code-python-server-port nil)
    (setq claude-code-python-http-port nil)
    (remove-hook 'post-command-hook #'claude-code-python-schedule-selection-update)))

(defun claude-code-python-schedule-selection-update ()
  "Schedule a selection update with debouncing."
  (when claude-code-python-http-port
    ;; Cancel any pending timer
    (when claude-code-python-selection-timer
      (cancel-timer claude-code-python-selection-timer))
    ;; Schedule new update
    (setq claude-code-python-selection-timer
          (run-with-timer 0.05 nil #'claude-code-python-send-selection))))

(defun claude-code-python-send-selection ()
  "Send current selection to Python server via HTTP."
  (when claude-code-python-http-port
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
      (unless (equal selection-data claude-code-python-last-selection)
        (setq claude-code-python-last-selection selection-data)
        
        ;; Send HTTP POST request
        (let* ((url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "application/json")))
               (url-request-data (json-encode selection-data))
               (url (format "http://127.0.0.1:%d/selection" claude-code-python-http-port)))
          
          (url-retrieve url
                       (lambda (status)
                         (if (plist-get status :error)
                             (message "Failed to send selection update: %s" 
                                     (plist-get status :error))
                           (kill-buffer))) ; Clean up the response buffer
                       nil t nil))))))

;;;###autoload
(defun claude-code-show-python-server-log ()
  "Show the Python server output buffer."
  (interactive)
  (display-buffer claude-code-python-server-buffer))

(provide 'claude-code-python-bridge)

;;; claude-code-python-bridge.el ends here