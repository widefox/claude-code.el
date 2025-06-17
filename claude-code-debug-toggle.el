;;; claude-code-debug-toggle.el --- Toggle debug mode for Claude IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience functions to toggle debug logging

;;; Code:

(defvar claude-code-python-debug-level "WARNING"
  "Python logging level (INFO, WARNING, ERROR).")

(defun claude-code-toggle-debug ()
  "Toggle debug mode for Claude IDE integration."
  (interactive)
  (setq claude-code-websocket-debug (not claude-code-websocket-debug))
  (message "Claude WebSocket debug: %s" 
           (if claude-code-websocket-debug "ON" "OFF")))

(defun claude-code-set-python-log-level (level)
  "Set Python server log level to LEVEL (INFO, WARNING, or ERROR)."
  (interactive
   (list (completing-read "Log level: " '("INFO" "WARNING" "ERROR") nil t)))
  (setq claude-code-python-debug-level level)
  (message "Python log level set to: %s (restart server to apply)" level))

(defun claude-code-quiet-mode ()
  "Turn off all debug logging."
  (interactive)
  (setq claude-code-websocket-debug nil)
  (setq claude-code-python-debug-level "WARNING")
  (message "Debug logging disabled (restart server for Python changes)"))

(defun claude-code-verbose-mode ()
  "Turn on all debug logging."
  (interactive)
  (setq claude-code-websocket-debug t)
  (setq claude-code-python-debug-level "INFO")
  (message "Debug logging enabled (restart server for Python changes)"))

(provide 'claude-code-debug-toggle)

;;; claude-code-debug-toggle.el ends here