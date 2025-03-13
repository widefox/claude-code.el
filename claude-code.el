;;; claude-code.el --- Claude Code Emacs integration -*- lexical-binding: t; -*-

;; Author: Stephen Molitor <stevemolitor@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (transient "0.7.5") (eat "0.9.2"))
;; Keywords: tools, ai
;; URL: https://github.com/stevemolitor/claude-code.el

;;; Commentary:

;; An Emacs interface to Claude Code. This package provides convenient
;; ways to interact with Claude from within Emacs, including sending
;; commands, toggling the Claude window, and accessing slash commands.

;;; Code:

(require 'transient)

;;;; Customization options
(defgroup claude-code nil
  "Claude AI interface for Emacs."
  :group 'tools)

;;;###autoload (autoload 'claude-code-prefix-key "claude-code")
(defcustom claude-code-prefix-key "C-c c"
  "Prefix key for Claude commands."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-start-hook nil
  "Hook run after Claude is started."
  :type 'hook
  :group 'claude-code)

(defcustom claude-code-startup-delay 0.035
  "Delay in seconds before redisplaying terminal after startup.

This allows the Claude Code terminal display to settle and prevents
layout issues in the EAT window. The appropriate value may vary
depending on the speed of your machine."
  :type 'float
  :group 'claude-code)

(defcustom claude-code-min-latency 0.035
  "Minimum latency in seconds for terminal updates in Claude sessions.

This helps reduce flickering in the EAT window by controlling the minimum
time between screen updates. The appropriate value may vary depending on
the speed of your machine."
  :type 'float
  :group 'claude-code)

(defcustom claude-code-max-latency 0.05
  "Maximum latency in seconds for terminal updates in Claude sessions.

This helps reduce flickering in the EAT window by controlling the maximum
time between screen updates. The appropriate value may vary depending on
the speed of your machine."
  :type 'float
  :group 'claude-code)

;;;; Key bindings
;;;###autoload (autoload 'claude-code-command-map "claude-code")
(defvar claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'claude-code)
    (define-key map "d" 'claude-code-current-directory)
    (define-key map "t" 'claude-code-toggle)
    (define-key map "k" 'claude-code-kill)
    (define-key map "s" 'claude-code-send-command)
    (define-key map "x" 'claude-code-send-command-with-context)
    (define-key map "/" 'claude-code-slash-commands)
    (define-key map "m" 'claude-code-transient)
    map)
  "Keymap for Claude commands.")

;;;; Transient Menus
(transient-define-prefix claude-code-transient ()
  "Claude command menu."
  ["Claude Commands"
   ["Manage Claude" ("c" "Start Claude" claude-code)
    ("d" "Start Claude in current directory" claude-code-current-directory)
    ("t" "Toggle claude window" claude-code-toggle)
    ("k" "Kill Claude" claude-code-kill)]
   ["Send Commands to Claude" ("s" "Send command" claude-code-send-command)
    ("x" "Send command with context" claude-code-send-command-with-context)
    ("/" "Slash Commands" claude-code-slash-commands)]])

(transient-define-prefix claude-code-slash-commands ()
  "Claude slash commands menu."
  ["Slash Commands"
   ["Basic Commands"
    ("c" "Clear" (lambda () (interactive) (claude-code--do-send-command "/clear")))
    ("o" "Compact" (lambda () (interactive) (claude-code--do-send-command "/compact")))
    ("f" "Config" (lambda () (interactive) (claude-code--do-send-command "/config")))
    ("t" "Cost" (lambda () (interactive) (claude-code--do-send-command "/cost")))
    ("d" "Doctor" (lambda () (interactive) (claude-code--do-send-command "/doctor")))
    ("x" "Exit" (lambda () (interactive) (claude-code--do-send-command "/exit")))
    ("h" "Help" (lambda () (interactive) (claude-code--do-send-command "/help")))]
   
   ["Special Commands"
    ("i" "Init" (lambda () (interactive) (claude-code--do-send-command "/init")))
    ("p" "PR" (lambda () (interactive) (claude-code--do-send-command "/pr")))
    ("r" "Release" (lambda () (interactive) (claude-code--do-send-command "/release")))
    ("b" "Bug" (lambda () (interactive) (claude-code--do-send-command "/bug")))
    ("v" "Review" (lambda () (interactive) (claude-code--do-send-command "/review")))]
   
   ["Additional Commands"
    ("e" "Terminal" (lambda () (interactive) (claude-code--do-send-command "/terminal")))
    ("m" "Theme" (lambda () (interactive) (claude-code--do-send-command "/theme")))
    ("v" "Vim" (lambda () (interactive) (claude-code--do-send-command "/vim")))
    ("a" "Approved" (lambda () (interactive) (claude-code--do-send-command "/approved")))
    ("l" "Logout" (lambda () (interactive) (claude-code--do-send-command "/logout")))
    ("g" "Login" (lambda () (interactive) (claude-code--do-send-command "/login")))]
   ])

;;;; Private util functions
(defun claude-code--do-send-command (cmd)
  "Send a command CMD to Claude if Claude buffer exists.

After sending the command, move point to the end of the buffer."
  (if-let ((claude-code-buffer (get-buffer "*claude*")))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal (kbd "RET"))
        (display-buffer claude-code-buffer))
    (error "Claude is not running")))

(defun claude-code-start (dir)
  "Start Claude in directory DIR."
  (require 'eat)
  (let ((default-directory dir)
        (current-window (selected-window)))
    (eat-make "claude" "claude")
    (with-current-buffer "*claude*"
      (sleep-for claude-code-startup-delay)
      (eat-term-redisplay eat-terminal)
      (set-face-attribute 'nobreak-space nil :underline nil)
      (set-face-attribute 'eat-term-faint nil :foreground "#999999" :weight 'light)
      (setq
       eat-minimum-latency claude-code-min-latency
       eat-maximum-latency claude-code-max-latency)
      (run-hooks 'claude-code-start-hook))
    (display-buffer (get-buffer "*claude*"))
    (select-window current-window)
    (run-hooks 'claude-code-start-hook)))

;;;; Interactive Commands
(defun claude-code (&optional arg)
  "Start Claude in an eat terminal and enable `claude-code-mode'.

With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let* ((dir (if arg
                  (read-directory-name "Project directory: ")
                (project-root (project-current t)))))
    (claude-code-start dir)))

(defun claude-code-current-directory ()
  "Start Claude in the current directory."
  (interactive)
  (claude-code-start default-directory))

(defun claude-code-toggle ()
  "Show or hide the Claude window.

If the Claude buffer doesn't exist, create it."
  (interactive)
  (let ((claude-code-buffer (get-buffer "*claude*")))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (display-buffer claude-code-buffer))
      (error "Claude is not running"))))

(defun claude-code-kill ()
  "Kill Claude process and close its window."
  (interactive)
  (when-let ((claude-code-buffer (get-buffer "*claude*")))
    (with-current-buffer claude-code-buffer
      (eat-kill-process)
      (kill-buffer claude-code-buffer))))

(defun claude-code-send-command (cmd &optional arg)
  "Read a Claude command from the minibuffer and send it.

With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (claude-code--do-send-command cmd)
  (when arg
    (switch-to-buffer "*claude*")))

(defun claude-code-send-command-with-context (cmd &optional arg)
  "Read a Claude command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let* ((file-name (buffer-file-name))
         (line-info (if (use-region-p)
                        (format "Lines: %d-%d"
                                (line-number-at-pos (region-beginning))
                                (line-number-at-pos (region-end)))
                      (format "Line: %d" (line-number-at-pos))))
         (cmd-with-context (if file-name
                               (format "%s\nContext: File: %s, %s"
                                       cmd
                                       (file-name-nondirectory file-name)
                                       line-info)
                             cmd)))
    (claude-code--do-send-command cmd-with-context)
    (when arg
      (switch-to-buffer "*claude*"))))

;;;; Mode definition
;;;###autoload
(define-minor-mode claude-code-mode
  "Minor mode for interacting with Claude AI CLI.

When enabled, provides keybindings for starting, sending commands to,
and managing Claude sessions."
  :init-value nil
  :lighter " Claude"
  :global t
  :group 'claude-code
  (if claude-code-mode
      (global-set-key (kbd claude-code-prefix-key) claude-code-command-map)
    (global-unset-key (kbd claude-code-prefix-key))))

;;;; Provide the feature
(provide 'claude-code)

;;; claude-code.el ends here
