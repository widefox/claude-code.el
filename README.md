# claude-code.el

An Emacs interface for [Claude Code CLI](https://github.com/anthropics/claude-code), providing integration between Emacs and Claude AI for coding assistance.

## Features

- Start, stop, and toggle Claude Code sessions directly from Emacs
- Support for multiple Claude instances across different projects and directories
- Send commands to Claude with or without file/line context
- Quick access to all Claude slash commands via transient menus
- Customizable key bindings and appearance settings

## Installation

### Prerequisites

- Emacs 30.0 or higher
- [Claude Code CLI](https://github.com/anthropics/claude-code) installed and configured
- Required Emacs packages: transient (0.4.0+), eat (0.9.2+)

### Using builtin use-package (Emacs 30+)

```elisp
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)) ;; or your preferred key
```

### Using straight.el

```elisp
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))
```

## Usage

You need to set your own key binding for the Claude Code command map. The examples in this README use `C-c c` as the prefix key.

### Basic Commands

- `claude-code` (`C-c c c`) - Start Claude 
- `claude-code-toggle` (`C-c c t`) - Toggle Claude window
- `claude-code-switch-to-buffer` (`C-c c b`) - Switch to the Claude buffer
- `claude-code-kill` (`C-c c k`) - Kill Claude session
- `claude-code-send-command` (`C-c c s`) - Send command to Claude
- `claude-code-send-command-with-context` (`C-c c x`) - Send command with current file and line context
- `claude-code-send-region` (`C-c c r`) - Send the current region or buffer to Claude. With prefix arg (`C-u`), prompts for instructions to add to the text. With double prefix (`C-u C-u`), adds instructions and switches to Claude buffer
- `claude-code-fix-error-at-point` (`C-c c e`) - Ask Claude to fix the error at the current point (works with flycheck, flymake, and any system that implements help-at-pt)
- `claude-code-slash-commands` (`C-c c /`) - Access Claude slash commands menu
- `claude-code-transient` (`C-c c m`) - Show all commands (transient menu)
- `claude-code-send-return` (`C-c c y`) - Send return key to Claude (useful for confirming with Claude without switching to the Claude REPL buffer)
- `claude-code-send-escape` (`C-c c n`) - Send escape key to Claude (useful for saying "No" when Claude asks for confirmation without switching to the Claude REPL buffer)
- `claude-code-fork` (`C-c c f`) - Fork conversation (jump to previous conversation by sending escape-escape to Claude)

With a single prefix arg, `claude-code`, `claude-code-send-command` and
`claude-code-send-command-with-context` will switch to the Claude terminal buffer after sending the
command.



### Read-Only Mode Toggle

The `claude-code-toggle-read-only-mode` command provides a convenient way to switch between normal terminal mode and read-only mode in the Claude buffer:

- `claude-code-toggle-read-only-mode` - Toggle between read-only mode and normal mode

In read-only mode, you can interact with the terminal buffer just like a regular Emacs buffer, making it easy to select and copy text. However, you cannot change the buffer contents or enter Claude commands in this mode. This is particularly useful when you need to copy output from Claude without accidentally modifying the terminal.

The command automatically detects the current mode and switches to the other:
- If in normal terminal mode (semi-char mode), it switches to read-only mode
- If in read-only mode (emacs mode), it switches back to normal terminal mode

### Continuing Previous Conversations

The `claude-code` command supports continuing previous conversations using Claude's `--continue`
flag:

- Double prefix arg (`C-u C-u C-c c c`) - Start Claude in project root and continue previous conversation

This allows you to resume where you left off in your previous Claude session.

### Transient Menus

Access all commands through the transient menu with `C-c c m`.

#### Slash Commands Menu

For quick access to Claude slash commands like `/help`, `/clear`, or `/compact`, use `C-c c /` to open the slash commands menu.

## Customization

```elisp
;; Set your key binding for the command map
(global-set-key (kbd "C-c C-a") claude-code-command-map)

;; Set terminal type for the Claude terminal emulation (default is "xterm-256color")
;; This determines terminal capabilities like color support
;; See the documentation for eat-term-name for more information
(setq claude-code-term-name "xterm-256color")

;; Change the path to the Claude executable (default is "claude")
;; Useful if Claude is not in your PATH or you want to use a specific version
(setq claude-code-program "/usr/local/bin/claude")

;; Set command line arguments for Claude
;; For example, to enable verbose output
(setq claude-code-program-switches '("--verbose"))

;; Add hooks to run after Claude is started
(add-hook 'claude-code-start-hook 'my-claude-setup-function)

;; Adjust initialization delay (default is 0.1 seconds)
;; This helps prevent terminal layout issues if the buffer is displayed before Claude is fully ready
(setq claude-code-startup-delay 0.2)

;; Configure the buffer size threshold for confirmation prompt (default is 1000 characters)
;; If a buffer is larger than this threshold, claude-code-send-region will ask for confirmation
;; before sending the entire buffer to Claude
(setq claude-code-large-buffer-threshold 1000)
```

### Customizing Window Position

You can control how the Claude Code window appears using Emacs' `display-buffer-alist`. For example, to make the Claude window appear on the right side of your screen with 33% width:

```elisp
(add-to-list 'display-buffer-alist
             '("^\\*claude"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.33)))
```

This layout works best on wide screens.

### Font Configuration for Better Rendering

Using a font with good Unicode support helps avoid flickering while Claude Code is rendering its thinking icons. [JuliaMono](https://juliamono.netlify.app/) has excellent Unicode symbols support. To let the Claude Code buffer use Julia Mono for rendering Unicode characters while still using your default font for ASCII characters add this elisp code:

```elisp
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'unicode (font-spec :family "JuliaMono"))

;; for emoji characters on MacOS
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)
```

If instead you want to use a particular font just for the Claude Code REPL but use a different font
everywhere else you can customize the `claude-code-repl-face`:

```elisp
(custom-set-faces
   '(claude-code-repl-face ((t (:family "JuliaMono")))))
```

### Reducing Flickering on Window Configuration Changes

To reduce flickering in the Claude buffer on window configuration changes, you can adjust eat latency variables in a hook. This reduces flickering at the cost of some increased latency:

```elisp
  ;; reduce flickering
  (add-hook 'claude-code-start-hook
            (lambda ()
              (setq-local eat-minimum-latency 0.08
                          eat-maximum-latency 0.2)))
```

## Demo

### GIF Demo

![Claude Code Emacs Demo](./demo.gif)

This [demo](./demo.gif) shows claude-code.el in action, including toggling the Claude window visibility, accessing the transient menu, and sending commands with file context.

### Video Demo

[![The Emacs Claude Code Package](https://img.youtube.com/vi/K8sCVLmFyyU/0.jpg)](https://www.youtube.com/watch?v=K8sCVLmFyyU)

Check out this [video demo](https://www.youtube.com/watch?v=K8sCVLmFyyU) demonstrating the claude-code.el package. This video was kindly created and shared by a user of the package.

### Read-Only Mode for Text Selection

In the Claude terminal, you can switch to a read-only mode to select and copy text:

- `C-c C-e` (`eat-emacs-mode`) - Switch to read-only mode with normal Emacs cursor for text selection
- `C-c C-j` (`semi-char-mode`) - Return to normal terminal mode

The cursor appearance in read-only mode can be customized via the `claude-code-read-only-mode-cursor-type` variable:

```elisp
;; Customize cursor type in read-only mode (default is 'box)
;; Options: 'box, 'hollow, 'bar, 'hbar, or nil
(setq claude-code-read-only-mode-cursor-type 'bar)
```

## Multiple Claude Instances

`claude-code.el` supports running multiple Claude instances across different projects and directories. Each Claude instance is associated with a specific directory (project root, file directory, or current directory).

### Instance Management

- When you start Claude with `claude-code`, it creates an instance for the current directory
- Buffer names follow the format `*claude:/path/to/directory*` to clearly identify each instance
- If you're in a directory without a Claude instance but have instances running in other directories, you'll be prompted to select one
- Your selection is remembered for that directory, so you won't be prompted again
- To start a new instance instead of selecting an existing one, cancel the prompt with `C-g`

This allows you to have separate Claude conversations for different projects while easily switching between them or sharing a Claude instance across related directories.

## Limitations

- `claude-code.el` only supports using [eat](https://codeberg.org/akib/emacs-eat) for the Claude Code terminal window. Eat provides better rendering with less flickering and visual artifacts compared to other terminal emulators like ansi-term and vterm in testing.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.
