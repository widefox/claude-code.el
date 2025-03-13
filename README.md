# claude-code.el

An Emacs interface for [Claude Code CLI](https://github.com/anthropics/claude-code), providing integration between Emacs and Claude AI for coding assistance.

## Features

- Start, stop, and toggle Claude Code sessions directly from Emacs
- Send commands to Claude with or without file/line context
- Quick access to all Claude slash commands via transient menus
- Customizable key bindings and appearance settings

## Installation

### Prerequisites

- Emacs 30.0 or higher
- [Claude Code CLI](https://github.com/anthropics/claude-code) installed and configured
- Required Emacs packages: transient (0.7.5+), eat (0.9.2+)

### Using straight.el

```elisp
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :hook ((claude-code--start . sm-setup-claude-faces))
  :config
  (claude-code-mode))
```

## Usage

The default prefix key for all Claude Code commands is `C-c c`. This can be customized with `claude-code-prefix-key`.

### Basic Commands

- `claude-code` (`C-c c c`) - Start Claude in the current project root
- `claude-code-current-directory` (`C-c c d`) - Start Claude in the current directory
- `claude-code-toggle` (`C-c c t`) - Toggle Claude window
- `claude-code-kill` (`C-c c k`) - Kill Claude session
- `claude-code-send-command` (`C-c c s`) - Send command to Claude
- `claude-code-send-command-with-context` (`C-c c x`) - Send command with current file and line context
- `claude-code-slash-commands` (`C-c c /`) - Access Claude slash commands menu
- `claude-code-transient` (`C-c c m`) - Show all commands (transient menu)

With a prefix arg, `claude-code`, `claude-code-current-directory`, `claude-code-send-command` and `claude-code-send-command-with-context` will switch to the Claude terminal buffer after sending the command. 

### Transient Menus

Access all commands through the transient menu with `C-c c m`.

#### Slash Commands Menu

For quick access to Claude slash commands like `/help`, `/clear`, or `/compact`, use `C-c c /` to open the slash commands menu.

## Customization

```elisp
;; Change the prefix key
(setq claude-code-prefix-key "C-c C-a")

;; Add hooks to run after Claude is started
(add-hook 'claude-code-start-hook 'my-claude-setup-function)
```

### Customizing Window Position

You can control how the Claude Code window appears using Emacs' `display-buffer-alist`. For example, to make the Claude window appear on the right side of your screen with 33% width:

```elisp
(add-to-list 'display-buffer-alist
             '("^\\*claude\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.33)))
```

This layout works best on wide screens.

### Font Configuration for Better Rendering

Using a font with good Unicode support helps avoid flickering while Claude Code is rendering its thinking icons. [JuliaMono](https://juliamono.netlify.app/) works particularly well. If you want to use JuliaMono just for Claude Code but use a different font everywhere else you can add this function to `claude-code-start-hook`:

```elisp
(defun sm-setup-claude-faces ()
  (let ((attrs '(:family "JuliaMono" :weight light :height 140)))
    (dolist (face '(eat-shell-prompt-annotation-running
                    eat-shell-prompt-annotation-success
                    eat-shell-prompt-annotation-failure
                    eat-term-bold
                    eat-term-faint
                    eat-term-italic
                    eat-term-slow-blink
                    eat-term-fast-blink))
      (apply 'set-face-attribute face nil attrs))
    (dotimes (i 10)
      (apply 'set-face-attribute (intern (format "eat-term-font-%d" i)) nil attrs))
    (dotimes (i 255)
      (apply 'set-face-attribute (intern (format "eat-term-color-%d" i)) nil attrs))
    (apply 'buffer-face-set attrs))
  (setq line-spacing 0.25))
```

Then enable it in your configuration:

```elisp
(add-hook 'claude-code-start-hook 'sm-setup-claude-faces)
```

## Demo

![Claude Code Emacs Demo](./demo.gif)

This [demo](./demo.gif) shows claude-code.el in action, including toggling the Claude window visibility, accessing the transient menu, and sending commands with file context.

## Limitations

- Currently, `claude-code.el` only supports running one Claude Code process at a time.
- `claude-code.el` only supports using [eat](https://codeberg.org/akib/emacs-eat) for the Claude Code terminal window. Eat provides better rendering with less flickering and visual artifacts compared to other terminal emulators like ansi-term and vterm in testing.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the Apache License 2.0 - see the LICENSE file for details.
