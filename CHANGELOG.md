# Changelog

All notable changes to claude-code.el will be documented in this file.

## [0.3.5]

### Fixed

- Potential fix for issue #29: check if eat process is still running before adjusting claude buffer window size

## [0.3.4]

### Fixed

- Do not move to end of buffer when in eat-emacs-mode (read-only mode)
- Always send absolute files paths as context to Claude

## [0.3.3]

### Fixed 

- Fixed `claude-code-send-command-with-context` and `claude-code-fix-error-at-point` to use full absolute paths for files outside of projects, ensuring commands work correctly with non-project files.

## [0.3.2]

### Fixed

- Further reduce flickering by only telling the Claude process about window resize events when the _width_ of the Claude window has changed. When the width has changed, Claude needs to redraw the prompt input box. But when only the height has changed, Claude does not have to re-create everything. This greatly reduces flickering that can occur when editing while a Claude window is open in Emacs.

## [0.3.1]

### Fixed

- Fixed bug using `claude-code-send-command-with-context` and `claude-code-fix-error-at-point` when invoked outside of a project, where it incorrectly prompted for a project.

## [0.3.0]

### Added

- **New feature**: Launch repository-specific Claude sessions - work on multiple projects simultaneously with separate Claude instances
- **New feature**: Support for multiple named Claude instances per directory (e.g., one for coding, another for tests)
  - Prompts for instance name when creating additional instances in the same directory
  - Buffer names now include instance names: `*claude:/path/to/project:instance-name*`
- Intelligent instance selection: When switching between directories, claude-code.el prompts to select from existing Claude instances or start a new one
- Instance memory: Your Claude instance selections are remembered per directory during the current Emacs session
- Simplified startup behavior: `claude-code` now automatically detects the appropriate directory (project root, current file directory, or default directory)
- Added prefix arg support to `claude-code-switch-to-buffer` - use `C-u` to see all Claude instances across all directories
- Added prefix arg support to `claude-code-kill` - use `C-u` to kill ALL Claude instances across all directories

### Changed

- Improved performance by reducing terminal reflows - Claude windows now only trigger terminal resizing when width changes, not height

- Claude buffer names now use abbreviated file paths for better readability (e.g., `*claude:~/projects/myapp*`)
- Reorganized prefix arguments for `claude-code` command:
  - Single prefix (`C-u`) now switches to buffer after creating (more commonly used)
  - Double prefix (`C-u C-u`) continues previous conversation (unchanged)
  - Triple prefix (`C-u C-u C-u`) prompts for project directory (previously single prefix)

### Removed

- Removed `claude-code-current-directory` command - its functionality is now integrated into the main `claude-code` command
- Removed limitation of only supporting one Claude process at a time

## [0.2.5] - 2025-06-06

### Added
- New `claude-code-fork` command to jump to previous conversations by sending escape-escape to Claude
  - Bound to `C-c c f` in the command map
  - Available in the transient menu

### Fixed
- Disabled unnecessary shell integration features (command history and prompt annotation) to improve performance

### Changed

## [0.2.4] - 2025-06-05

### Changed
-  `claude-code-kill` now shows a message instead of throwing an error when Claude is not running

## [0.2.3] - 2025-05-23

### Fixed
- Fixed Claude buffer jumping to top when editing in other windows (#8)

## [0.2.2] - 2025-05-22

### Added
- Support for continuing previous conversations with double prefix arg (`C-u C-u`) in `claude-code` and `claude-code-current-directory` commands
    - Uses Claude's `--continue` flag to resume previous sessions
- Read-only mode for text selection in Claude terminal. Toggle with `claude-code-toggle-read-only-mode`.
- Customizable cursor appearance in read-only mode via `claude-code-read-only-mode-cursor-type`

## [0.2.1] - 2025-05-01

### Added
- Extended `claude-code-fix-error-at-point` to support flymake and any system implementing help-at-pt

### Changed
- Fixed compiler warnings (thanks to [ncaq](https://github.com/ncaq))

## [0.2.0] - 2025-04-22

### Added
- New `claude-code-fix-error-at-point` function to help fix flycheck errors. A flymake version will come later.
- Adding option to prompt for extra input in `claude-code-send-region`.
- Confirm before sending large regions or buffers in `claude-code-send-region`. The customization variable `claude-code-large-buffer-threshold` determines what is "large". 
- Added gitleaks pre-commit hook for security

### Changed
- Renamed functions to follow elisp naming conventions:
  - `claude-fix-error-at-point` → `claude-code-fix-error-at-point`
  - `claude--format-flycheck-errors-at-point` → `claude-code--format-flycheck-errors-at-point`
- Removed prefix key customization in favor of manual key binding, fixes #2. 
- Updated Makefile to disable sentence-end-double-space
- Enhanced documentation for flycheck integration and build process


