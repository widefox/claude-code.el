# Claude Code IDE Integration Guide

## Overview

This guide explains how to use the WebSocket IDE integration between Emacs and Claude. With this integration, Claude can see your cursor position, selected text, and the files you're working on in real-time.

## Current Status ✅

The WebSocket IDE integration is successfully working with:
- Python bridge server implementing the MCP protocol
- Real-time selection tracking
- Stable connection that doesn't disconnect
- Automatic fallback when native websocket.el fails

## How to Start the Integration

### 1. From Emacs

```elisp
;; Load the Python bridge
M-x load-file RET
/Users/stephenmolitor/repos/claude-code.el/claude-code-python-bridge.el RET

;; Start the server
M-x claude-code-start-python-ide-server
```

You'll see output like:
```
Python IDE server started on port 10001!
To connect Claude:
  export CLAUDE_CODE_SSE_PORT=10001
  export ENABLE_IDE_INTEGRATION=true
  claude
```

### 2. Connect Claude

In a terminal:
```bash
export CLAUDE_CODE_SSE_PORT=10001  # Use the port shown above
export ENABLE_IDE_INTEGRATION=true
claude
```

In Claude, run:
```
/ide
```

You should see "IDE connected" in green.

## Features Available

### 1. Real-Time Selection Tracking

Claude can see:
- Your current cursor position
- Selected text regions
- The file you're editing
- Line and column numbers

Example messages in `*Messages*` buffer:
```
WS response: {"jsonrpc": "2.0", "method": "selection_changed", "params": {...}}
Sent selection update for claude-code.el
```

### 2. Context-Aware Assistance

You can now ask Claude questions like:
- "What function am I looking at?"
- "Can you explain this code?"
- "How can I improve this function?"
- "What does this variable do?"

### 3. Code Navigation

Claude can use the `openFile` tool to help you navigate to specific files.

## Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x claude-code-start-python-ide-server` | Start the Python IDE server |
| `M-x claude-code-stop-python-ide-server` | Stop the server |
| `M-x claude-code-show-python-server-log` | View server output/debug info |
| `M-x claude-code-test-selection-tracking` | Test selection sending |

## Next Steps

### 1. Integrate into Main Package

To make this permanent, add the Python bridge code to `claude-code.el`:

```elisp
;; Add the code from claude-code-ide-patch.el to claude-code.el
;; This will give you automatic fallback to Python when needed
```

### 2. Automate Startup

Add to your Emacs config:
```elisp
(with-eval-after-load 'claude-code
  ;; Automatically start IDE server when Claude buffer is created
  (add-hook 'claude-code-mode-hook
            (lambda ()
              (when claude-code-enable-ide-integration
                (claude-code--start-websocket-server)))))
```

### 3. Enhanced Features to Implement

- **File watching**: Notify Claude when files are saved
- **Project context**: Send project structure to Claude
- **Diagnostic integration**: Share compiler errors/warnings
- **Symbol information**: Jump to definition support

### 4. Create Shell Wrapper

Create a wrapper script to always start Claude with IDE support:
```bash
#!/bin/bash
# ~/bin/claude-ide
PORT=$(emacsclient --eval '(claude-code-start-python-ide-server)' 2>/dev/null | grep -o '[0-9]\+')
if [ -n "$PORT" ]; then
    export CLAUDE_CODE_SSE_PORT=$PORT
    export ENABLE_IDE_INTEGRATION=true
    claude "$@"
else
    echo "Failed to start IDE server"
    exit 1
fi
```

### 5. Package Distribution

To share this with others:
1. Include Python scripts in the package
2. Add dependency checking for Python/websockets
3. Create automated installer for Python dependencies
4. Submit PR to main claude-code.el repository

## Troubleshooting

### Connection Issues

1. **Check server is running:**
   ```bash
   ps aux | grep claude-ide-server
   ```

2. **Check lock file:**
   ```bash
   ls ~/.claude/ide/*.lock
   ```

3. **View server logs:**
   ```
   M-x claude-code-show-python-server-log
   ```

### Debug Mode

Enable debug output:
```elisp
(setq claude-code-websocket-debug t)
```

### Common Issues

| Issue | Solution |
|-------|----------|
| "IDE disconnected" immediately | Check both env variables are exported |
| Server won't start | Kill existing servers: `pkill -f claude-ide-server` |
| No selection updates | Check `*Messages*` buffer for errors |
| Port conflicts | Server auto-selects free port 10000-65535 |

## Architecture Notes

The integration works by:
1. Python server implements MCP protocol with 'mcp' subprotocol
2. Creates discovery lock file in `~/.claude/ide/`
3. Sends JSON-RPC messages over WebSocket
4. Tracks selection via post-command-hook in Emacs
5. Debounces updates to avoid overwhelming the connection

## Contributing

To improve the integration:
1. Python server: `claude-ide-server-with-selection.py`
2. Emacs bridge: `claude-code-python-bridge.el`
3. Test protocol: `test-claude-protocol.py`

Pull requests welcome at: https://github.com/stevemolitor/claude-code.el