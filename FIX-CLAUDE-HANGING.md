# Fix for Claude Hanging on IDE Commands

## Problem

When Claude tries to use IDE features like:
- "Tell me what the current line of code does"
- "Open file X"
- "What am I looking at?"

Claude hangs/runs forever because it's trying to use tools that aren't implemented in the basic server.

## Solution

Use the complete IDE server (`claude-ide-server-complete.py`) which implements all essential tools:

### 1. Stop Current Server

```
M-x claude-code-stop-python-ide-server
```

### 2. Start Complete Server

The updated `claude-code-python-bridge.el` now uses the complete server automatically:

```
M-x claude-code-start-python-ide-server
```

### 3. Reconnect Claude

```bash
# Use the new port shown in Emacs
export CLAUDE_CODE_SSE_PORT=10002  
export ENABLE_IDE_INTEGRATION=true
claude
```

Run `/ide` in Claude again.

## What's Different

The complete server implements these tools that Claude needs:

| Tool | What it does | Example Claude Usage |
|------|--------------|---------------------|
| `getCurrentSelection` | Returns selected text with context | "What does this code do?" |
| `openFile` | Opens files in Emacs via emacsclient | "Open main.py" |
| `readFile` | Reads file contents | "Show me the config file" |
| `getOpenEditors` | Lists open files | "What files are open?" |
| `getWorkspaceFolders` | Shows project roots | "What project am I in?" |

## Testing the Fix

1. **Test selection reading:**
   - Select some code in Emacs
   - Ask Claude: "What code am I looking at?"
   - Claude should respond quickly with the actual code

2. **Test file opening:**
   - Ask Claude: "Open claude-code.el"
   - The file should open in Emacs
   - Claude should confirm it opened

3. **Test file reading:**
   - Ask Claude: "What's in the README.md file?"
   - Claude should read and summarize the contents

## If Claude Still Hangs

1. Check the server log:
   ```
   M-x claude-code-show-python-server-log
   ```
   Look for any error messages when Claude makes requests.

2. Restart everything:
   ```
   M-x claude-code-stop-python-ide-server
   M-x claude-code-start-python-ide-server
   ```
   Then reconnect Claude with the new port.

3. Enable debug mode to see what tools Claude is trying to use:
   ```elisp
   (setq claude-code-websocket-debug t)
   ```

## Technical Details

The hanging occurs because:
1. Claude sends a `tools/call` request
2. The basic server doesn't implement that tool
3. Claude waits forever for a response that never comes

The complete server:
- Implements all tools Claude commonly uses
- Returns proper JSON-RPC responses for each tool
- Handles errors gracefully with error responses
- Uses `emacsclient` to actually open files in Emacs

## Next Steps

If you need additional tools, add them to the `self.tools` dictionary in `claude-ide-server-complete.py`. Each tool needs:
- A description
- An inputSchema
- A handler in the `handle_tools_call` method