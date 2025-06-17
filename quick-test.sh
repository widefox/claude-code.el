#!/bin/bash
# Quick test script to verify the solution works

echo "=== Claude Code IDE Integration Test ==="
echo

# Check for virtual environment
if [ ! -d ".venv" ]; then
    echo "Creating Python virtual environment..."
    python3 -m venv .venv
    source .venv/bin/activate
    pip install websockets
else
    source .venv/bin/activate
fi

# Start the server
echo "Starting Python IDE server..."
python claude-ide-server-with-selection.py . &
SERVER_PID=$!

# Wait for server to start
sleep 2

# Get the port
PORT=$(ls ~/.claude/ide/*.lock 2>/dev/null | head -1 | xargs basename | cut -d. -f1)

if [ -z "$PORT" ]; then
    echo "❌ Server failed to start"
    kill $SERVER_PID 2>/dev/null
    exit 1
fi

echo "✅ Server started on port $PORT"
echo
echo "To connect Claude:"
echo "  export CLAUDE_CODE_SSE_PORT=$PORT"
echo "  export ENABLE_IDE_INTEGRATION=true"
echo "  claude"
echo
echo "Then run the /ide command in Claude"
echo
echo "Press Ctrl+C to stop the server"

# Wait for interrupt
trap "kill $SERVER_PID 2>/dev/null; rm -f ~/.claude/ide/$PORT.lock; exit" INT
wait $SERVER_PID