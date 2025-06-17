#!/bin/bash
# Run the simple Python IDE server

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Activate virtual environment
source .venv/bin/activate

# Run the simple server (no aiohttp needed)
echo "Starting Simple Python IDE server..."
python claude-ide-server-simple.py "$@"