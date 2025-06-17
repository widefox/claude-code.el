#!/usr/bin/env python3
"""
Simplified Claude IDE WebSocket Server without HTTP endpoint
"""

import asyncio
import json
import os
import sys
import signal
import socket
from pathlib import Path
from typing import List
import websockets
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s'
)
logger = logging.getLogger(__name__)


class SimpleClaudeIDEServer:
    def __init__(self, workspace_folders: List[str], ide_name: str = "Python IDE Server"):
        self.workspace_folders = workspace_folders
        self.ide_name = ide_name
        self.port = None
        self.server = None
        self.lock_file_path = None
        self.clients = set()

    def find_free_port(self) -> int:
        """Find a free port between 10000 and 65535"""
        for port in range(10000, 65536):
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                try:
                    s.bind(('127.0.0.1', port))
                    return port
                except OSError:
                    continue
        raise RuntimeError("No free ports available")

    def write_lock_file(self):
        """Write the discovery lock file"""
        lock_dir = Path.home() / ".claude" / "ide"
        lock_dir.mkdir(parents=True, exist_ok=True)
        
        self.lock_file_path = lock_dir / f"{self.port}.lock"
        
        lock_data = {
            "pid": os.getpid(),
            "workspaceFolders": self.workspace_folders,
            "ideName": self.ide_name,
            "transport": "ws"
        }
        
        with open(self.lock_file_path, 'w') as f:
            json.dump(lock_data, f, indent=2)
        
        logger.info(f"Created lock file: {self.lock_file_path}")

    def remove_lock_file(self):
        """Remove the lock file on shutdown"""
        if self.lock_file_path and self.lock_file_path.exists():
            self.lock_file_path.unlink()
            logger.info(f"Removed lock file: {self.lock_file_path}")

    async def handle_message(self, websocket, message: str):
        """Handle incoming JSON-RPC message"""
        try:
            data = json.loads(message)
            logger.info(f"Received: {data.get('method', 'unknown')}")
            
            method = data.get("method")
            request_id = data.get("id")
            
            if method == "initialize":
                response = {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "protocolVersion": "2024-11-05",
                        "capabilities": {
                            "tools": {},
                            "prompts": {}
                        },
                        "serverInfo": {
                            "name": self.ide_name,
                            "version": "1.0.0"
                        }
                    }
                }
                await websocket.send(json.dumps(response))
                logger.info("Sent initialize response")
                
            elif method == "initialized":
                logger.info("Client initialized")
                
            elif method == "tools/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "tools": []
                    }
                }
                await websocket.send(json.dumps(response))
                
            elif method == "prompts/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "prompts": []
                    }
                }
                await websocket.send(json.dumps(response))
                
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse JSON: {e}")
        except Exception as e:
            logger.error(f"Error handling message: {e}")

    async def handle_connection(self, websocket):
        """Handle new WebSocket connection"""
        logger.info(f"New connection established")
        self.clients.add(websocket)
        
        try:
            async for message in websocket:
                await self.handle_message(websocket, message)
                
        except websockets.exceptions.ConnectionClosed:
            logger.info("Connection closed")
        except Exception as e:
            logger.error(f"Connection error: {e}")
        finally:
            self.clients.discard(websocket)

    async def start(self):
        """Start the WebSocket server"""
        self.port = self.find_free_port()
        self.write_lock_file()
        
        logger.info(f"Starting WebSocket server on ws://127.0.0.1:{self.port}")
        
        self.server = await websockets.serve(
            self.handle_connection,
            '127.0.0.1',
            self.port,
            compression=None,
            max_size=10 * 1024 * 1024,
            subprotocols=['mcp']
        )
        
        print(f"\n{'='*60}")
        print(f"Simple Claude IDE Server started!")
        print(f"{'='*60}")
        print(f"Port: {self.port}")
        print(f"Lock file: {self.lock_file_path}")
        print(f"\nTo connect Claude:")
        print(f"  export CLAUDE_CODE_SSE_PORT={self.port}")
        print(f"  export ENABLE_IDE_INTEGRATION=true")
        print(f"  claude")
        print(f"\nPress Ctrl+C to stop")
        print(f"{'='*60}\n")
        
        await asyncio.Future()  # Run forever

    def cleanup(self):
        """Clean up resources"""
        self.remove_lock_file()
        if self.server:
            self.server.close()


async def main():
    workspace = sys.argv[1] if len(sys.argv) > 1 else os.getcwd()
    workspace = os.path.abspath(workspace)
    
    server = SimpleClaudeIDEServer([workspace])
    
    def signal_handler(sig, frame):
        logger.info("Shutting down...")
        server.cleanup()
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    try:
        await server.start()
    except Exception as e:
        logger.error(f"Server error: {e}")
        server.cleanup()
        sys.exit(1)


if __name__ == "__main__":
    asyncio.run(main())