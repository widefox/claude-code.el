#!/usr/bin/env python3
"""
Complete Claude IDE WebSocket Server with all essential tools
"""

import asyncio
import json
import os
import sys
import signal
import socket
from pathlib import Path
from typing import List, Dict, Any, Optional
import websockets
import logging
import subprocess

# Configure logging
logging.basicConfig(
    level=logging.WARNING,  # Change to WARNING to reduce output
    format='%(asctime)s [%(levelname)s] %(message)s'
)
logger = logging.getLogger(__name__)


class CompleteClaudeIDEServer:
    def __init__(self, workspace_folders: List[str], ide_name: str = "Emacs IDE Server"):
        self.workspace_folders = workspace_folders
        self.ide_name = ide_name
        self.port = None
        self.server = None
        self.lock_file_path = None
        self.clients = set()
        
        # Track current state
        self.current_file = None
        self.current_selection = None
        self.open_files = set()
        
        # Tool definitions with all essential tools
        self.tools = {
            "getCurrentSelection": {
                "description": "Get the current text selection in the editor",
                "inputSchema": {
                    "type": "object",
                    "properties": {}
                }
            },
            "getWorkspaceFolders": {
                "description": "Get a list of workspace folders",
                "inputSchema": {
                    "type": "object",
                    "properties": {}
                }
            },
            "openFile": {
                "description": "Open a file in the editor",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "filePath": {
                            "type": "string",
                            "description": "Path to the file to open"
                        }
                    },
                    "required": ["filePath"]
                }
            },
            "getOpenEditors": {
                "description": "Get a list of all open editors/files",
                "inputSchema": {
                    "type": "object",
                    "properties": {}
                }
            },
            "readFile": {
                "description": "Read the contents of a file",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "filePath": {
                            "type": "string",
                            "description": "Path to the file to read"
                        }
                    },
                    "required": ["filePath"]
                }
            }
        }

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

    async def handle_tools_call(self, request_id: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle tools/call request"""
        tool_name = params.get("name")
        arguments = params.get("arguments", {})
        
        logger.info(f"Tool call: {tool_name} with args: {arguments}")
        
        try:
            if tool_name == "getCurrentSelection":
                if self.current_selection:
                    text = self.current_selection.get("text", "")
                    file_path = self.current_selection.get("filePath", "")
                    start_line = self.current_selection.get("startLine", 0)
                    
                    # Format response with context
                    response_text = f"File: {file_path}\nLine: {start_line + 1}\nSelected text:\n{text}"
                    
                    return {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "content": [{
                                "type": "text",
                                "text": response_text
                            }]
                        }
                    }
                else:
                    return {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "content": [{
                                "type": "text",
                                "text": "No selection available"
                            }]
                        }
                    }
            
            elif tool_name == "getWorkspaceFolders":
                return {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "content": [{
                            "type": "text",
                            "text": json.dumps(self.workspace_folders, indent=2)
                        }]
                    }
                }
            
            elif tool_name == "openFile":
                file_path = arguments.get("filePath", "")
                
                # Normalize path
                if not os.path.isabs(file_path):
                    # Try to find file relative to workspace folders
                    for workspace in self.workspace_folders:
                        full_path = os.path.join(workspace, file_path)
                        if os.path.exists(full_path):
                            file_path = full_path
                            break
                
                if os.path.exists(file_path):
                    # Use emacsclient to open the file
                    try:
                        subprocess.run(["emacsclient", "--no-wait", file_path], 
                                     capture_output=True, text=True)
                        self.open_files.add(file_path)
                        return {
                            "jsonrpc": "2.0",
                            "id": request_id,
                            "result": {
                                "content": [{
                                    "type": "text",
                                    "text": f"Opened file: {file_path}"
                                }]
                            }
                        }
                    except Exception as e:
                        return {
                            "jsonrpc": "2.0",
                            "id": request_id,
                            "result": {
                                "content": [{
                                    "type": "text",
                                    "text": f"Error opening file: {str(e)}"
                                }]
                            }
                        }
                else:
                    return {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "content": [{
                                "type": "text",
                                "text": f"File not found: {file_path}"
                            }]
                        }
                    }
            
            elif tool_name == "getOpenEditors":
                # Include current file and tracked open files
                open_editors = list(self.open_files)
                if self.current_file and self.current_file not in open_editors:
                    open_editors.append(self.current_file)
                
                return {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "content": [{
                            "type": "text",
                            "text": json.dumps(open_editors, indent=2)
                        }]
                    }
                }
            
            elif tool_name == "readFile":
                file_path = arguments.get("filePath", "")
                
                # Normalize path
                if not os.path.isabs(file_path):
                    for workspace in self.workspace_folders:
                        full_path = os.path.join(workspace, file_path)
                        if os.path.exists(full_path):
                            file_path = full_path
                            break
                
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    
                    return {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "content": [{
                                "type": "text",
                                "text": content
                            }]
                        }
                    }
                except Exception as e:
                    return {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "content": [{
                                "type": "text",
                                "text": f"Error reading file: {str(e)}"
                            }]
                        }
                    }
            
            else:
                return {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "error": {
                        "code": -32601,
                        "message": f"Unknown tool: {tool_name}"
                    }
                }
                
        except Exception as e:
            logger.error(f"Error in tool {tool_name}: {e}", exc_info=True)
            return {
                "jsonrpc": "2.0",
                "id": request_id,
                "error": {
                    "code": -32603,
                    "message": f"Internal error: {str(e)}"
                }
            }

    async def send_selection_changed(self, websocket, 
                                   text: str, file_path: str, 
                                   start_line: int, start_char: int,
                                   end_line: int, end_char: int):
        """Send selection_changed notification"""
        # Update internal state
        self.current_selection = {
            "text": text,
            "filePath": file_path,
            "startLine": start_line,
            "startChar": start_char,
            "endLine": end_line,
            "endChar": end_char
        }
        self.current_file = file_path
        if file_path:
            self.open_files.add(file_path)
        
        # Send notification
        message = {
            "jsonrpc": "2.0",
            "method": "selection_changed",
            "params": {
                "text": text,
                "filePath": file_path,
                "fileUrl": f"file://{file_path}" if file_path else "",
                "selection": {
                    "start": {"line": start_line, "character": start_char},
                    "end": {"line": end_line, "character": end_char},
                    "isEmpty": len(text) == 0
                }
            }
        }
        
        await websocket.send(json.dumps(message))
        logger.info(f"Sent selection_changed for {file_path}")

    async def handle_message(self, websocket, message: str):
        """Handle incoming JSON-RPC message"""
        try:
            data = json.loads(message)
            method = data.get("method")
            request_id = data.get("id")
            params = data.get("params", {})
            
            logger.info(f"Received: {method or 'response'} (id: {request_id})")
            
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
                            "version": "2.0.0"
                        }
                    }
                }
                await websocket.send(json.dumps(response))
                logger.info("Sent initialize response")
                
            elif method == "initialized":
                logger.info("Client initialized successfully")
                
            elif method == "tools/list":
                tools_list = []
                for name, tool in self.tools.items():
                    tools_list.append({
                        "name": name,
                        "description": tool["description"],
                        "inputSchema": tool["inputSchema"]
                    })
                
                response = {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "tools": tools_list
                    }
                }
                await websocket.send(json.dumps(response))
                logger.info(f"Sent tools list with {len(tools_list)} tools")
                
            elif method == "prompts/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": {
                        "prompts": []
                    }
                }
                await websocket.send(json.dumps(response))
                
            elif method == "tools/call":
                response = await self.handle_tools_call(request_id, params)
                await websocket.send(json.dumps(response))
                
            elif method == "updateSelection":
                # Custom method from Emacs
                await self.send_selection_changed(
                    websocket,
                    text=params.get("text", ""),
                    file_path=params.get("filePath", ""),
                    start_line=params.get("startLine", 0),
                    start_char=params.get("startChar", 0),
                    end_line=params.get("endLine", 0),
                    end_char=params.get("endChar", 0)
                )
                # Send acknowledgment
                if request_id:
                    response = {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {"status": "ok"}
                    }
                    await websocket.send(json.dumps(response))
                    
            else:
                if request_id:  # Only respond to requests, not notifications
                    logger.warning(f"Unknown method: {method}")
                    response = {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "error": {
                            "code": -32601,
                            "message": f"Method not found: {method}"
                        }
                    }
                    await websocket.send(json.dumps(response))
                
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse JSON: {e}")
        except Exception as e:
            logger.error(f"Error handling message: {e}", exc_info=True)

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
        print(f"Complete Claude IDE Server started!")
        print(f"{'='*60}")
        print(f"Port: {self.port}")
        print(f"Lock file: {self.lock_file_path}")
        print(f"\nAvailable tools:")
        for tool_name in self.tools.keys():
            print(f"  - {tool_name}")
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
    
    server = CompleteClaudeIDEServer([workspace])
    
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
    asyncio.run(main()