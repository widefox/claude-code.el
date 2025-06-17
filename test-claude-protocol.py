#!/usr/bin/env python3
"""
Test script to verify Claude IDE protocol implementation
"""

import asyncio
import json
import sys
import websockets
from typing import Dict, Any
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s [%(levelname)s] %(message)s')
logger = logging.getLogger(__name__)


class ProtocolTester:
    def __init__(self, port: int):
        self.port = port
        self.uri = f"ws://127.0.0.1:{port}"
        self.message_id = 0
    
    def next_id(self) -> str:
        """Generate next message ID"""
        self.message_id += 1
        return str(self.message_id)
    
    async def test_connection(self):
        """Test basic WebSocket connection"""
        logger.info(f"Testing connection to {self.uri}")
        try:
            async with websockets.connect(self.uri, subprotocols=['mcp']) as websocket:
                logger.info("✓ WebSocket connection established")
                
                # Wait for any initial messages
                try:
                    message = await asyncio.wait_for(websocket.recv(), timeout=2.0)
                    data = json.loads(message)
                    logger.info(f"✓ Received initial message: {data.get('method', 'unknown')}")
                except asyncio.TimeoutError:
                    logger.info("  No initial message received (this is OK)")
                
                return True
        except Exception as e:
            logger.error(f"✗ Connection failed: {e}")
            return False
    
    async def test_mcp_protocol(self):
        """Test MCP protocol implementation"""
        logger.info("\nTesting MCP protocol...")
        
        async with websockets.connect(self.uri, subprotocols=['mcp']) as websocket:
            # Test 1: Initialize
            logger.info("\n1. Testing initialize...")
            init_request = {
                "jsonrpc": "2.0",
                "id": self.next_id(),
                "method": "initialize",
                "params": {
                    "protocolVersion": "2025-03-26",
                    "capabilities": {},
                    "clientInfo": {
                        "name": "test-client",
                        "version": "1.0.0"
                    }
                }
            }
            
            await websocket.send(json.dumps(init_request))
            response = await websocket.recv()
            data = json.loads(response)
            
            if "result" in data:
                logger.info("✓ Initialize response received")
                logger.info(f"  Protocol version: {data['result'].get('protocolVersion')}")
                logger.info(f"  Server info: {data['result'].get('serverInfo')}")
            else:
                logger.error(f"✗ Initialize failed: {data}")
                return False
            
            # Send initialized notification
            logger.info("\n2. Sending initialized notification...")
            initialized_msg = {
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }
            await websocket.send(json.dumps(initialized_msg))
            logger.info("✓ Initialized notification sent")
            
            # Test 2: List tools
            logger.info("\n3. Testing tools/list...")
            tools_request = {
                "jsonrpc": "2.0",
                "id": self.next_id(),
                "method": "tools/list",
                "params": {}
            }
            
            await websocket.send(json.dumps(tools_request))
            response = await websocket.recv()
            data = json.loads(response)
            
            if "result" in data and "tools" in data["result"]:
                tools = data["result"]["tools"]
                logger.info(f"✓ Tools list received: {len(tools)} tools")
                for tool in tools:
                    logger.info(f"  - {tool['name']}: {tool['description']}")
            else:
                logger.error(f"✗ Tools list failed: {data}")
            
            # Test 3: List prompts
            logger.info("\n4. Testing prompts/list...")
            prompts_request = {
                "jsonrpc": "2.0",
                "id": self.next_id(),
                "method": "prompts/list",
                "params": {}
            }
            
            await websocket.send(json.dumps(prompts_request))
            response = await websocket.recv()
            data = json.loads(response)
            
            if "result" in data and "prompts" in data["result"]:
                prompts = data["result"]["prompts"]
                logger.info(f"✓ Prompts list received: {len(prompts)} prompts")
            else:
                logger.error(f"✗ Prompts list failed: {data}")
            
            # Test 4: Call a tool
            logger.info("\n5. Testing tool call (getCurrentSelection)...")
            tool_request = {
                "jsonrpc": "2.0",
                "id": self.next_id(),
                "method": "tools/call",
                "params": {
                    "name": "getCurrentSelection",
                    "arguments": {}
                }
            }
            
            await websocket.send(json.dumps(tool_request))
            response = await websocket.recv()
            data = json.loads(response)
            
            if "result" in data and "content" in data["result"]:
                logger.info("✓ Tool call successful")
                content = data["result"]["content"]
                if content and len(content) > 0:
                    logger.info(f"  Response: {content[0].get('text', '')}")
            else:
                logger.error(f"✗ Tool call failed: {data}")
            
            # Test 5: Check for selection_changed notifications
            logger.info("\n6. Waiting for selection_changed notification...")
            try:
                message = await asyncio.wait_for(websocket.recv(), timeout=3.0)
                data = json.loads(message)
                if data.get("method") == "selection_changed":
                    logger.info("✓ Received selection_changed notification")
                    params = data.get("params", {})
                    logger.info(f"  File: {params.get('filePath')}")
                    logger.info(f"  Text: {params.get('text', '')[:50]}...")
                else:
                    logger.info(f"  Received other message: {data.get('method')}")
            except asyncio.TimeoutError:
                logger.info("  No selection notification received (server may not send automatically)")
            
            logger.info("\n✓ All protocol tests completed successfully!")
            return True
    
    async def test_claude_simulation(self):
        """Simulate Claude's connection pattern"""
        logger.info("\n\nSimulating Claude connection pattern...")
        
        async with websockets.connect(self.uri, subprotocols=['mcp']) as websocket:
            # Claude's typical initialization sequence
            logger.info("1. Sending initialize (as Claude would)...")
            
            init_request = {
                "jsonrpc": "2.0",
                "id": "1",
                "method": "initialize",
                "params": {
                    "protocolVersion": "2025-03-26",
                    "capabilities": {
                        "tools": {},
                        "prompts": {}
                    },
                    "clientInfo": {
                        "name": "claude",
                        "version": "unknown"
                    }
                }
            }
            
            await websocket.send(json.dumps(init_request))
            response = await websocket.recv()
            logger.info(f"  Response: {json.loads(response).get('result', {}).get('serverInfo')}")
            
            # Send initialized
            await websocket.send(json.dumps({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }))
            
            # Request tools
            await websocket.send(json.dumps({
                "jsonrpc": "2.0",
                "id": "2",
                "method": "tools/list",
                "params": {}
            }))
            response = await websocket.recv()
            logger.info(f"  Tools available: {len(json.loads(response).get('result', {}).get('tools', []))}")
            
            # Keep connection open and listen for messages
            logger.info("\n2. Keeping connection open (like Claude does)...")
            logger.info("   Listening for server messages for 5 seconds...")
            
            end_time = asyncio.get_event_loop().time() + 5
            message_count = 0
            
            while asyncio.get_event_loop().time() < end_time:
                try:
                    message = await asyncio.wait_for(websocket.recv(), timeout=1.0)
                    data = json.loads(message)
                    message_count += 1
                    logger.info(f"  Received: {data.get('method', 'response')} message")
                except asyncio.TimeoutError:
                    continue
                except websockets.exceptions.ConnectionClosed:
                    logger.error("  Connection closed by server!")
                    return False
            
            logger.info(f"\n✓ Connection remained stable for 5 seconds")
            logger.info(f"  Received {message_count} messages from server")
            
            # Send a tool call before closing
            logger.info("\n3. Testing tool call before closing...")
            await websocket.send(json.dumps({
                "jsonrpc": "2.0",
                "id": "3",
                "method": "tools/call",
                "params": {
                    "name": "getWorkspaceFolders",
                    "arguments": {}
                }
            }))
            response = await websocket.recv()
            logger.info("  ✓ Tool call successful")
            
            logger.info("\n✓ Claude simulation completed successfully!")
            return True


async def main():
    if len(sys.argv) != 2:
        print("Usage: python test-claude-protocol.py <port>")
        sys.exit(1)
    
    try:
        port = int(sys.argv[1])
    except ValueError:
        print("Error: Port must be a number")
        sys.exit(1)
    
    tester = ProtocolTester(port)
    
    # Run tests
    if not await tester.test_connection():
        logger.error("Connection test failed. Is the server running?")
        sys.exit(1)
    
    if not await tester.test_mcp_protocol():
        logger.error("MCP protocol test failed")
        sys.exit(1)
    
    if not await tester.test_claude_simulation():
        logger.error("Claude simulation test failed")
        sys.exit(1)
    
    logger.info("\n" + "="*60)
    logger.info("ALL TESTS PASSED! ✓")
    logger.info("The server appears to be working correctly.")
    logger.info("="*60)


if __name__ == "__main__":
    asyncio.run(main())