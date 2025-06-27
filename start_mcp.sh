#!/bin/bash

# Start Kultivate AI MCP Server
echo "🚀 Starting Kultivate AI MCP Server..."

# Kill any existing server processes
pkill -f "python.*mcp_server.py" 2>/dev/null

# Start the MCP server
python mcp_server.py