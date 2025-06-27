#!/usr/bin/env python3
"""
Kultivate AI MCP Server
Standalone backend API server for data analysis and business intelligence
"""

import sys
import os
import subprocess
import signal
import time

def start_mcp_server():
    """Start the MCP server"""
    print("🚀 Starting Kultivate AI MCP Server...")
    print("📊 Business Intelligence API ready for external connections")
    
    # Change to backend directory and start the server
    backend_path = os.path.join(os.path.dirname(__file__), 'backend')
    os.chdir(backend_path)
    
    # Start the Python backend directly
    try:
        subprocess.run([sys.executable, 'main_2.py'], check=True)
    except KeyboardInterrupt:
        print("\n🛑 MCP Server stopped by user")
    except Exception as e:
        print(f"❌ Error starting MCP server: {e}")
        sys.exit(1)

if __name__ == "__main__":
    start_mcp_server()