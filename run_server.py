#!/usr/bin/env python3
"""
Simple startup script for Kultivate AI MCP Server
Runs the Python Flask server directly without npm dependencies
"""

import os
import subprocess
import sys

def main():
    """Start the MCP server directly"""
    print("🚀 Starting Kultivate AI MCP Server...")
    
    # Set environment variables if needed
    os.environ.setdefault('PORT', '8081')
    os.environ.setdefault('FLASK_ENV', 'production')
    
    # Run the MCP server
    try:
        subprocess.run([sys.executable, 'mcp_server.py'], check=True)
    except KeyboardInterrupt:
        print("\n⚡ Server stopped by user")
    except Exception as e:
        print(f"❌ Server error: {e}")
        return 1
    
    return 0

if __name__ == '__main__':
    sys.exit(main())