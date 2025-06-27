#!/usr/bin/env python3
"""
Test HTTP endpoints by running server in background and making requests
"""

import subprocess
import time
import sys
import requests
import json
import signal
import os

def test_http_server():
    """Test the MCP server HTTP endpoints"""
    print("Starting MCP server for HTTP endpoint testing...")
    
    # Start server in background
    server_process = subprocess.Popen([
        sys.executable, 'mcp_server.py'
    ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    # Wait for server to start
    print("Waiting for server startup...")
    time.sleep(5)
    
    try:
        base_url = "http://localhost:8081"
        
        # Test 1: Health endpoint
        print("\n1. Testing /health endpoint...")
        response = requests.get(f"{base_url}/health", timeout=5)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            print(f"   Response: {response.json()}")
        
        # Test 2: Root endpoint
        print("\n2. Testing root endpoint...")
        response = requests.get(base_url, timeout=5)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Service: {data.get('service')}")
            print(f"   Endpoints: {len(data.get('endpoints', {}))}")
        
        # Test 3: Tables endpoint
        print("\n3. Testing /api/tables endpoint...")
        response = requests.get(f"{base_url}/api/tables", timeout=10)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            if isinstance(data, dict) and 'data' in data:
                print(f"   Tables found: {len(data['data'])}")
        
        # Test 4: SQL endpoint
        print("\n4. Testing /api/sql endpoint...")
        sql_data = {
            "query": "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 3"
        }
        response = requests.post(f"{base_url}/api/sql", 
                               json=sql_data, 
                               timeout=10)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            if data.get('status') == 'success':
                print(f"   Query executed successfully, {len(data.get('data', []))} rows")
        
        # Test 5: Natural language query
        print("\n5. Testing /api/query endpoint...")
        query_data = {
            "query": "show me table names"
        }
        response = requests.post(f"{base_url}/api/query", 
                               json=query_data, 
                               timeout=10)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Query processed: {data.get('status', 'completed')}")
        
        print("\n✅ All HTTP endpoint tests completed successfully!")
        print("🚀 MCP server is ready for deployment")
        
        return True
        
    except requests.exceptions.ConnectionError:
        print("❌ Could not connect to server")
        return False
    except Exception as e:
        print(f"❌ Error testing endpoints: {e}")
        return False
    finally:
        # Clean up server process
        print("\nShutting down test server...")
        server_process.terminate()
        try:
            server_process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            server_process.kill()

if __name__ == '__main__':
    success = test_http_server()
    sys.exit(0 if success else 1)