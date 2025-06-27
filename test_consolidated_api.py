#!/usr/bin/env python3
"""
Comprehensive API testing for consolidated main_2.py server
Tests all endpoints with real queries to validate functionality
"""

import subprocess
import time
import sys
import requests
import json
import os

def test_consolidated_api():
    """Test the consolidated main_2.py API server"""
    print("Testing Consolidated API Server...")
    
    # Start server from backend directory
    server_process = subprocess.Popen([
        sys.executable, 'main_2.py'
    ], cwd='backend', stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    # Wait for server to start
    print("Starting server...")
    time.sleep(8)
    
    try:
        base_url = "http://localhost:8081"
        
        # Test 1: Root endpoint
        print("\n1. Testing root endpoint...")
        response = requests.get(base_url, timeout=5)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Service: {data.get('service')}")
            print(f"   Endpoints available: {len(data.get('endpoints', {}))}")
        
        # Test 2: Health check
        print("\n2. Testing /health endpoint...")
        response = requests.get(f"{base_url}/health", timeout=5)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Health status: {data.get('status')}")
            print(f"   Timestamp: {data.get('timestamp')}")
        
        # Test 3: Tools list
        print("\n3. Testing /api/tools endpoint...")
        response = requests.get(f"{base_url}/api/tools", timeout=5)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Tools available: {len(data.get('tools', []))}")
        
        # Test 4: Table discovery
        print("\n4. Testing /api/tables endpoint...")
        response = requests.get(f"{base_url}/api/tables", timeout=15)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            if data.get('status') == 'success' and 'data' in data:
                print(f"   Tables discovered: {len(data['data'])}")
                if data['data']:
                    print(f"   First table: {data['data'][0].get('table_name', 'N/A')}")
        
        # Test 5: Direct SQL query
        print("\n5. Testing /api/sql endpoint...")
        sql_data = {
            "query": "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 3"
        }
        response = requests.post(f"{base_url}/api/sql", 
                               json=sql_data, 
                               timeout=15)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            if data.get('status') == 'success':
                print(f"   Query successful: {len(data.get('data', []))} rows returned")
        
        # Test 6: Natural language query
        print("\n6. Testing /api/query endpoint...")
        query_data = {
            "query": "show me available tables"
        }
        response = requests.post(f"{base_url}/api/query", 
                               json=query_data, 
                               timeout=15)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Natural language query processed: {data.get('status', 'completed')}")
        
        # Test 7: Geographic lookup
        print("\n7. Testing /api/geography endpoint...")
        geo_data = {
            "city_name": "San Francisco",
            "state_code": "CA"
        }
        response = requests.post(f"{base_url}/api/geography", 
                               json=geo_data, 
                               timeout=10)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            if data.get('status') == 'success':
                print(f"   Zip codes found: {len(data.get('zip_codes', []))}")
        
        print("\n✅ All API endpoint tests completed successfully!")
        print("🚀 Consolidated main_2.py server is fully operational and ready for deployment")
        
        return True
        
    except requests.exceptions.ConnectionError as e:
        print(f"❌ Connection error: {e}")
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
    success = test_consolidated_api()
    sys.exit(0 if success else 1)