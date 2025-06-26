#!/usr/bin/env python3
import requests
import json
import time

def test_api():
    base_url = 'https://kultivate-chat-ck.replit.app'
    
    print("Testing API Documentation...")
    print("=" * 50)
    
    # Test 1: Health Check
    print("\n1. Health Check Endpoint")
    try:
        response = requests.get(f"{base_url}/api/health", timeout=10)
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            print(f"   Response: {data}")
            print("   ✅ Health check working")
        else:
            print(f"   ❌ Health check failed: {response.status_code}")
    except Exception as e:
        print(f"   ❌ Health check error: {e}")
    
    # Test 2: Tables Discovery
    print("\n2. Tables Discovery Endpoint")
    try:
        response = requests.post(
            f"{base_url}/api/v1/data/tables",
            headers={'Content-Type': 'application/json'},
            json={},
            timeout=15
        )
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            tables_count = len(data.get('data', []))
            print(f"   Tables found: {tables_count}")
            print(f"   Success: {data.get('success', False)}")
            print("   ✅ Tables endpoint working")
        else:
            print(f"   ❌ Tables endpoint failed: {response.status_code}")
    except Exception as e:
        print(f"   ❌ Tables endpoint error: {e}")
    
    # Test 3: SQL Execution
    print("\n3. SQL Execution Endpoint")
    try:
        response = requests.post(
            f"{base_url}/api/v1/data/sql",
            headers={'Content-Type': 'application/json'},
            json={'sql': 'SELECT 1 as test_value, "hello" as test_string'},
            timeout=15
        )
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            rows = data.get('rows_returned', 0)
            print(f"   Rows returned: {rows}")
            print(f"   Success: {data.get('success', False)}")
            print("   ✅ SQL endpoint working")
        else:
            print(f"   ❌ SQL endpoint failed: {response.status_code}")
    except Exception as e:
        print(f"   ❌ SQL endpoint error: {e}")
    
    # Test 4: Intelligent Router
    print("\n4. Intelligent Query Router")
    try:
        response = requests.post(
            f"{base_url}/api/v1/data/query",
            headers={'Content-Type': 'application/json'},
            json={'query': 'show me tables'},
            timeout=15
        )
        print(f"   Status: {response.status_code}")
        if response.status_code == 200:
            data = response.json()
            route = data.get('route_used', 'unknown')
            print(f"   Route used: {route}")
            print(f"   Success: {data.get('success', False)}")
            print("   ✅ Router working")
        else:
            print(f"   ❌ Router failed: {response.status_code}")
    except Exception as e:
        print(f"   ❌ Router error: {e}")
    
    print("\n" + "=" * 50)
    print("API Documentation Test Complete")

if __name__ == "__main__":
    test_api()