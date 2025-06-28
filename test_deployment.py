#!/usr/bin/env python3
"""
Test the deployed Kultivate AI MCP Server API
"""
import requests
import json
import time

def test_deployed_api():
    base_url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"
    
    print(f"Testing deployed API at: {base_url}")
    print("=" * 50)
    
    # Test endpoints
    endpoints = [
        "/",
        "/health", 
        "/api/tools",
        "/api/tables"
    ]
    
    for endpoint in endpoints:
        url = f"{base_url}{endpoint}"
        try:
            print(f"\nTesting: {url}")
            response = requests.get(url, timeout=15)
            print(f"Status Code: {response.status_code}")
            
            if response.status_code == 200:
                try:
                    data = response.json()
                    print(f"Response: {json.dumps(data, indent=2)[:200]}...")
                except:
                    print(f"Response (text): {response.text[:200]}...")
            else:
                print(f"Error response: {response.text[:200]}")
                
        except requests.exceptions.ConnectionError as e:
            print(f"❌ Connection Error: {e}")
        except requests.exceptions.Timeout as e:
            print(f"❌ Timeout Error: {e}")
        except Exception as e:
            print(f"❌ Other Error: {type(e).__name__}: {e}")
    
    print("\n" + "=" * 50)
    print("Deployment test completed")

if __name__ == "__main__":
    test_deployed_api()