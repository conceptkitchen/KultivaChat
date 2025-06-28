#!/usr/bin/env python3
"""
Test API query functionality on deployed server
"""
import requests
import json

def test_api_query():
    base_url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"
    
    # Test natural language query
    query_data = {
        "query": "Show me the first 3 tables in my workspace"
    }
    
    try:
        response = requests.post(
            f"{base_url}/api/query", 
            json=query_data,
            timeout=20
        )
        print(f"Query Status: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print("✅ Query successful!")
            print(f"Response: {json.dumps(data, indent=2)[:500]}...")
        else:
            print(f"❌ Query failed: {response.text}")
            
    except Exception as e:
        print(f"❌ Query test error: {e}")

if __name__ == "__main__":
    test_api_query()