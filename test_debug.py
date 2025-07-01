#!/usr/bin/env python3
"""
Test meta-data query with debugging
"""
import requests
import json

def test_debug():
    """Test the exact query that should trigger meta-data handling"""
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query"
    
    query = {
        "query": "What years of sales data do we have?"
    }
    
    try:
        response = requests.post(url, json=query, timeout=20)
        print(f"Status Code: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print(f"Response status: {data.get('status')}")
            print(f"Routing method: {data.get('routing_method')}")
            print(f"Query type: {data.get('query_type')}")
            
            if data.get('status') == 'success' and data.get('query_type') == 'meta_data_analysis':
                print("✅ SUCCESS: Meta-data query handled correctly!")
                print(f"Available years: {data['data'][0]['available_years']}")
            else:
                print("❌ STILL GOING TO MCP TOOL INSTEAD OF API-LEVEL HANDLER")
                if 'reasoning' in data and 'available_years' in data['reasoning']:
                    print(f"At least error reasoning shows years: {data['reasoning']['available_years']}")
        else:
            print(f"❌ ERROR: Status {response.status_code}")
            print(response.text)
                
    except Exception as e:
        print(f"❌ ERROR: {e}")

if __name__ == "__main__":
    test_debug()