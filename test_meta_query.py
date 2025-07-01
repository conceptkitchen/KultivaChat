#!/usr/bin/env python3
"""
Test the enhanced meta-data query handling
"""
import requests
import json

def test_meta_data_query():
    """Test the meta-data query functionality"""
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query"
    
    query = {
        "query": "What years of sales data do we have?"
    }
    
    try:
        response = requests.post(url, json=query, timeout=30)
        print(f"Status Code: {response.status_code}")
        print(f"Response:")
        print(json.dumps(response.json(), indent=2))
        
        # Test if it's working correctly
        if response.status_code == 200:
            data = response.json()
            if data.get('status') == 'success' and data.get('query_type') == 'meta_data_analysis':
                print("\n✅ SUCCESS: Meta-data query handled correctly!")
                print(f"Available years: {data['data'][0]['available_years']}")
                print(f"Total tables: {data['data'][0]['total_event_tables']}")
            else:
                print("\n❌ ISSUE: Query not routed to meta-data handler")
        
    except Exception as e:
        print(f"Error testing API: {e}")

if __name__ == "__main__":
    test_meta_data_query()