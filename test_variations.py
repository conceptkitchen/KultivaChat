#!/usr/bin/env python3
"""
Test different variations of meta-data queries
"""
import requests
import json

def test_variations():
    """Test multiple variations of the meta-data query"""
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query"
    
    queries = [
        "What years of sales data do we have?",
        "Which years of data do we have?", 
        "What data do we have?",
        "Data coverage please",
        "How many years of data are available?"
    ]
    
    for query_text in queries:
        print(f"\n🧪 Testing: '{query_text}'")
        query = {"query": query_text}
        
        try:
            response = requests.post(url, json=query, timeout=15)
            print(f"Status: {response.status_code}")
            
            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'success' and data.get('query_type') == 'meta_data_analysis':
                    print("✅ SUCCESS: Meta-data query handled correctly!")
                    print(f"Years: {data['data'][0]['available_years']}")
                else:
                    print(f"❌ ISSUE: Routing method = {data.get('routing_method')}")
                    print(f"Status = {data.get('status')}")
                    print(f"Query type = {data.get('query_type')}")
            else:
                print(f"❌ ERROR: Status {response.status_code}")
                
        except Exception as e:
            print(f"❌ ERROR: {e}")

if __name__ == "__main__":
    test_variations()