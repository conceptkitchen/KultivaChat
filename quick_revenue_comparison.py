#!/usr/bin/env python3
"""
Quick revenue comparison across all events 2021-2024
Direct BigQuery approach to get the highest revenue event
"""

import requests
import json
import time

def test_revenue_comparison():
    """Get the top revenue event from 2021-2024"""
    
    url = "http://localhost:8081/api/query"
    
    # Direct SQL approach for faster results
    query = {
        "query": "Which event from 2021 to 2024 made the most money for vendors?",
        "sql_direct": True
    }
    
    print("🔍 Testing cross-year revenue comparison...")
    print(f"Query: {query['query']}")
    
    try:
        response = requests.post(url, json=query, timeout=30)
        
        if response.status_code == 200:
            result = response.json()
            print("\n✅ SUCCESS - API Response:")
            print(json.dumps(result, indent=2))
            
            # Extract key information
            if 'data' in result and result['data']:
                data = result['data']
                if isinstance(data, list) and len(data) > 0:
                    top_event = data[0]
                    print(f"\n🏆 TOP EVENT: {top_event.get('table_source', 'Unknown')}")
                    print(f"💰 Total Revenue: ${top_event.get('total_revenue', 0):,.2f}")
                    print(f"📊 Record Count: {top_event.get('record_count', 0)}")
            
        else:
            print(f"❌ API Error: {response.status_code}")
            print(f"Response: {response.text}")
            
    except requests.exceptions.Timeout:
        print("⏱️ Request timed out - comprehensive analysis takes time")
        print("The API is processing multiple tables across 4 years")
        
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    test_revenue_comparison()