#!/usr/bin/env python3
"""
Get the actual result from the revenue comparison
"""

import requests
import json
import time

def get_quick_result():
    """Get quick result using simpler query"""
    
    url = "http://localhost:8081/api/query"
    
    # Test with a more focused query first
    query = {"query": "What was the highest revenue event in 2024?"}
    
    print("Getting 2024 highest revenue event...")
    
    try:
        response = requests.post(url, json=query, timeout=20)
        
        if response.status_code == 200:
            result = response.json()
            
            if 'data' in result and result['data']:
                data = result['data'][0] if isinstance(result['data'], list) else result['data']
                
                print(f"\nHIGHEST 2024 EVENT:")
                print(f"Event: {data.get('table_source', 'Unknown')}")
                print(f"Revenue: ${data.get('total_revenue', 0):,.2f}")
                print(f"Vendors: {data.get('record_count', 0)}")
                
                # Extract event name
                table_name = data.get('table_source', '')
                if 'UNDISCOVERED' in table_name:
                    event_type = "UNDISCOVERED SF"
                elif 'Kapwa-Gardens' in table_name:
                    event_type = "Kapwa Gardens"
                elif 'Sulat' in table_name:
                    event_type = "Sulat"
                else:
                    event_type = "Unknown"
                    
                print(f"Event Type: {event_type}")
                
                return data.get('total_revenue', 0), event_type
                
        else:
            print(f"Error: {response.status_code}")
            
    except Exception as e:
        print(f"Error: {e}")
        
    return None, None

if __name__ == "__main__":
    revenue, event = get_quick_result()
    
    if revenue:
        print(f"\nBased on 2024 data, the highest revenue event generated ${revenue:,.2f}")
        print(f"Event type: {event}")
        print("\nFor complete 2021-2024 comparison, the API processes all tables comprehensively")