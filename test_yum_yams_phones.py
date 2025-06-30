#!/usr/bin/env python3
"""
Get Yum Yams vendor phone numbers
"""

import requests
import json

def test_yum_yams_phones():
    """Get phone numbers for Yum Yams event vendors"""
    
    url = "http://localhost:8081/api/query"
    query = {"query": "What are the cell numbers of vendors that participated in Yum Yams event?"}
    
    print("Testing Yum Yams vendor phone extraction...")
    print("Query: What are the cell numbers of vendors that participated in Yum Yams event?")
    
    try:
        response = requests.post(url, json=query, timeout=30)
        
        if response.status_code == 200:
            result = response.json()
            
            # Extract phone numbers from response
            if 'data' in result:
                data = result['data']
                print(f"\nYUM YAMS EVENT DATA:")
                
                if isinstance(data, list) and len(data) > 0:
                    event_data = data[0]
                    print(f"Event: {event_data.get('table_source', 'Unknown')}")
                    print(f"Total Vendors: {event_data.get('record_count', 0)}")
                    print(f"Total Revenue: ${event_data.get('total_revenue', 0):,.2f}")
                    
                    # Check if detailed vendor records with phone numbers are included
                    if len(data) > 1:
                        print(f"\nVENDOR PHONE NUMBERS:")
                        phones_found = []
                        
                        for record in data[1:]:  # Skip summary record
                            if isinstance(record, dict):
                                for key, value in record.items():
                                    if 'phone' in key.lower() and value and value.strip():
                                        phones_found.append(value.strip())
                        
                        if phones_found:
                            for i, phone in enumerate(phones_found, 1):
                                print(f"{i}. {phone}")
                        else:
                            print("Phone numbers extracted but need to process detailed records")
                    else:
                        print("\nSummary data returned - need detailed vendor records for phone numbers")
                        
            # Print full response for debugging
            print(f"\n=== FULL RESPONSE ===")
            print(json.dumps(result, indent=2)[:1000] + "..." if len(json.dumps(result)) > 1000 else json.dumps(result, indent=2))
            
        else:
            print(f"Error: {response.status_code}")
            print(f"Response: {response.text}")
            
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    test_yum_yams_phones()