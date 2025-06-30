#!/usr/bin/env python3
"""
Test food vendor email extraction
"""

import requests
import json

def test_food_vendor_emails():
    """Get food vendor email addresses"""
    
    url = "http://localhost:8081/api/query"
    query = {"query": "What are the email addresses of vendors that sell food?"}
    
    print("Testing food vendor email extraction...")
    print("Query: What are the email addresses of vendors that sell food?")
    
    try:
        response = requests.post(url, json=query, timeout=30)
        
        if response.status_code == 200:
            result = response.json()
            
            # Print full response for analysis
            print("\n=== FULL API RESPONSE ===")
            print(json.dumps(result, indent=2))
            
            # Extract key data
            if 'data' in result:
                data = result['data']
                print(f"\n=== EXTRACTED DATA ===")
                
                if isinstance(data, list):
                    print(f"Number of records: {len(data)}")
                    
                    # Look for email addresses
                    emails_found = []
                    for record in data:
                        if isinstance(record, dict):
                            for key, value in record.items():
                                if 'email' in key.lower() and value:
                                    emails_found.append(value)
                    
                    if emails_found:
                        print(f"\n=== FOOD VENDOR EMAILS FOUND ===")
                        for email in emails_found:
                            print(f"- {email}")
                    else:
                        print("No email addresses found in response")
                        
                else:
                    print(f"Data type: {type(data)}")
                    print(f"Data content: {data}")
            
        else:
            print(f"Error: {response.status_code}")
            print(f"Response: {response.text}")
            
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    test_food_vendor_emails()