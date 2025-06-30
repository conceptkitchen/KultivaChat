#!/usr/bin/env python3
import requests
import json

def test_query2_fast():
    try:
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": "Which event made the most money from 2021 to 2024?"},
            timeout=60
        )
        
        if response.status_code == 200:
            result = response.json()
            
            print("QUERY 2 RESULTS:")
            print("=" * 50)
            print(f"Status: {result.get('status')}")
            
            if 'data' in result and result['data']:
                print(f"Records returned: {len(result['data'])}")
                
                # Show top event
                top_event = result['data'][0]
                print(f"TOP EVENT: {top_event}")
                
                # Check for business intelligence
                if 'business_intelligence' in result:
                    print("\nBUSINESS INTELLIGENCE:")
                    print(result['business_intelligence'][:500])
                
                return True
            else:
                print("No data returned")
                return False
        else:
            print(f"Error: {response.status_code}")
            return False
            
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    success = test_query2_fast()
    print(f"\nQuery 2 MCP Test: {'PASSED' if success else 'FAILED'}")