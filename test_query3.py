#!/usr/bin/env python3
import requests
import json
import time

def test_query3():
    """Test Query 3: Show me vendors who made over $500"""
    print("=== QUERY 3 SYSTEMATIC TEST ===")
    print("Query: Show me vendors who made over $500")
    print("\nStep 1: Sending request...")
    
    try:
        start_time = time.time()
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": "Show me vendors who made over $500"},
            timeout=45
        )
        end_time = time.time()
        
        print(f"Response time: {end_time - start_time:.2f} seconds")
        print(f"Status: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            
            print("\n=== STEP 1: RESPONSE ===")
            if 'response' in result:
                print(result['response'])
            else:
                print("Raw response structure:")
                print(json.dumps(result, indent=2))
            
            # Save response for validation
            with open('query3_response.json', 'w') as f:
                json.dump(result, f, indent=2)
                
            return True
        else:
            print(f"Error: {response.status_code}")
            print(response.text)
            return False
            
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    test_query3()