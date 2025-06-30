#!/usr/bin/env python3
import requests
import json

def test_original_query2():
    """Test the original Query 2 with proper natural language handling"""
    print("=== TESTING ORIGINAL QUERY 2 ===")
    print("Query: Which event from 2021 to 2024 made the most money for vendors?")
    print("Expected: System should find data within range and explain no 2021-2022 data exists")
    
    # Send original query exactly as asked
    try:
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": "Which event from 2021 to 2024 made the most money for vendors?"},
            timeout=30
        )
        
        if response.status_code == 200:
            result = response.json()
            
            print("\n=== RESPONSE ===")
            if 'response' in result:
                print(result['response'])
            else:
                print(json.dumps(result, indent=2))
                
            # Save the response
            with open('original_query2_response.json', 'w') as f:
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
    test_original_query2()