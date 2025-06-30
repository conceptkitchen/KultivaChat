#!/usr/bin/env python3
"""
Query 2 Validation Script - Test MCP implementation
Tests: "Which event made the most money from 2021 to 2024?"
"""
import requests
import json
import time

def test_query2():
    """Test Query 2 with MCP implementation"""
    print("=== QUERY 2 VALIDATION ===")
    print("Query: Which event from 2021 to 2024 made the most money for vendors?")
    print("\nStep 1: Sending request...")
    
    try:
        start_time = time.time()
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": "Which event from 2021 to 2024 made the most money for vendors?"},
            timeout=60
        )
        end_time = time.time()
        
        print(f"Response time: {end_time - start_time:.2f} seconds")
        print(f"Status: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            
            # Save response
            with open('query2_full_response.json', 'w') as f:
                json.dump(result, f, indent=2)
            
            print("\n=== COMPLETE RESPONSE ===")
            if 'response' in result:
                print(result['response'])
            else:
                print(json.dumps(result, indent=2))
                
            return True
        else:
            print(f"Error: {response.status_code}")
            print(response.text)
            return False
            
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    test_query2()