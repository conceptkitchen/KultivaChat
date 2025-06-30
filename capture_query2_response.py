#!/usr/bin/env python3
import requests
import json
import sys

def capture_query2():
    """Capture complete Query 2 response"""
    try:
        print("Sending Query 2: Which event from 2021 to 2024 made the most money for vendors?")
        
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": "Which event from 2021 to 2024 made the most money for vendors?"},
            timeout=90
        )
        
        if response.status_code == 200:
            result = response.json()
            
            # Save complete response
            with open('query2_response.json', 'w') as f:
                json.dump(result, f, indent=2)
            
            print("=== QUERY 2 COMPLETE RESPONSE ===")
            print(json.dumps(result, indent=2))
            
            return True
        else:
            print(f"HTTP Error: {response.status_code}")
            print(response.text)
            return False
            
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    capture_query2()