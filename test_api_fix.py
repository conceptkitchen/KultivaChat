#!/usr/bin/env python3
import requests
import json
import time

def quick_api_test():
    """Quick test to verify API is working and not spinning"""
    base_url = 'https://kultivate-chat-ck.replit.app'
    
    print("Testing API fix for spinning issue...")
    
    # Test 1: Table discovery (should work quickly)
    print("\n1. Testing table discovery...")
    start_time = time.time()
    
    try:
        response = requests.post(
            f"{base_url}/api/v1/data/query",
            headers={'Content-Type': 'application/json'},
            json={'query': 'show me tables'},
            timeout=10
        )
        
        end_time = time.time()
        print(f"   Response time: {end_time - start_time:.2f} seconds")
        print(f"   Status: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print(f"   Success: {data.get('success', False)}")
            print(f"   Route: {data.get('route_used', 'unknown')}")
            
            if data.get('data'):
                print(f"   Tables found: {len(data.get('data', []))}")
            else:
                print(f"   Response: {data.get('response', 'No response')[:100]}")
        else:
            print(f"   Error: {response.text[:100]}")
            
    except requests.exceptions.Timeout:
        print("   ERROR: Request timed out (still spinning)")
    except Exception as e:
        print(f"   ERROR: {str(e)}")
    
    # Test 2: Simple natural language query
    print("\n2. Testing natural language query...")
    start_time = time.time()
    
    try:
        response = requests.post(
            f"{base_url}/api/v1/data/query",
            headers={'Content-Type': 'application/json'},
            json={'query': 'show me data from Balay Kreative'},
            timeout=15
        )
        
        end_time = time.time()
        print(f"   Response time: {end_time - start_time:.2f} seconds")
        print(f"   Status: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print(f"   Success: {data.get('success', False)}")
            print(f"   Route: {data.get('route_used', 'unknown')}")
            print(f"   Response: {data.get('response', 'No response')[:150]}")
        else:
            print(f"   Error: {response.text[:100]}")
            
    except requests.exceptions.Timeout:
        print("   ERROR: Request timed out (still spinning)")
    except Exception as e:
        print(f"   ERROR: {str(e)}")
    
    print("\nAPI test complete.")

if __name__ == "__main__":
    quick_api_test()