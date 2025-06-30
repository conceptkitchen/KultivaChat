#!/usr/bin/env python3
"""
Debug revenue query routing to find the exact source of false error
"""
import requests
import json

def debug_revenue_routing():
    """Debug the exact routing path for revenue queries"""
    
    print("=== DEBUGGING REVENUE QUERY ROUTING ===")
    
    # Test 1: Simple revenue query
    print("\n1. Testing simple revenue query...")
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': 'revenue analysis'},
        headers={'Content-Type': 'application/json'})
    
    print(f"Status: {response.status_code}")
    data = response.json()
    print(f"Response: {json.dumps(data, indent=2)}")
    
    # Test 2: Vendor query (should hit revenue routing)
    print("\n2. Testing vendor query...")
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': 'show me vendor data'},
        headers={'Content-Type': 'application/json'})
    
    print(f"Status: {response.status_code}")
    data = response.json()
    print(f"Response: {json.dumps(data, indent=2)}")
    
    # Test 3: Direct comprehensive query (should work)
    print("\n3. Testing comprehensive query...")
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': 'comprehensive business intelligence analysis'},
        headers={'Content-Type': 'application/json'})
    
    print(f"Status: {response.status_code}")
    data = response.json()
    if 'error' in data:
        print(f"Error: {data['error']}")
    elif 'data' in data:
        print(f"Success: {len(data['data'])} records")
    elif 'tables' in data:
        print(f"Success: {len(data['tables'])} tables")
    else:
        print(f"Unexpected: {str(data)[:100]}")

if __name__ == "__main__":
    debug_revenue_routing()