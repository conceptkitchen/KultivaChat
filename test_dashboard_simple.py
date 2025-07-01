#!/usr/bin/env python3
"""
Simple test for the dashboard endpoint
"""

import requests

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_dashboard():
    """Test dashboard endpoint"""
    print("Testing /dashboard endpoint...")
    
    try:
        response = requests.get(f"{BASE_URL}/dashboard", timeout=30)
        print(f"Status: {response.status_code}")
        print(f"Response: {response.text[:500]}...")
        
        if response.status_code == 200:
            data = response.json()
            print("SUCCESS! Dashboard data received")
            if 'metadata' in data:
                metadata = data['metadata']
                print(f"Total vendors: {metadata.get('total_vendors')}")
                print(f"Total revenue: ${metadata.get('total_revenue', 0):,.2f}")
        
    except Exception as e:
        print(f"Error: {e}")

def test_root():
    """Test root endpoint"""
    print("\nTesting root endpoint...")
    
    try:
        response = requests.get(f"{BASE_URL}/", timeout=15)
        print(f"Root status: {response.status_code}")
        
    except Exception as e:
        print(f"Root error: {e}")

if __name__ == "__main__":
    test_root()
    test_dashboard()