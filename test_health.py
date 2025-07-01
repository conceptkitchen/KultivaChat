#!/usr/bin/env python3
"""
Test health endpoint to see if server is responding with updated code
"""
import requests

def test_health():
    """Test health endpoint"""
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/health"
    
    try:
        response = requests.get(url, timeout=10)
        print(f"Health Status: {response.status_code}")
        print(f"Health Response: {response.text}")
    except Exception as e:
        print(f"Health check failed: {e}")

def test_root():
    """Test root endpoint"""
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/"
    
    try:
        response = requests.get(url, timeout=10)
        print(f"Root Status: {response.status_code}")
        print(f"Root Response (first 200 chars): {response.text[:200]}")
    except Exception as e:
        print(f"Root check failed: {e}")

if __name__ == "__main__":
    print("Testing health...")
    test_health()
    print("\nTesting root...")
    test_root()