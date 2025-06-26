#!/usr/bin/env python3
import requests
import json

def test_enhanced_natural_language():
    """Test the enhanced natural language processing capabilities"""
    base_url = 'https://kultivate-chat-ck.replit.app'
    
    test_queries = [
        "Show me revenue from Balay Kreative events",
        "What tables are available?", 
        "How many people attended Filipino events?",
        "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 3"
    ]
    
    print("Testing Enhanced Natural Language API...")
    print("=" * 60)
    
    for i, query in enumerate(test_queries, 1):
        print(f"\n{i}. Testing: '{query}'")
        
        try:
            response = requests.post(
                f"{base_url}/api/v1/data/query",
                headers={'Content-Type': 'application/json'},
                json={'query': query},
                timeout=30
            )
            
            print(f"   Status: {response.status_code}")
            
            if response.status_code == 200:
                data = response.json()
                print(f"   Success: {data.get('success', False)}")
                print(f"   Route: {data.get('route_used', 'unknown')}")
                print(f"   Response: {data.get('response', 'No response')[:100]}...")
                
                if data.get('data'):
                    rows = data.get('rows_returned', 0)
                    print(f"   Data returned: {rows} rows")
                else:
                    print("   No data returned")
                    
            else:
                print(f"   Error: {response.status_code} - {response.text[:100]}")
                
        except Exception as e:
            print(f"   Exception: {str(e)[:100]}")
    
    print("\n" + "=" * 60)
    print("Enhanced API Test Complete")

if __name__ == "__main__":
    test_enhanced_natural_language()