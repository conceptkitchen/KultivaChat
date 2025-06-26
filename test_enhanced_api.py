#!/usr/bin/env python3
"""
Test the enhanced natural language API capabilities
"""
import requests
import json

def test_enhanced_natural_language():
    """Test the enhanced natural language processing capabilities"""
    
    base_url = "https://kultivate-chat-ck.replit.app"
    endpoint = "/api/v1/data/query"
    
    test_queries = [
        {
            "query": "show me tables",
            "expected": "table discovery with actual BigQuery tables"
        },
        {
            "query": "show me data from Balay Kreative events",
            "expected": "business entity data with actual records"
        },
        {
            "query": "what revenue did we generate from Kapwa Gardens",
            "expected": "revenue analysis with real financial data"
        },
        {
            "query": "list all attendees from recent events",
            "expected": "attendee data from event tables"
        }
    ]
    
    print("🧪 Testing Enhanced Natural Language API")
    print(f"📡 Testing endpoint: {base_url}{endpoint}")
    print("=" * 60)
    
    for i, test in enumerate(test_queries, 1):
        print(f"\n📋 Test {i}: {test['query']}")
        print(f"   Expected: {test['expected']}")
        
        try:
            response = requests.post(
                f"{base_url}{endpoint}",
                json={"query": test["query"]},
                headers={"Content-Type": "application/json"},
                timeout=30
            )
            
            print(f"   Status: {response.status_code}")
            
            if response.status_code == 200:
                data = response.json()
                print(f"   Success: {data.get('success')}")
                
                if data.get('data') and len(data['data']) > 0:
                    print(f"   Data Records: {len(data['data'])}")
                    if data.get('table_name'):
                        print(f"   Table: {data['table_name']}")
                    print("   ✅ DATA RETRIEVED")
                else:
                    print(f"   Response: {data.get('response', '')[:80]}...")
                    if data.get('ai_analysis'):
                        print("   ✅ AI PROCESSED")
                    else:
                        print("   ⚠️ NO DATA")
            else:
                print(f"   ❌ ERROR: {response.status_code}")
                
        except requests.exceptions.Timeout:
            print("   ❌ TIMEOUT")
        except Exception as e:
            print(f"   ❌ ERROR: {str(e)}")
    
    print("\n" + "=" * 60)
    print("🎯 Enhanced API Test Complete")

if __name__ == "__main__":
    test_enhanced_natural_language()