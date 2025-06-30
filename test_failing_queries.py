#!/usr/bin/env python3
"""
Test script for the failing revenue analysis queries
"""
import requests
import json
import time

def test_api_query(query, description):
    """Test a single API query"""
    print(f"\n{'='*50}")
    print(f"Testing: {description}")
    print(f"Query: {query}")
    print(f"{'='*50}")
    
    try:
        response = requests.post(
            'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
            json={'query': query},
            headers={'Content-Type': 'application/json'},
            timeout=30
        )
        
        print(f"Status Code: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            
            # Check for errors
            if 'error' in data:
                print(f"❌ API Error: {data['error']}")
                return False
            
            # Check for successful response
            if 'status' in data and data['status'] == 'success':
                if 'data' in data and data['data']:
                    print(f"✅ Success: Found {len(data['data'])} records")
                    print(f"Sample record: {json.dumps(data['data'][0], indent=2)[:300]}...")
                    return True
                else:
                    print(f"⚠️ Success but no data returned")
                    return False
            
            # Check for other response formats
            if 'tables' in data:
                print(f"✅ Success: Found {len(data['tables'])} tables")
                return True
            
            # General success response
            print(f"✅ Response received: {str(data)[:500]}...")
            return True
            
        else:
            print(f"❌ HTTP Error {response.status_code}: {response.text[:200]}")
            return False
            
    except requests.exceptions.Timeout:
        print(f"❌ Request timed out after 30 seconds")
        return False
    except Exception as e:
        print(f"❌ Exception: {str(e)}")
        return False

def main():
    """Test all the failing queries"""
    print("🚀 Testing Previously Failing Revenue Analysis Queries")
    print("="*60)
    
    # Test queries from the user's failed report
    test_queries = [
        ("Which Kapwa Gardens vendors made over $500?", "Revenue Threshold Analysis"),
        ("Show me vendors who participated in multiple Kapwa Gardens events", "Multi-event Participation Tracking"),
        ("How has Kapwa Gardens revenue changed over time?", "Performance Trend Analysis"),
        ("Show me comprehensive Kapwa Gardens business intelligence", "Comprehensive Business Intelligence"),
        ("show me all my tables", "Table Discovery Test"),
        ("Show me revenue from Kapwa Gardens events", "Basic Revenue Query"),
        ("Which vendors had the highest sales?", "Top Performer Analysis"),
        ("Show me data from close out sales", "Sales Data Query")
    ]
    
    results = []
    
    for query, description in test_queries:
        success = test_api_query(query, description)
        results.append((description, success))
        time.sleep(2)  # Brief pause between requests
    
    # Summary
    print(f"\n{'='*60}")
    print("📊 TEST RESULTS SUMMARY")
    print(f"{'='*60}")
    
    passed = sum(1 for _, success in results if success)
    total = len(results)
    
    for description, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status}: {description}")
    
    print(f"\nOverall: {passed}/{total} queries working ({passed/total*100:.1f}%)")
    
    if passed == total:
        print("🎉 All queries are now working!")
    elif passed > 0:
        print("⚠️ Some queries working - partial fix successful")
    else:
        print("❌ All queries still failing - additional fixes needed")

if __name__ == "__main__":
    main()