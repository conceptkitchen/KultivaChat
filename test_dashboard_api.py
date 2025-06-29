#!/usr/bin/env python3
"""
Test Dashboard API Endpoints
Validates the four dashboard endpoints with authentic financial data
"""

import requests
import json

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_endpoint(endpoint_path, description):
    """Test a dashboard API endpoint"""
    url = f"{BASE_URL}{endpoint_path}"
    print(f"\n=== Testing {description} ===")
    print(f"URL: {url}")
    
    try:
        response = requests.get(url, timeout=15)
        print(f"Status Code: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print(f"Response Keys: {list(data.keys())}")
            
            if data.get('status') == 'success':
                print(f"✅ SUCCESS - Data Source: {data.get('source', 'unknown')}")
                
                # Show sample data
                if 'data' in data and data['data']:
                    sample_record = data['data'][0]
                    print(f"Sample Record: {json.dumps(sample_record, indent=2)}")
                    print(f"Total Records: {len(data['data'])}")
                    
                    # Calculate totals for financial endpoints
                    if 'revenue' in str(sample_record):
                        total_revenue = sum(item.get('revenue', 0) for item in data['data'] if isinstance(item.get('revenue'), (int, float)))
                        print(f"Total Revenue: ${total_revenue:,.2f}")
                
                return True
            else:
                print(f"❌ FAILED - Status: {data.get('status')}")
                return False
        else:
            print(f"❌ HTTP ERROR - Status: {response.status_code}")
            print(f"Response: {response.text[:500]}")
            return False
            
    except Exception as e:
        print(f"❌ EXCEPTION - {str(e)}")
        return False

def main():
    """Test all dashboard endpoints"""
    print("🚀 Dashboard API Testing Started")
    print("=" * 50)
    
    endpoints = [
        ("/api/dashboard/financial-summary", "Financial Summary"),
        ("/api/dashboard/vendor-performance", "Vendor Performance"),
        ("/api/dashboard/event-timeline", "Event Timeline"),
        ("/api/dashboard/revenue-breakdown", "Revenue Breakdown")
    ]
    
    results = []
    for endpoint, description in endpoints:
        success = test_endpoint(endpoint, description)
        results.append((description, success))
    
    print("\n" + "=" * 50)
    print("🎯 DASHBOARD API TEST RESULTS")
    print("=" * 50)
    
    for description, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} - {description}")
    
    success_count = sum(1 for _, success in results if success)
    total_count = len(results)
    
    print(f"\nOverall: {success_count}/{total_count} endpoints working")
    
    if success_count == total_count:
        print("🎉 All dashboard endpoints operational!")
        print("👉 Frontend can now connect to dashboard API for financial data visualization")
    else:
        print("⚠️  Some endpoints need attention")

if __name__ == "__main__":
    main()