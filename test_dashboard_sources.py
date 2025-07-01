#!/usr/bin/env python3
"""
Test both dashboard sources to verify combined revenue calculations
"""

import requests
import json

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_direct_query(query, description):
    """Test a direct query through the natural language API"""
    print(f"\n🔍 TESTING: {description}")
    print("=" * 60)
    
    try:
        response = requests.post(f"{BASE_URL}/api/query", 
                               json={"query": query}, 
                               timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            print(f"✅ SUCCESS")
            print(f"Response preview: {data.get('response', 'No response')[:300]}...")
            
            # Look for revenue numbers in the response
            response_text = data.get('response', '')
            import re
            amounts = re.findall(r'\$[\d,]+\.?\d*', response_text)
            if amounts:
                print(f"💰 Found revenue amounts: {amounts}")
            
            return data
        else:
            print(f"❌ FAILED - Status: {response.status_code}")
            return None
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return None

def main():
    """Test both dashboard data sources"""
    print("🚀 TESTING BOTH DASHBOARD DATA SOURCES")
    print("Checking if we can access both close-out sales and Squarespace forms...")
    
    # Test queries to understand what data sources exist
    test_queries = [
        ("List all tables in my workspace", "Table Discovery"),
        ("What close-out sales tables do I have?", "Close-out Sales Tables"),
        ("Show me vendor registration or Squarespace forms data", "Squarespace Forms"),
        ("What's the total revenue from all sources?", "Combined Revenue Test"),
        ("Show me revenue from Kapwa Gardens events", "Kapwa Gardens Revenue"),
        ("Show me revenue from UNDISCOVERED events", "UNDISCOVERED Revenue")
    ]
    
    for query, description in test_queries:
        test_direct_query(query, description)
    
    # Also test the dashboard endpoints again
    print(f"\n{'='*60}")
    print("🔍 FINAL DASHBOARD ENDPOINT TEST")
    print(f"{'='*60}")
    
    endpoints = [
        "/api/dashboard/financial-summary",
        "/api/dashboard/revenue-breakdown"
    ]
    
    for endpoint in endpoints:
        try:
            response = requests.get(f"{BASE_URL}{endpoint}", timeout=15)
            if response.status_code == 200:
                data = response.json()
                source = data.get('source', 'Unknown')
                note = data.get('note', 'N/A')
                
                if endpoint == "/api/dashboard/financial-summary":
                    total = sum(item.get('total_revenue', 0) for item in data.get('data', []))
                    print(f"✅ Financial Summary: ${total:,.2f} (Source: {source})")
                    
                elif endpoint == "/api/dashboard/revenue-breakdown":
                    total = data.get('total_revenue', 0)
                    print(f"✅ Revenue Breakdown: ${total:,.2f} (Source: {source})")
                    print(f"   Note: {note}")
                    
            else:
                print(f"❌ {endpoint}: Status {response.status_code}")
                
        except Exception as e:
            print(f"❌ {endpoint}: Error - {e}")

if __name__ == "__main__":
    main()