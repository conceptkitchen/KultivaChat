#!/usr/bin/env python3
"""
Analyze revenue field structure across all tables to ensure proper aggregation
"""

import requests
import json

def analyze_revenue_fields():
    """Analyze what revenue fields exist in the data"""
    
    BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"
    
    # Query the natural language API to understand table structure
    queries = [
        "Show me the column names from a Kapwa Gardens close-out sales table",
        "Show me the column names from an UNDISCOVERED close-out sales table",
        "What revenue fields are available in the vendor data?"
    ]
    
    print("🔍 Analyzing Revenue Field Structure")
    print("=" * 50)
    
    for query in queries:
        try:
            response = requests.post(f"{BASE_URL}/api/query", 
                                   json={"query": query}, 
                                   timeout=30)
            
            if response.status_code == 200:
                data = response.json()
                print(f"\nQuery: {query}")
                print(f"Response: {data.get('response', 'No response')[:200]}...")
            else:
                print(f"Query failed: {query} - Status: {response.status_code}")
                
        except Exception as e:
            print(f"Error with query '{query}': {e}")
    
    # Test current dashboard endpoints
    print("\n" + "=" * 50)
    print("🔍 Current Dashboard Endpoint Analysis")
    print("=" * 50)
    
    endpoints = [
        "/api/dashboard/financial-summary",
        "/api/dashboard/vendor-performance",
        "/api/dashboard/revenue-breakdown"
    ]
    
    for endpoint in endpoints:
        try:
            response = requests.get(f"{BASE_URL}{endpoint}", timeout=15)
            if response.status_code == 200:
                data = response.json()
                print(f"\n✅ {endpoint}:")
                
                if endpoint == "/api/dashboard/vendor-performance":
                    vendors = data.get('data', [])[:3]
                    for vendor in vendors:
                        revenue = vendor.get('total_sales', vendor.get('revenue', 0))
                        print(f"   • {vendor.get('vendor_name', 'Unknown')}: ${revenue}")
                
                elif endpoint == "/api/dashboard/financial-summary":
                    summary = data.get('data', [])
                    for item in summary:
                        print(f"   • {item.get('event_type', 'Unknown')}: ${item.get('total_revenue', 0)}")
                        
                elif endpoint == "/api/dashboard/revenue-breakdown":
                    breakdown = data.get('data', [])[:3]
                    total = data.get('total_revenue', 0)
                    print(f"   • Total Revenue: ${total}")
                    for item in breakdown:
                        print(f"   • {item.get('event_name', 'Unknown')}: ${item.get('revenue', 0)}")
                        
            else:
                print(f"❌ {endpoint}: Status {response.status_code}")
                
        except Exception as e:
            print(f"❌ {endpoint}: Error - {e}")

if __name__ == "__main__":
    analyze_revenue_fields()