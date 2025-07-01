#!/usr/bin/env python3
"""
Trace the actual revenue calculations to show where the numbers come from
"""

import requests
import json

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_endpoint_details(endpoint, name):
    """Test an endpoint and show detailed breakdown"""
    print(f"\n{'='*60}")
    print(f"TESTING: {name}")
    print(f"ENDPOINT: {endpoint}")
    print(f"{'='*60}")
    
    try:
        response = requests.get(f"{BASE_URL}{endpoint}", timeout=20)
        
        if response.status_code == 200:
            data = response.json()
            print(f"✅ SUCCESS - Status: {response.status_code}")
            print(f"📊 Raw Response Structure:")
            print(f"   - Status: {data.get('status')}")
            print(f"   - Data Type: {type(data.get('data'))}")
            print(f"   - Data Source: {data.get('source', 'Unknown')}")
            
            if endpoint == "/api/dashboard/vendor-performance":
                vendors = data.get('data', [])
                print(f"\n📈 VENDOR PERFORMANCE BREAKDOWN:")
                print(f"   - Total Vendors Returned: {len(vendors)}")
                if vendors:
                    print(f"   - First 5 vendors:")
                    for i, vendor in enumerate(vendors[:5], 1):
                        revenue = vendor.get('total_sales', vendor.get('revenue', 'N/A'))
                        event_count = vendor.get('event_count', 'N/A')
                        print(f"     #{i}: {vendor.get('vendor_name')} = ${revenue} (Events: {event_count})")
                    
                    # Check if revenue is being aggregated
                    top_vendor = vendors[0]
                    print(f"\n🔍 TOP VENDOR ANALYSIS:")
                    print(f"   - Name: {top_vendor.get('vendor_name')}")
                    print(f"   - Revenue: ${top_vendor.get('total_sales', top_vendor.get('revenue'))}")
                    print(f"   - Event Count: {top_vendor.get('event_count', 'N/A')}")
                    print(f"   - All Events: {top_vendor.get('all_events', 'N/A')}")
            
            elif endpoint == "/api/dashboard/financial-summary":
                summary = data.get('data', [])
                print(f"\n💰 FINANCIAL SUMMARY BREAKDOWN:")
                total_across_events = 0
                for item in summary:
                    revenue = item.get('total_revenue', 0)
                    vendors = item.get('vendor_count', 0)
                    event_type = item.get('event_type', 'Unknown')
                    total_across_events += revenue
                    print(f"   - {event_type}: ${revenue} ({vendors} vendors)")
                print(f"   - CALCULATED TOTAL: ${total_across_events}")
            
            elif endpoint == "/api/dashboard/revenue-breakdown":
                breakdown = data.get('data', [])
                api_total = data.get('total_revenue', 0)
                print(f"\n📊 REVENUE BREAKDOWN:")
                print(f"   - API Reported Total: ${api_total}")
                print(f"   - Events Breakdown:")
                calculated_total = 0
                for item in breakdown:
                    revenue = item.get('revenue', 0)
                    calculated_total += revenue
                    print(f"     • {item.get('event_name')}: ${revenue}")
                print(f"   - CALCULATED TOTAL: ${calculated_total}")
                print(f"   - DIFFERENCE: ${abs(api_total - calculated_total)}")
            
            return True
            
        else:
            print(f"❌ FAILED - Status: {response.status_code}")
            print(f"   Error: {response.text[:200]}")
            return False
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return False

def main():
    """Trace all revenue calculations"""
    print("🔍 REVENUE CALCULATION AUDIT")
    print("Tracing where dashboard numbers come from...")
    
    # Test each endpoint that contributes to dashboard totals
    endpoints = [
        ("/api/dashboard/financial-summary", "Financial Summary (Event Type Totals)"),
        ("/api/dashboard/vendor-performance", "Vendor Performance (Top Vendors)"),  
        ("/api/dashboard/revenue-breakdown", "Revenue Breakdown (All Events)"),
        ("/api/dashboard/event-timeline", "Event Timeline (Individual Events)")
    ]
    
    results = {}
    for endpoint, name in endpoints:
        success = test_endpoint_details(endpoint, name)
        results[name] = success
    
    print(f"\n{'='*60}")
    print("🎯 AUDIT SUMMARY")
    print(f"{'='*60}")
    
    for name, success in results.items():
        status = "✅ WORKING" if success else "❌ FAILED"
        print(f"{status} - {name}")
    
    # Analysis of dashboard discrepancies
    print(f"\n{'='*60}")
    print("🚨 DASHBOARD DISCREPANCY ANALYSIS")
    print(f"{'='*60}")
    print("Based on your observations:")
    print("• Total Revenue: $27,867.99 (you say this looks correct)")
    print("• Active Vendors: 25 (should be total unique vendors)")
    print("• Top Vendor Revenue: $5,593 (you say this is just one vendor, not total)")
    print("")
    print("ISSUES TO INVESTIGATE:")
    print("1. Where does $27,867.99 come from? Which tables/fields?")
    print("2. Are we counting unique vendors or total vendor entries?")
    print("3. Is $5,593 the total for one vendor across all events, or just one event?")
    print("4. Are we missing revenue from some tables/events?")

if __name__ == "__main__":
    main()