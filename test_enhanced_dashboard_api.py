#!/usr/bin/env python3
"""
Test Enhanced Dashboard API Endpoints
Validates the enhanced dashboard endpoints match frontend requirements
"""

import requests
import json

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_endpoint(endpoint, description):
    """Test a specific endpoint and show response structure"""
    print(f"\n{'='*80}")
    print(f"🔍 TESTING: {description}")
    print(f"📡 ENDPOINT: {endpoint}")
    print(f"{'='*80}")
    
    try:
        response = requests.get(f"{BASE_URL}{endpoint}", timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            print(f"✅ SUCCESS - Status: {response.status_code}")
            print(f"📊 Data Source: {data.get('source', 'Unknown')}")
            print(f"📝 Note: {data.get('note', 'N/A')}")
            
            if endpoint == "/api/dashboard/financial-summary":
                summary = data.get('data', [])
                total_calculated = 0
                print(f"\n💰 FINANCIAL SUMMARY:")
                for item in summary:
                    revenue = item.get('total_revenue', 0)
                    vendors = item.get('vendor_count', 0)
                    event_type = item.get('event_type', 'Unknown')
                    total_calculated += revenue
                    print(f"   • {event_type}: ${revenue:,.2f} ({vendors} vendors)")
                print(f"   • TOTAL REVENUE: ${total_calculated:,.2f}")
                
            elif endpoint == "/api/dashboard/vendor-performance":
                vendors = data.get('data', [])
                print(f"\n🏆 TOP VENDORS (Showing {len(vendors)} vendors):")
                total_unique_vendors = len(set(v.get('vendor_name') for v in vendors))
                print(f"   • Unique Vendors: {total_unique_vendors}")
                
                for i, vendor in enumerate(vendors[:5], 1):
                    name = vendor.get('vendor_name', 'Unknown')
                    revenue = vendor.get('total_sales', vendor.get('revenue', 0))
                    events = vendor.get('event_count', 'N/A')
                    aggregated = vendor.get('aggregated', False)
                    
                    if isinstance(revenue, str):
                        revenue = float(revenue)
                    
                    status = " [AGGREGATED]" if aggregated else " [SINGLE EVENT]"
                    print(f"   #{i}: {name} = ${revenue:,.2f} ({events} events){status}")
                
                # Show total of top vendor (should be aggregated across all events)
                if vendors:
                    top_vendor = vendors[0]
                    top_revenue = top_vendor.get('total_sales', top_vendor.get('revenue', 0))
                    if isinstance(top_revenue, str):
                        top_revenue = float(top_revenue)
                    print(f"\n🥇 TOP VENDOR TOTAL: ${top_revenue:,.2f}")
                    
            elif endpoint == "/api/dashboard/revenue-breakdown":
                breakdown = data.get('data', [])
                api_total = data.get('total_revenue', 0)
                print(f"\n📊 REVENUE BREAKDOWN:")
                print(f"   • API Total: ${api_total:,.2f}")
                
                calculated_total = 0
                for item in breakdown[:3]:
                    revenue = item.get('revenue', 0)
                    calculated_total += revenue
                    print(f"   • {item.get('event_name', 'Unknown')}: ${revenue:,.2f}")
                
                print(f"   • CALCULATED TOTAL: ${calculated_total:,.2f}")
                print(f"   • MATCH: {'✅' if abs(api_total - calculated_total) < 0.01 else '❌'}")
                
            return data
            
        else:
            print(f"❌ FAILED - Status: {response.status_code}")
            print(f"   Response: {response.text[:200]}")
            return None
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return None

def main():
    """Test all enhanced dashboard endpoints"""
    print("🚀 ENHANCED DASHBOARD API TESTING")
    print("Testing corrected revenue calculations...")
    
    # Test each endpoint
    endpoints = [
        ("/api/dashboard/financial-summary", "Financial Summary (Total Revenue by Event Type)"),
        ("/api/dashboard/vendor-performance", "Vendor Performance (Aggregated Vendor Totals)"),
        ("/api/dashboard/revenue-breakdown", "Revenue Breakdown (Should Match Financial Summary)")
    ]
    
    results = {}
    for endpoint, description in endpoints:
        result = test_endpoint(endpoint, description)
        results[endpoint] = result
    
    # Cross-validation
    print(f"\n{'='*80}")
    print("🔍 CROSS-VALIDATION ANALYSIS")
    print(f"{'='*80}")
    
    # Compare totals between endpoints
    financial_total = 0
    if results.get("/api/dashboard/financial-summary"):
        for item in results["/api/dashboard/financial-summary"].get('data', []):
            financial_total += item.get('total_revenue', 0)
    
    breakdown_total = 0
    if results.get("/api/dashboard/revenue-breakdown"):
        breakdown_total = results["/api/dashboard/revenue-breakdown"].get('total_revenue', 0)
    
    print(f"💰 Financial Summary Total: ${financial_total:,.2f}")
    print(f"📊 Revenue Breakdown Total: ${breakdown_total:,.2f}")
    print(f"🎯 Totals Match: {'✅' if abs(financial_total - breakdown_total) < 0.01 else '❌'}")
    
    # Vendor analysis
    if results.get("/api/dashboard/vendor-performance"):
        vendor_data = results["/api/dashboard/vendor-performance"].get('data', [])
        unique_vendors = len(set(v.get('vendor_name') for v in vendor_data))
        top_vendor_revenue = 0
        if vendor_data:
            top_revenue = vendor_data[0].get('total_sales', vendor_data[0].get('revenue', 0))
            if isinstance(top_revenue, str):
                top_revenue = float(top_revenue)
            top_vendor_revenue = top_revenue
        
        print(f"\n🏆 VENDOR METRICS:")
        print(f"   • Total Unique Vendors: {unique_vendors}")
        print(f"   • Top Vendor Revenue: ${top_vendor_revenue:,.2f}")
        print(f"   • Data Source: {results['/api/dashboard/vendor-performance'].get('source', 'Unknown')}")

if __name__ == "__main__":
    main()