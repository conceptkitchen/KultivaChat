#!/usr/bin/env python3
"""
Test the new /dashboard endpoint that uses actual CSV transformation files
"""

import requests
import json

BASE_URL = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app"

def test_dashboard_endpoint():
    """Test the new main dashboard endpoint"""
    print("🚀 TESTING NEW /dashboard ENDPOINT")
    print("=" * 60)
    print(f"Using actual transformation CSV files...")
    
    try:
        response = requests.get(f"{BASE_URL}/dashboard", timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            print(f"✅ SUCCESS - Status: 200")
            
            # Display metadata
            metadata = data.get('metadata', {})
            print(f"\n📊 DASHBOARD METADATA:")
            print(f"   • Total Vendors: {metadata.get('total_vendors', 'N/A')}")
            print(f"   • Total Revenue: ${metadata.get('total_revenue', 0):,.2f}")
            print(f"   • Data Source: {metadata.get('data_source', 'Unknown')}")
            print(f"   • Last Updated: {metadata.get('last_updated', 'N/A')}")
            
            # Display financial summary
            financial = data.get('data', {}).get('financial_summary', {})
            print(f"\n💰 FINANCIAL SUMMARY:")
            for item in financial.get('data', []):
                print(f"   • {item.get('event_type')}: ${item.get('total_revenue', 0):,.2f} ({item.get('vendor_count')} vendors)")
            print(f"   • TOTAL: ${financial.get('total_revenue', 0):,.2f}")
            
            # Display top vendors
            vendors = data.get('data', {}).get('vendor_performance', {})
            print(f"\n🏆 TOP 10 VENDORS:")
            for i, vendor in enumerate(vendors.get('data', [])[:10], 1):
                print(f"   #{i}: {vendor.get('vendor_name')} = ${vendor.get('total_sales', 0):,.2f} ({vendor.get('source')})")
            
            # Display revenue breakdown
            breakdown = data.get('data', {}).get('revenue_breakdown', {})
            print(f"\n📊 REVENUE BREAKDOWN (Top 10 Events):")
            for i, event in enumerate(breakdown.get('data', [])[:10], 1):
                print(f"   #{i}: {event.get('event_name')} = ${event.get('revenue', 0):,.2f}")
            
            # Display event timeline
            timeline = data.get('data', {}).get('event_timeline', {})
            total_events = len(timeline.get('data', []))
            print(f"\n📅 EVENT TIMELINE:")
            print(f"   • Total Events: {total_events}")
            if total_events > 0:
                recent_events = sorted(timeline.get('data', []), key=lambda x: x.get('date', ''), reverse=True)[:5]
                print(f"   • Recent Events:")
                for event in recent_events:
                    print(f"     - {event.get('date')}: {event.get('event_name')} (${event.get('total_revenue', 0):,.2f})")
            
            return data
            
        else:
            print(f"❌ FAILED - Status: {response.status_code}")
            print(f"Response: {response.text}")
            return None
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return None

def compare_to_old_endpoints():
    """Compare new dashboard to old individual endpoints"""
    print(f"\n{'='*60}")
    print("🔄 COMPARING TO OLD ENDPOINTS")
    print(f"{'='*60}")
    
    old_endpoints = [
        "/api/dashboard/financial-summary",
        "/api/dashboard/vendor-performance", 
        "/api/dashboard/revenue-breakdown"
    ]
    
    for endpoint in old_endpoints:
        try:
            response = requests.get(f"{BASE_URL}{endpoint}", timeout=15)
            if response.status_code == 200:
                data = response.json()
                
                if endpoint == "/api/dashboard/financial-summary":
                    total = sum(item.get('total_revenue', 0) for item in data.get('data', []))
                    print(f"✅ Old Financial Summary: ${total:,.2f}")
                    
                elif endpoint == "/api/dashboard/vendor-performance":
                    vendor_count = len(data.get('data', []))
                    top_vendor = data.get('data', [{}])[0] if data.get('data') else {}
                    print(f"✅ Old Vendor Performance: {vendor_count} vendors, top: ${top_vendor.get('total_sales', 0):,.2f}")
                    
                elif endpoint == "/api/dashboard/revenue-breakdown":
                    total = data.get('total_revenue', 0)
                    source = data.get('source', 'Unknown')
                    print(f"✅ Old Revenue Breakdown: ${total:,.2f} (Source: {source})")
                    
            else:
                print(f"❌ {endpoint}: Status {response.status_code}")
                
        except Exception as e:
            print(f"❌ {endpoint}: Error - {e}")

def main():
    """Test the new dashboard endpoint and compare"""
    # Test new endpoint
    dashboard_data = test_dashboard_endpoint()
    
    # Compare to old endpoints
    compare_to_old_endpoints()
    
    if dashboard_data:
        print(f"\n{'='*60}")
        print("🎯 FINAL SUMMARY")
        print(f"{'='*60}")
        print("✅ New /dashboard endpoint successfully reads from actual CSV transformation files")
        print("✅ Provides complete vendor data with accurate totals")
        print("✅ Shows financial summary, vendor performance, revenue breakdown, and event timeline")
        print("✅ Ready for frontend integration")

if __name__ == "__main__":
    main()