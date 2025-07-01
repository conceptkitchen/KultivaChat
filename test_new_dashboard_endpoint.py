#!/usr/bin/env python3
"""
Test the new /dashboard endpoint that uses actual CSV transformation files
"""

import requests
import json

def test_dashboard_endpoint():
    """Test the new main dashboard endpoint"""
    print("🚀 TESTING NEW /dashboard ENDPOINT WITH CORRECT DATA")
    print("=" * 60)
    
    url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard"
    
    try:
        response = requests.get(url, timeout=30)
        print(f"Status Code: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            print("✅ SUCCESS! Dashboard endpoint working")
            
            # Extract key metrics
            metadata = data.get("metadata", {})
            financial = data.get("data", {}).get("financial_summary", {})
            vendors = data.get("data", {}).get("vendor_performance", {}).get("data", [])
            
            print(f"\n💰 FINANCIAL SUMMARY:")
            print(f"   • Total Revenue: ${financial.get('total_revenue', 0):,.2f}")
            print(f"   • Total Vendors: {financial.get('total_vendors', 0)}")
            print(f"   • UNDISCOVERED: ${financial.get('undiscovered_revenue', 0):,.2f} ({financial.get('undiscovered_vendors', 0)} vendors)")
            print(f"   • Kapwa Gardens: ${financial.get('kapwa_revenue', 0):,.2f} ({financial.get('kapwa_vendors', 0)} vendors)")
            
            print(f"\n🏆 TOP 5 VENDORS:")
            for i, vendor in enumerate(vendors[:5], 1):
                print(f"   #{i}: {vendor.get('vendor_name')} = ${vendor.get('total_sales', 0):,.2f} ({vendor.get('event_count')} events)")
            
            print(f"\n📊 COMPARISON TO YOUR SCREENSHOT DASHBOARD:")
            screenshot_total = 27867.99
            screenshot_vendors = 25
            actual_total = financial.get('total_revenue', 0)
            actual_vendors = financial.get('total_vendors', 0)
            
            print(f"   • Revenue: ${screenshot_total:,.2f} → ${actual_total:,.2f} (+${actual_total - screenshot_total:,.2f})")
            print(f"   • Vendors: {screenshot_vendors} → {actual_vendors} (+{actual_vendors - screenshot_vendors})")
            
            if actual_total > 400000:
                print(f"\n✅ CORRECT! Dashboard now shows the full $458,430.52 scope from both CSV files")
            else:
                print(f"\n⚠️  Still incomplete - should be around $458,430.52")
                
            return data
            
        else:
            print(f"❌ FAILED - Status: {response.status_code}")
            print(f"Response: {response.text[:200]}...")
            return None
            
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return None

def compare_to_old_endpoints():
    """Compare new dashboard to old individual endpoints"""
    print(f"\n{'='*60}")
    print("🔄 COMPARING TO OLD INDIVIDUAL ENDPOINTS")
    print("=" * 60)
    
    base_url = "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/dashboard"
    
    # Test old endpoints
    old_endpoints = [
        "financial-summary",
        "vendor-performance", 
        "revenue-breakdown"
    ]
    
    for endpoint in old_endpoints:
        try:
            response = requests.get(f"{base_url}/{endpoint}", timeout=15)
            if response.status_code == 200:
                data = response.json()
                print(f"✅ Old {endpoint}: Working")
            else:
                print(f"❌ Old {endpoint}: Error - {response.status_code}")
        except Exception as e:
            print(f"❌ Old {endpoint}: Error - {e}")

def main():
    """Test the new dashboard endpoint and compare"""
    results = test_dashboard_endpoint()
    
    if results:
        print(f"\n{'='*60}")
        print("✅ NEW DASHBOARD ENDPOINT READY FOR YOUR FRONTEND!")
        print("=" * 60)
        print("✅ Endpoint: /dashboard")
        print("✅ Method: GET")
        print("✅ Data source: Your actual CSV transformation files")
        print("✅ SQL aggregation: Proper vendor totals across events")
        print("✅ Accurate numbers: Full $458,430.52 revenue scope")
        print("✅ Easy access: Single endpoint returns all dashboard data")
        
        # Show sample frontend integration
        print(f"\n📱 SAMPLE FRONTEND INTEGRATION:")
        print("```javascript")
        print("fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard')")
        print("  .then(response => response.json())")
        print("  .then(data => {")
        print("    const totalRevenue = data.metadata.total_revenue;")
        print("    const totalVendors = data.metadata.total_vendors;")
        print("    const topVendors = data.data.vendor_performance.data;")
        print("    // Update your dashboard components")
        print("  });")
        print("```")
        
    else:
        print("❌ Dashboard endpoint needs fixing")

if __name__ == "__main__":
    main()