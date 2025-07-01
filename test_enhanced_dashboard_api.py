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
    try:
        print(f"\n🔍 Testing: {description}")
        print(f"📍 Endpoint: {endpoint}")
        print("-" * 50)
        
        response = requests.get(f"{BASE_URL}{endpoint}", timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            print(f"✅ Status: SUCCESS ({response.status_code})")
            
            # Show key data structure
            if 'data' in data:
                if endpoint == '/api/dashboard/attendee-analytics':
                    # RSVP data structure
                    rsvp_data = data['data']
                    if 'rsvp_summary' in rsvp_data:
                        summary = rsvp_data['rsvp_summary']
                        print(f"📊 Total Registrations: {summary.get('total_registrations')}")
                        print(f"📈 Attendance Rate: {summary.get('attendance_rate')}")
                        print(f"🏆 Events Count: {summary.get('events_count')}")
                    
                    if 'geographic_breakdown' in rsvp_data:
                        geo_data = rsvp_data['geographic_breakdown'][:3]
                        print(f"🌍 Geographic Data (top 3):")
                        for city in geo_data:
                            print(f"   • {city['city']}: {city['count']} attendees")
                
                elif endpoint == '/api/dashboard/vendor-performance':
                    # Top vendors structure  
                    vendors = data['data'][:3]
                    print(f"🏆 Top Vendors:")
                    for i, vendor in enumerate(vendors, 1):
                        revenue = vendor.get('revenue', vendor.get('total_sales', 0))
                        print(f"   #{i} {vendor.get('vendor_name')}: ${revenue}")
                
                elif endpoint == '/api/dashboard/financial-summary':
                    # Financial summary by event type
                    summary = data['data']
                    print(f"💰 Financial Summary:")
                    for event_type in summary:
                        print(f"   • {event_type.get('event_type')}: ${event_type.get('total_revenue')} ({event_type.get('vendor_count')} vendors)")
                
                elif endpoint == '/api/dashboard/cumulative-sales':
                    # Cumulative sales data
                    sales_data = data['data']
                    summary = data.get('summary', {})
                    print(f"📈 Cumulative Sales:")
                    print(f"   • Total Events: {summary.get('total_events')}")
                    print(f"   • Total Revenue: ${summary.get('total_revenue')}")
                    if sales_data:
                        print(f"   • Latest Event: {sales_data[-1].get('event_name')} (${sales_data[-1].get('cumulative_revenue')})")
                
                print(f"📄 Data Source: {data.get('data_source', 'Unknown')}")
            
            return True
            
        else:
            print(f"❌ Status: FAILED ({response.status_code})")
            try:
                error_data = response.json()
                print(f"Error: {error_data.get('error', 'Unknown error')}")
            except:
                print(f"Error: {response.text}")
            return False
            
    except requests.exceptions.Timeout:
        print(f"⏱️ Status: TIMEOUT (30s)")
        return False
    except Exception as e:
        print(f"❌ Status: ERROR - {str(e)}")
        return False

def main():
    """Test all enhanced dashboard endpoints"""
    print("🚀 Enhanced Dashboard API Testing Started")
    print("Testing dashboard endpoints for frontend compatibility")
    print("=" * 60)
    
    endpoints = [
        ("/api/dashboard/financial-summary", "Financial Summary by Event Type"),
        ("/api/dashboard/vendor-performance", "Top Vendors Performance (Sorted)"),
        ("/api/dashboard/attendee-analytics", "RSVP & Geographic Analytics"),
        ("/api/dashboard/cumulative-sales", "Cumulative Sales Over Time"),
        ("/api/dashboard/event-timeline", "Event Timeline"),
        ("/api/dashboard/revenue-breakdown", "Revenue Breakdown by Event")
    ]
    
    results = []
    for endpoint, description in endpoints:
        success = test_endpoint(endpoint, description)
        results.append((description, success))
    
    print("\n" + "=" * 60)
    print("🎯 ENHANCED DASHBOARD API TEST RESULTS")
    print("=" * 60)
    
    for description, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} - {description}")
    
    success_count = sum(1 for _, success in results if success)
    total_count = len(results)
    
    print(f"\nOverall: {success_count}/{total_count} endpoints working")
    
    if success_count == total_count:
        print("🎉 All enhanced dashboard endpoints operational!")
        print("👉 Frontend can now connect to enhanced dashboard API")
        print("📊 Dashboard should show: Total RSVPs, Geographic data, Top vendors, Cumulative sales")
    else:
        print("⚠️  Some endpoints need attention")

    # Test specific dashboard requirements
    print("\n" + "=" * 60)
    print("🔍 DASHBOARD REQUIREMENTS VALIDATION")
    print("=" * 60)
    
    print("Testing specific dashboard components:")
    
    # Test RSVP data
    try:
        response = requests.get(f"{BASE_URL}/api/dashboard/attendee-analytics", timeout=15)
        if response.status_code == 200:
            data = response.json()
            rsvp_data = data.get('data', {}).get('rsvp_summary', {})
            if rsvp_data.get('total_registrations'):
                print("✅ Total RSVPs data available")
            else:
                print("❌ Total RSVPs data missing")
        else:
            print("❌ RSVP endpoint failed")
    except:
        print("❌ RSVP endpoint error")
    
    # Test geographic data
    try:
        response = requests.get(f"{BASE_URL}/api/dashboard/attendee-analytics", timeout=15)
        if response.status_code == 200:
            data = response.json()
            geo_data = data.get('data', {}).get('geographic_breakdown', [])
            if len(geo_data) >= 6:
                print("✅ Geographic breakdown data available (6+ cities)")
            else:
                print(f"⚠️  Geographic data limited ({len(geo_data)} cities)")
        else:
            print("❌ Geographic endpoint failed")
    except:
        print("❌ Geographic endpoint error")

    # Test cumulative sales
    try:
        response = requests.get(f"{BASE_URL}/api/dashboard/cumulative-sales", timeout=15)
        if response.status_code == 200:
            data = response.json()
            sales_data = data.get('data', [])
            if len(sales_data) > 0:
                print("✅ Cumulative sales data available")
            else:
                print("❌ Cumulative sales data empty")
        else:
            print("❌ Cumulative sales endpoint failed")
    except:
        print("❌ Cumulative sales endpoint error")

if __name__ == "__main__":
    main()