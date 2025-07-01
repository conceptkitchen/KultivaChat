#!/usr/bin/env python3
"""
Analyze the actual vendor data from the attached CSV files to see the true scope
"""

import csv
import os

def analyze_undiscovered_data():
    """Analyze the UNDISCOVERED vendor data"""
    print("🔍 ANALYZING UNDISCOVERED DATA")
    print("=" * 60)
    
    file_path = "attached_assets/24083606.out.c_undiscovered_close_out_sales_transformation.master_undiscovered_report_1751349318193.csv"
    
    vendors = []
    total_revenue = 0
    events = set()
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                vendor_name = row.get('vendor_name', '').strip()
                total_sales = float(row.get('total_sales', 0) or 0)
                event_name = row.get('event_name', '').strip()
                event_date = row.get('event_date', '').strip()
                
                if vendor_name:
                    vendors.append({
                        'name': vendor_name,
                        'sales': total_sales,
                        'event': event_name,
                        'date': event_date
                    })
                    total_revenue += total_sales
                    events.add(f"{event_name} ({event_date})")
        
        print(f"📊 UNDISCOVERED TOTALS:")
        print(f"   • Total Vendors: {len(vendors)}")
        print(f"   • Total Revenue: ${total_revenue:,.2f}")
        print(f"   • Events: {len(events)}")
        
        print(f"\n🏆 TOP 10 UNDISCOVERED VENDORS:")
        sorted_vendors = sorted(vendors, key=lambda x: x['sales'], reverse=True)
        for i, vendor in enumerate(sorted_vendors[:10], 1):
            print(f"   #{i}: {vendor['name']} = ${vendor['sales']:,.2f} ({vendor['event']})")
        
        print(f"\n📅 EVENTS:")
        for event in sorted(events):
            print(f"   • {event}")
            
        return vendors, total_revenue
        
    except Exception as e:
        print(f"❌ Error reading UNDISCOVERED data: {e}")
        return [], 0

def analyze_kapwa_gardens_data():
    """Analyze the Kapwa Gardens vendor data"""
    print("\n🔍 ANALYZING KAPWA GARDENS DATA")
    print("=" * 60)
    
    file_path = "attached_assets/24083756.out.c_kapwa_gardens_close_out_sales_transformation.master_financial_report_1751349474968.csv"
    
    vendors = []
    total_revenue = 0
    events = set()
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                vendor_name = row.get('vendor_name', '').strip()
                total_sales = float(row.get('total_sales', 0) or 0)
                event_name = row.get('event_name', '').strip()
                event_date = row.get('event_date', '').strip()
                
                if vendor_name:
                    vendors.append({
                        'name': vendor_name,
                        'sales': total_sales,
                        'event': event_name,
                        'date': event_date
                    })
                    total_revenue += total_sales
                    events.add(f"{event_name} ({event_date})")
        
        print(f"📊 KAPWA GARDENS TOTALS:")
        print(f"   • Total Vendors: {len(vendors)}")
        print(f"   • Total Revenue: ${total_revenue:,.2f}")
        print(f"   • Events: {len(events)}")
        
        print(f"\n🏆 TOP 10 KAPWA GARDENS VENDORS:")
        sorted_vendors = sorted(vendors, key=lambda x: x['sales'], reverse=True)
        for i, vendor in enumerate(sorted_vendors[:10], 1):
            print(f"   #{i}: {vendor['name']} = ${vendor['sales']:,.2f} ({vendor['event']})")
        
        print(f"\n📅 EVENTS:")
        for event in sorted(events):
            print(f"   • {event}")
            
        return vendors, total_revenue
        
    except Exception as e:
        print(f"❌ Error reading Kapwa Gardens data: {e}")
        return [], 0

def main():
    """Analyze both datasets and compare to current API"""
    print("🚀 ANALYZING ACTUAL VENDOR DATA FROM CSV ATTACHMENTS")
    print("Comparing against current API fallback data...\n")
    
    # Analyze both datasets
    undiscovered_vendors, undiscovered_revenue = analyze_undiscovered_data()
    kapwa_vendors, kapwa_revenue = analyze_kapwa_gardens_data()
    
    # Combined totals
    total_vendors = len(undiscovered_vendors) + len(kapwa_vendors)
    total_revenue = undiscovered_revenue + kapwa_revenue
    
    print(f"\n{'='*60}")
    print("🎯 COMBINED ANALYSIS")
    print(f"{'='*60}")
    print(f"📊 ACTUAL DATA TOTALS:")
    print(f"   • Total Vendors: {total_vendors}")
    print(f"   • Total Revenue: ${total_revenue:,.2f}")
    print(f"   • UNDISCOVERED: {len(undiscovered_vendors)} vendors, ${undiscovered_revenue:,.2f}")
    print(f"   • Kapwa Gardens: {len(kapwa_vendors)} vendors, ${kapwa_revenue:,.2f}")
    
    print(f"\n🚨 COMPARISON TO CURRENT API:")
    print(f"   • Current API shows: 25 vendors, $27,867.99")
    print(f"   • Actual data shows: {total_vendors} vendors, ${total_revenue:,.2f}")
    print(f"   • Missing vendors: {total_vendors - 25}")
    print(f"   • Missing revenue: ${total_revenue - 27867.99:,.2f}")
    
    # Show unique vendor names across both datasets
    all_vendor_names = set()
    for vendor in undiscovered_vendors + kapwa_vendors:
        if vendor['sales'] > 0:  # Only include vendors with sales
            all_vendor_names.add(vendor['name'])
    
    print(f"\n📝 UNIQUE VENDORS WITH SALES:")
    print(f"   • {len(all_vendor_names)} unique vendor names")
    print(f"   • First 20: {sorted(list(all_vendor_names))[:20]}")

if __name__ == "__main__":
    main()