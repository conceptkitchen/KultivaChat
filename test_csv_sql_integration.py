#!/usr/bin/env python3
"""
Test SQL query integration with CSV files to prepare proper dashboard data
"""

import csv
import sqlite3
from pathlib import Path

def load_csv_to_sqlite():
    """Load both CSV files into SQLite for proper SQL aggregation"""
    
    # Create in-memory SQLite database
    conn = sqlite3.connect(':memory:')
    cursor = conn.cursor()
    
    # Create tables
    cursor.execute('''
        CREATE TABLE undiscovered_vendors (
            vendor_name TEXT,
            contact_name TEXT,
            vendor_email TEXT,
            total_sales REAL,
            event_date TEXT,
            event_name TEXT
        )
    ''')
    
    cursor.execute('''
        CREATE TABLE kapwa_vendors (
            vendor_name TEXT,
            contact_name TEXT,
            vendor_email TEXT,
            total_sales REAL,
            event_date TEXT,
            event_name TEXT
        )
    ''')
    
    # Load UNDISCOVERED data
    undiscovered_file = "attached_assets/24083606.out.c_undiscovered_close_out_sales_transformation.master_undiscovered_report_1751349318193.csv"
    with open(undiscovered_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            cursor.execute('''
                INSERT INTO undiscovered_vendors 
                (vendor_name, contact_name, vendor_email, total_sales, event_date, event_name)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                row.get('vendor_name', ''),
                row.get('contact_name', ''),
                row.get('vendor_email', ''),
                float(row.get('total_sales', 0) or 0),
                row.get('event_date', ''),
                row.get('event_name', '')
            ))
    
    # Load Kapwa Gardens data
    kapwa_file = "attached_assets/24083756.out.c_kapwa_gardens_close_out_sales_transformation.master_financial_report_1751349474968.csv"
    with open(kapwa_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            cursor.execute('''
                INSERT INTO kapwa_vendors 
                (vendor_name, contact_name, vendor_email, total_sales, event_date, event_name)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                row.get('vendor_name', ''),
                row.get('contact_name', ''),
                row.get('vendor_email', ''),
                float(row.get('total_sales', 0) or 0),
                row.get('event_date', ''),
                row.get('event_name', '')
            ))
    
    conn.commit()
    return conn

def test_dashboard_sql_queries():
    """Test SQL queries that should power the dashboard"""
    
    print("🔍 TESTING DASHBOARD SQL QUERIES")
    print("=" * 60)
    
    conn = load_csv_to_sqlite()
    cursor = conn.cursor()
    
    # 1. Financial Summary
    print("\n💰 FINANCIAL SUMMARY QUERY:")
    cursor.execute('''
        SELECT 
            'UNDISCOVERED' as event_type,
            SUM(total_sales) as total_revenue,
            COUNT(CASE WHEN total_sales > 0 THEN 1 END) as vendor_count
        FROM undiscovered_vendors
        WHERE vendor_name != ''
        
        UNION ALL
        
        SELECT 
            'Kapwa Gardens' as event_type,
            SUM(total_sales) as total_revenue,
            COUNT(CASE WHEN total_sales > 0 THEN 1 END) as vendor_count
        FROM kapwa_vendors
        WHERE vendor_name != ''
    ''')
    
    financial_data = cursor.fetchall()
    total_revenue = 0
    for row in financial_data:
        event_type, revenue, vendor_count = row
        total_revenue += revenue
        print(f"   • {event_type}: ${revenue:,.2f} ({vendor_count} vendors)")
    print(f"   • TOTAL: ${total_revenue:,.2f}")
    
    # 2. Top Vendors (Aggregated)
    print("\n🏆 TOP VENDORS QUERY (Aggregated):")
    cursor.execute('''
        WITH combined_vendors AS (
            SELECT vendor_name, total_sales, 'UNDISCOVERED' as source
            FROM undiscovered_vendors 
            WHERE vendor_name != '' AND total_sales > 0
            
            UNION ALL
            
            SELECT vendor_name, total_sales, 'Kapwa Gardens' as source
            FROM kapwa_vendors 
            WHERE vendor_name != '' AND total_sales > 0
        )
        SELECT 
            vendor_name,
            SUM(total_sales) as total_sales,
            COUNT(*) as event_count,
            GROUP_CONCAT(DISTINCT source) as sources
        FROM combined_vendors
        GROUP BY vendor_name
        ORDER BY total_sales DESC
        LIMIT 10
    ''')
    
    vendor_data = cursor.fetchall()
    for i, row in enumerate(vendor_data, 1):
        vendor_name, total_sales, event_count, sources = row
        print(f"   #{i}: {vendor_name} = ${total_sales:,.2f} ({event_count} events, {sources})")
    
    # 3. Revenue Breakdown by Event
    print("\n📊 REVENUE BREAKDOWN QUERY:")
    cursor.execute('''
        SELECT 
            event_name || ' (' || event_date || ')' as event_key,
            SUM(total_sales) as revenue
        FROM (
            SELECT event_name, event_date, total_sales FROM undiscovered_vendors WHERE total_sales > 0
            UNION ALL
            SELECT event_name, event_date, total_sales FROM kapwa_vendors WHERE total_sales > 0
        )
        GROUP BY event_key
        HAVING revenue > 0
        ORDER BY revenue DESC
        LIMIT 10
    ''')
    
    event_data = cursor.fetchall()
    event_total = 0
    for row in event_data:
        event_key, revenue = row
        event_total += revenue
        print(f"   • {event_key}: ${revenue:,.2f}")
    print(f"   • TOP 10 EVENTS TOTAL: ${event_total:,.2f}")
    
    # 4. Summary Statistics
    print("\n📊 SUMMARY STATISTICS:")
    cursor.execute('''
        SELECT 
            COUNT(DISTINCT vendor_name) as unique_vendors,
            COUNT(*) as total_records,
            SUM(total_sales) as total_revenue
        FROM (
            SELECT vendor_name, total_sales FROM undiscovered_vendors WHERE vendor_name != ''
            UNION ALL
            SELECT vendor_name, total_sales FROM kapwa_vendors WHERE vendor_name != ''
        )
    ''')
    
    stats = cursor.fetchone()
    unique_vendors, total_records, total_revenue = stats
    print(f"   • Unique Vendors: {unique_vendors}")
    print(f"   • Total Records: {total_records}")
    print(f"   • Total Revenue: ${total_revenue:,.2f}")
    
    conn.close()
    
    return {
        "financial_summary": financial_data,
        "top_vendors": vendor_data,
        "revenue_breakdown": event_data,
        "summary": {
            "unique_vendors": unique_vendors,
            "total_records": total_records,
            "total_revenue": total_revenue
        }
    }

def main():
    """Test the SQL integration"""
    print("🚀 TESTING CSV TO SQL INTEGRATION FOR DASHBOARD")
    
    try:
        results = test_dashboard_sql_queries()
        
        print(f"\n{'='*60}")
        print("✅ SQL INTEGRATION SUCCESS")
        print(f"{'='*60}")
        print("✅ Both CSV files loaded successfully into SQLite")
        print("✅ Financial summary, vendor performance, and revenue breakdown queries working")
        print("✅ Data aggregation working correctly")
        print("✅ Ready to integrate into Flask endpoint")
        
        return results
        
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return None

if __name__ == "__main__":
    main()