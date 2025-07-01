#!/usr/bin/env python3
"""
Quick fix for dashboard endpoint - standalone server with working paths
"""

from flask import Flask, jsonify
from flask_cors import CORS
import csv
import sqlite3
import os
from datetime import datetime

app = Flask(__name__)
CORS(app)

def load_transformation_data_with_sql():
    """Load and process transformation data using SQL for accurate aggregation"""
    try:
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
        
    except Exception as e:
        print(f"Error loading transformation data: {e}")
        return None

def calculate_financial_summary_sql(cursor):
    """Calculate financial summary from transformation data using SQL"""
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
    
    undiscovered_revenue = 0
    undiscovered_vendors = 0
    kapwa_revenue = 0
    kapwa_vendors = 0
    
    for row in financial_data:
        event_type, revenue, vendor_count = row
        if event_type == 'UNDISCOVERED':
            undiscovered_revenue = revenue
            undiscovered_vendors = vendor_count
        else:
            kapwa_revenue = revenue
            kapwa_vendors = vendor_count
    
    total_revenue = undiscovered_revenue + kapwa_revenue
    total_vendors = undiscovered_vendors + kapwa_vendors
    
    return {
        "total_revenue": total_revenue,
        "total_vendors": total_vendors,
        "undiscovered_revenue": undiscovered_revenue,
        "undiscovered_vendors": undiscovered_vendors,
        "kapwa_revenue": kapwa_revenue,
        "kapwa_vendors": kapwa_vendors,
    }

def calculate_vendor_performance_sql(cursor):
    """Calculate top vendor performance from transformation data using SQL"""
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
        LIMIT 20
    ''')
    
    vendor_data = cursor.fetchall()
    vendor_list = []
    
    for row in vendor_data:
        vendor_name, total_sales, event_count, sources = row
        vendor_list.append({
            "vendor_name": vendor_name,
            "total_sales": total_sales,
            "event_count": event_count,
            "sources": sources,
            "rank": len(vendor_list) + 1
        })
    
    return {"data": vendor_list}

@app.route('/dashboard', methods=['GET'])
def main_dashboard():
    """Main dashboard endpoint using actual vendor transformation data"""
    try:
        print("Dashboard request received - using actual transformation CSV files with SQL")
        
        # Load data using SQL for accurate aggregation
        conn = load_transformation_data_with_sql()
        if not conn:
            return jsonify({"error": "Unable to load transformation data"}), 500
        
        cursor = conn.cursor()
        
        # Calculate dashboard components
        financial_summary = calculate_financial_summary_sql(cursor)
        vendor_performance = calculate_vendor_performance_sql(cursor)
        
        conn.close()
        
        return jsonify({
            "status": "success",
            "data": {
                "financial_summary": financial_summary,
                "vendor_performance": vendor_performance,
            },
            "metadata": {
                "total_vendors": financial_summary["total_vendors"],
                "total_revenue": financial_summary["total_revenue"],
                "undiscovered_revenue": financial_summary["undiscovered_revenue"],
                "kapwa_revenue": financial_summary["kapwa_revenue"],
                "data_source": "Transformation CSV Files (SQL Processed)",
                "last_updated": datetime.now().isoformat(),
            }
        })
        
    except Exception as e:
        print(f"Dashboard error: {e}")
        return jsonify({"error": f"Dashboard error: {str(e)}"}), 500

@app.route('/', methods=['GET'])
def root():
    """Root endpoint"""
    return jsonify({
        "message": "Kultivate AI Dashboard API",
        "endpoints": ["/dashboard"],
        "status": "operational"
    })

if __name__ == '__main__':
    print("🚀 Starting Dashboard API with correct data...")
    app.run(host='0.0.0.0', port=8082, debug=False)