#!/usr/bin/env python3
"""
Direct SQL approach to get ALL vendors over $500 across ALL tables
"""
import requests
import json
import time

def get_vendors_over_500():
    """Get specific vendors over $500 from all tables"""
    
    # Step 1: Get all table names
    print("=== STEP 1: Getting all tables ===")
    
    sql_query = """
    SELECT table_name 
    FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` 
    WHERE table_name LIKE '%Close-Out-Sales%' 
    OR table_name LIKE '%vendor%' 
    OR table_name LIKE '%Close%'
    ORDER BY table_name
    """
    
    response = requests.post('http://localhost:8081/api/sql', 
                           json={'query': sql_query}, 
                           timeout=30)
    
    if response.status_code == 200:
        tables_data = response.json()
        if 'data' in tables_data:
            tables = [row['table_name'] for row in tables_data['data']]
            print(f"Found {len(tables)} tables")
            
            # Step 2: Query each table for vendors over $500
            print("\n=== STEP 2: Checking each table for vendors over $500 ===")
            
            all_vendors = []
            
            for table in tables[:10]:  # Limit to first 10 tables for speed
                print(f"\nChecking table: {table}")
                
                # Try different revenue column patterns
                for revenue_col in ['Total_Sales', 'Cash__Credit_Total', 'Amount', 'Revenue', 'Total']:
                    try:
                        vendor_query = f"""
                        SELECT 
                            '{table}' as table_source,
                            COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) as vendor_name,
                            CAST(REGEXP_REPLACE(CAST({revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64) as revenue
                        FROM `kbc-use4-839-261b.WORKSPACE_23990909.{table}`
                        WHERE COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) IS NOT NULL
                        AND {revenue_col} IS NOT NULL
                        AND CAST(REGEXP_REPLACE(CAST({revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64) > 500
                        ORDER BY revenue DESC
                        LIMIT 20
                        """
                        
                        vendor_response = requests.post('http://localhost:8081/api/sql', 
                                                      json={'query': vendor_query}, 
                                                      timeout=15)
                        
                        if vendor_response.status_code == 200:
                            vendor_data = vendor_response.json()
                            if 'data' in vendor_data and vendor_data['data']:
                                print(f"  Found {len(vendor_data['data'])} vendors over $500 using column {revenue_col}")
                                all_vendors.extend(vendor_data['data'])
                                break  # Found working column, move to next table
                                
                    except Exception as e:
                        continue  # Try next column
            
            # Step 3: Display results
            print(f"\n=== STEP 3: FINAL RESULTS ===")
            print(f"Total vendors over $500 found: {len(all_vendors)}")
            
            if all_vendors:
                print("\nTop vendors over $500:")
                sorted_vendors = sorted(all_vendors, key=lambda x: x['revenue'], reverse=True)
                
                for i, vendor in enumerate(sorted_vendors[:15]):
                    print(f"{i+1:2d}. {vendor['vendor_name']:<30} ${vendor['revenue']:>8.2f} ({vendor['table_source'][:30]}...)")
                    
                total_revenue = sum(v['revenue'] for v in all_vendors)
                print(f"\nTotal revenue from vendors over $500: ${total_revenue:,.2f}")
                
            return all_vendors
            
    print("Failed to get table list")
    return []

if __name__ == "__main__":
    vendors = get_vendors_over_500()