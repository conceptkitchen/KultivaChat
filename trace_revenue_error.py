#!/usr/bin/env python3
"""
Trace the exact source of the "No sales tables found" error
"""
import requests
import json

def trace_revenue_error():
    """Trace where the false error is coming from"""
    
    # First, verify tables exist with direct SQL
    print("=== Step 1: Verify tables exist ===")
    table_query = 'SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE \'%kapwa%\' OR LOWER(table_name) LIKE \'%sales%\' ORDER BY table_name'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': table_query}, 
        headers={'Content-Type': 'application/json'})
    
    if response.status_code == 200:
        data = response.json()
        if 'data' in data and data['data']:
            tables = [row['table_name'] if isinstance(row, dict) else row[0] for row in data['data']]
            print(f"✅ Found {len(tables)} matching tables:")
            for table in tables[:10]:
                print(f"  - {table}")
        else:
            print(f"❌ No tables found in SQL response: {data}")
    else:
        print(f"❌ SQL query failed: {response.status_code}")
    
    # Test revenue analysis routing
    print(f"\n=== Step 2: Test revenue analysis routing ===")
    revenue_query = 'Which Kapwa Gardens vendors made over $500?'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': revenue_query}, 
        headers={'Content-Type': 'application/json'})
    
    print(f"Status: {response.status_code}")
    if response.status_code == 200:
        data = response.json()
        print(f"Response: {json.dumps(data, indent=2)}")
        
        # Check if it's the old error
        if 'error' in data and 'No sales tables found' in data['error']:
            print(f"❌ FALSE ERROR DETECTED: {data['error']}")
            print("This confirms data integrity issue - tables exist but API claims they don't")
        elif 'data' in data and data['data']:
            print(f"✅ Success: Found {len(data['data'])} records")
        else:
            print(f"⚠️ Unexpected response format")
    else:
        print(f"❌ Request failed: {response.text}")

    # Test direct table analysis
    print(f"\n=== Step 3: Test comprehensive analysis ===")
    comprehensive_query = 'Show me comprehensive Kapwa Gardens business intelligence'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': comprehensive_query}, 
        headers={'Content-Type': 'application/json'})
    
    if response.status_code == 200:
        data = response.json()
        if 'tables' in data:
            print(f"✅ Comprehensive analysis works: {len(data['tables'])} tables")
        elif 'data' in data and data['data']:
            print(f"✅ Comprehensive analysis works: {len(data['data'])} records")
        elif 'error' in data:
            print(f"❌ Comprehensive analysis failed: {data['error']}")
        else:
            print(f"⚠️ Unexpected comprehensive response: {str(data)[:200]}")
    else:
        print(f"❌ Comprehensive query failed: {response.status_code}")

if __name__ == "__main__":
    trace_revenue_error()