#!/usr/bin/env python3
"""
Test direct SQL bypass to confirm routing fix
"""
import requests
import json

def test_direct_sql():
    """Test that direct SQL queries bypass problematic routing"""
    
    # Test 1: Direct SQL query with SELECT
    print("=== Test 1: Direct SQL with SELECT ===")
    sql_query = 'SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` LIMIT 5'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': sql_query}, 
        headers={'Content-Type': 'application/json'})
    
    print(f"Status: {response.status_code}")
    if response.status_code == 200:
        data = response.json()
        if 'data' in data and data['data']:
            print(f"✅ SUCCESS: Found {len(data['data'])} tables")
            for row in data['data'][:3]:
                table_name = row['table_name'] if isinstance(row, dict) else row[0]
                print(f"  - {table_name}")
        elif 'error' in data:
            print(f"❌ ERROR: {data['error']}")
        else:
            print(f"⚠️ Unexpected response: {data}")
    else:
        print(f"❌ Request failed: {response.status_code}")
    
    # Test 2: Direct SQL using backticks (should trigger SQL detection)
    print(f"\n=== Test 2: Query with backticks ===")
    backtick_query = '`kbc-use4-839-261b.WORKSPACE_23990909.*` table count'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': backtick_query}, 
        headers={'Content-Type': 'application/json'})
    
    if response.status_code == 200:
        data = response.json()
        if 'error' in data and 'No sales tables found' in data['error']:
            print(f"❌ Still routing to problematic function: {data['error']}")
        else:
            print(f"✅ Backtick detection working")
    
    # Test 3: Natural language revenue query (should work now)
    print(f"\n=== Test 3: Natural language revenue query ===")
    revenue_query = 'Which vendors made over $500?'
    
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': revenue_query}, 
        headers={'Content-Type': 'application/json'})
    
    if response.status_code == 200:
        data = response.json()
        if 'error' in data and 'No sales tables found' in data['error']:
            print(f"❌ Revenue analysis still broken: {data['error']}")
        elif 'data' in data or 'business_intelligence' in data:
            print(f"✅ Revenue analysis working")
        else:
            print(f"⚠️ Unexpected revenue response: {str(data)[:100]}")

if __name__ == "__main__":
    test_direct_sql()