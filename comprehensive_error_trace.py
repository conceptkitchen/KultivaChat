#!/usr/bin/env python3
"""
Comprehensive trace to find and eliminate the persistent false error
"""
import requests
import json
import time

def test_all_revenue_scenarios():
    """Test every possible scenario to isolate the false error source"""
    
    base_url = 'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app'
    
    test_cases = [
        # Direct SQL queries - these should work
        ('Direct SQL', 'SELECT COUNT(*) as table_count FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES`'),
        
        # Revenue queries - these are failing
        ('Revenue Query 1', 'Which vendors made over $500?'),
        ('Revenue Query 2', 'Show me revenue analysis'),
        ('Revenue Query 3', 'total sales from Kapwa Gardens'),
        
        # Comprehensive queries - these work
        ('Comprehensive 1', 'Show me comprehensive business intelligence'),
        ('Comprehensive 2', 'Compare all events revenue'),
        
        # Test the /api/sql endpoint directly
        ('Direct SQL Endpoint', None),  # Special case
    ]
    
    results = {}
    
    for test_name, query in test_cases:
        print(f"\n=== {test_name} ===")
        
        if test_name == 'Direct SQL Endpoint':
            # Test /api/sql endpoint directly
            response = requests.post(f'{base_url}/api/sql',
                json={'query': 'SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` LIMIT 3'},
                headers={'Content-Type': 'application/json'})
        else:
            # Test /api/query endpoint
            response = requests.post(f'{base_url}/api/query',
                json={'query': query},
                headers={'Content-Type': 'application/json'})
        
        print(f"Status: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            
            # Check for the specific false error
            if isinstance(data, dict) and 'error' in data and 'No sales tables found for revenue analysis' in data['error']:
                print(f"❌ FALSE ERROR DETECTED: {data['error']}")
                results[test_name] = 'FALSE_ERROR'
            elif isinstance(data, dict) and 'data' in data and data['data']:
                record_count = len(data['data'])
                print(f"✅ SUCCESS: {record_count} records returned")
                results[test_name] = f'SUCCESS_{record_count}'
            elif isinstance(data, dict) and 'tables' in data:
                table_count = len(data['tables'])
                print(f"✅ SUCCESS: {table_count} tables returned")
                results[test_name] = f'SUCCESS_{table_count}'
            elif isinstance(data, dict) and any(key in data for key in ['business_intelligence', 'analysis']):
                print(f"✅ SUCCESS: Business intelligence response")
                results[test_name] = 'SUCCESS_BI'
            else:
                print(f"⚠️ UNEXPECTED: {str(data)[:100]}")
                results[test_name] = 'UNEXPECTED'
        else:
            print(f"❌ HTTP ERROR: {response.status_code}")
            results[test_name] = f'HTTP_ERROR_{response.status_code}'
    
    # Summary analysis
    print(f"\n" + "="*60)
    print("COMPREHENSIVE ERROR TRACE SUMMARY")
    print("="*60)
    
    false_errors = [k for k, v in results.items() if v == 'FALSE_ERROR']
    successes = [k for k, v in results.items() if v.startswith('SUCCESS')]
    
    print(f"FALSE ERRORS ({len(false_errors)}): {false_errors}")
    print(f"SUCCESSES ({len(successes)}): {successes}")
    
    if false_errors:
        print(f"\n🚨 DATA INTEGRITY ISSUE CONFIRMED")
        print(f"The following queries return false 'No sales tables found' errors:")
        for error_case in false_errors:
            print(f"  - {error_case}")
        print(f"\nThis violates the core requirement of providing authentic data analysis.")
        print(f"Root cause: Revenue-related queries are hitting a broken code path.")
    
    if successes:
        print(f"\n✅ WORKING COMPONENTS CONFIRMED")
        print(f"These query types work correctly:")
        for success_case in successes:
            print(f"  - {success_case}")

if __name__ == "__main__":
    test_all_revenue_scenarios()