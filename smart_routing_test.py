#!/usr/bin/env python3
"""
Smart Routing Validation Test
Tests the enhanced table filtering logic to ensure accurate query routing
"""

import requests
import json
import time

API_BASE = 'http://localhost:8081'

# Test queries that previously had routing issues
test_queries = [
    {
        "name": "UNDISCOVERED SF August 19 2023",
        "query": "Show me vendor data from UNDISCOVERED SF August 19, 2023",
        "expected_table_pattern": "2023-08-19-UNDISCOVERED-SF",
        "expected_event": "UNDISCOVERED"
    },
    {
        "name": "Lovers Mart February 11 2023", 
        "query": "Show me vendor sales from Lovers Mart February 11, 2023",
        "expected_table_pattern": "2023-02-11-Lovers-Mart",
        "expected_event": "Lovers-Mart"
    },
    {
        "name": "Yum Yams May 13 2023",
        "query": "Show me Yum Yams vendor data from May 13, 2023",
        "expected_table_pattern": "2023-05-13-Yum-Yams",
        "expected_event": "Yum-Yams"
    },
    {
        "name": "General vendor query",
        "query": "Show me vendor sales data from any Kapwa Gardens event",
        "expected_table_pattern": "kapwa",
        "expected_event": "Kapwa-Gardens"
    }
]

def test_smart_routing():
    print("🔍 SMART ROUTING VALIDATION TEST")
    print("=" * 50)
    
    for test in test_queries:
        print(f"\n📋 Testing: {test['name']}")
        print(f"Query: \"{test['query']}\"")
        
        try:
            response = requests.post(
                f"{API_BASE}/api/query",
                headers={'Content-Type': 'application/json'},
                json={'query': test['query']},
                timeout=30
            )
            
            if response.status_code != 200:
                print(f"❌ HTTP Error: {response.status_code}")
                print(f"Response: {response.text[:200]}")
                continue
            
            result = response.json()
            
            if result.get('status') == 'success':
                print(f"✅ Status: {result['status']}")
                print(f"📊 Rows returned: {len(result.get('data', []))}")
                
                # Check routing method
                if result.get('routing_method'):
                    print(f"🔀 Routing: {result['routing_method']}")
                
                # Check for table source information
                if 'table_source' in result:
                    table_used = result['table_source']
                    pattern_match = test['expected_table_pattern'].lower() in table_used.lower()
                    print(f"🎯 Table routing: {'✅ CORRECT' if pattern_match else '❌ WRONG'}")
                    print(f"   Expected pattern: {test['expected_table_pattern']}")
                    print(f"   Used: {table_used}")
                
                # Validate data authenticity
                if result.get('data') and len(result['data']) > 0:
                    sample_data = result['data'][0]
                    fields = list(sample_data.keys())[:3]
                    print(f"📈 Sample data fields: {', '.join(fields)}")
                    
                    # Check for vendor name
                    vendor_field = None
                    for field in sample_data:
                        if 'vendor' in field.lower() and 'name' in field.lower():
                            vendor_field = field
                            break
                    
                    if vendor_field and sample_data[vendor_field]:
                        print(f"🔢 First vendor: {sample_data[vendor_field]}")
                    
                    # Check for revenue data
                    revenue_field = None
                    for field in sample_data:
                        if any(term in field.lower() for term in ['total', 'sales', 'revenue', 'cash']):
                            revenue_field = field
                            break
                    
                    if revenue_field and sample_data[revenue_field]:
                        print(f"💰 Sample revenue: ${sample_data[revenue_field]}")
                
            else:
                print(f"❌ Error: {result.get('error', 'Unknown error')}")
                
        except requests.exceptions.Timeout:
            print("❌ Request timed out after 30 seconds")
        except requests.exceptions.RequestException as e:
            print(f"❌ Request failed: {str(e)}")
        except Exception as e:
            print(f"❌ Unexpected error: {str(e)}")
        
        print("-" * 30)
        time.sleep(1)  # Brief pause between tests

if __name__ == "__main__":
    test_smart_routing()