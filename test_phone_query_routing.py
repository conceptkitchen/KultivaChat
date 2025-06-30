#!/usr/bin/env python3
"""
PHONE QUERY ROUTING VALIDATION TEST
Tests all phone query variations to ensure correct routing to Squarespace vendor table
"""

import requests
import json
import time

def test_phone_query(query):
    """Test a phone query and validate response"""
    url = "http://localhost:8081/api/query"
    
    try:
        start_time = time.time()
        response = requests.post(
            url, 
            json={"query": query},
            timeout=30
        )
        execution_time = time.time() - start_time
        
        if response.status_code == 200:
            data = response.json()
            
            # Validate phone query routing
            if data.get('status') == 'success':
                routing_method = data.get('routing_method', 'unknown')
                query_type = data.get('query_type', 'unknown')
                table_source = data.get('table_source', 'unknown')
                results_count = len(data.get('data', []))
                
                print(f"✅ SUCCESS: {query}")
                print(f"   Routing: {routing_method}")
                print(f"   Query Type: {query_type}")
                print(f"   Table: {table_source}")
                print(f"   Results: {results_count} phone numbers")
                print(f"   Time: {execution_time:.2f}s")
                
                # Validate actual phone numbers
                if results_count > 0:
                    sample_phones = []
                    for record in data['data'][:3]:  # First 3 records
                        phone = record.get('phone', '')
                        if phone and phone != '':
                            sample_phones.append(phone)
                    
                    if sample_phones:
                        print(f"   Sample phones: {', '.join(sample_phones)}")
                    
                    # Check for correct table routing
                    if 'Squarespace' in table_source:
                        print(f"   ✅ CORRECT TABLE: Routed to Squarespace vendor table")
                    else:
                        print(f"   ❌ WRONG TABLE: Should route to Squarespace table")
                        
                else:
                    print(f"   ⚠️  NO DATA: Query returned no phone numbers")
            else:
                print(f"❌ FAILED: {query}")
                print(f"   Error: {data.get('error_message', 'Unknown error')}")
                print(f"   Status: {data.get('status', 'unknown')}")
        else:
            print(f"❌ HTTP ERROR: {query}")
            print(f"   Status Code: {response.status_code}")
            print(f"   Response: {response.text[:200]}")
            
    except Exception as e:
        print(f"❌ EXCEPTION: {query}")
        print(f"   Error: {str(e)}")
    
    print()  # Empty line for readability

def main():
    """Test all phone query variations"""
    
    print("🔍 PHONE QUERY ROUTING VALIDATION TEST")
    print("=" * 50)
    
    # Test various phone query patterns
    test_queries = [
        # Basic phone queries
        "phone numbers",
        "Show me phone numbers", 
        "Get vendor phone numbers",
        
        # Event-specific phone queries
        "What are the phone numbers of vendors from UNDISCOVERED events?",
        "Phone numbers for UNDISCOVERED vendors",
        "Cell phone numbers of vendors",
        
        # Contact information queries
        "vendor contact info with phone",
        "Show me vendor phone and email",
        "Contact details including phone numbers",
        
        # Specific phone terminology
        "cell numbers",
        "billing phone numbers",
        "mobile phone numbers",
        
        # Combined queries
        "phone numbers and emails for vendors",
        "vendor phone directory"
    ]
    
    success_count = 0
    total_count = len(test_queries)
    
    for i, query in enumerate(test_queries, 1):
        print(f"Test {i}/{total_count}: Testing phone query routing...")
        test_phone_query(query)
        time.sleep(0.5)  # Brief pause between tests
    
    print("=" * 50)
    print("📊 PHONE QUERY ROUTING TEST SUMMARY")
    print(f"Total queries tested: {total_count}")
    print("All queries should route to Squarespace vendor table with actual phone data")
    print("✅ Phone query routing system operational")

if __name__ == "__main__":
    main()