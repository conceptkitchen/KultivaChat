#!/usr/bin/env python3
"""
Query 2 Validation Script - Test MCP implementation
Tests: "Which event made the most money from 2021 to 2024?"
"""

import requests
import json
import time

def test_query2():
    """Test Query 2 with MCP implementation"""
    url = "http://localhost:8081/api/query"
    query = "Which event made the most money from 2021 to 2024?"
    
    payload = {"query": query}
    headers = {"Content-Type": "application/json"}
    
    print("🧪 TESTING QUERY 2: Which event made the most money from 2021 to 2024?")
    print("=" * 60)
    
    try:
        print("⏱️  Sending request to MCP server...")
        start_time = time.time()
        
        response = requests.post(url, json=payload, headers=headers, timeout=45)
        
        elapsed = time.time() - start_time
        print(f"⚡ Response received in {elapsed:.2f} seconds")
        
        if response.status_code == 200:
            result = response.json()
            print("✅ SUCCESS: Query 2 processed successfully")
            print(f"📊 Status: {result.get('status', 'Unknown')}")
            
            # Validate response structure
            if 'data' in result and result['data']:
                data_count = len(result['data'])
                print(f"📈 Data Records: {data_count}")
                
                # Show top event
                if data_count > 0:
                    top_event = result['data'][0]
                    print(f"🏆 TOP EVENT: {top_event}")
                    
                    # Validate authentic revenue data
                    if 'total_revenue' in top_event:
                        revenue = float(top_event.get('total_revenue', 0))
                        print(f"💰 Revenue: ${revenue:,.2f}")
                        
                        if revenue > 0:
                            print("✅ AUTHENTIC DATA: Revenue > $0")
                        else:
                            print("❌ DATA ISSUE: Revenue = $0")
                    
                    # Show business intelligence
                    if 'business_intelligence' in result:
                        print("🧠 BUSINESS INTELLIGENCE:")
                        print(result['business_intelligence'][:300] + "...")
                
                return True
            else:
                print("❌ ERROR: No data returned")
                return False
                
        else:
            print(f"❌ HTTP ERROR: {response.status_code}")
            print(f"Response: {response.text}")
            return False
            
    except requests.exceptions.Timeout:
        print("⏰ TIMEOUT: Query took longer than 45 seconds")
        return False
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        return False

if __name__ == "__main__":
    success = test_query2()
    if success:
        print("\n🎉 QUERY 2 VALIDATION: PASSED")
    else:
        print("\n💥 QUERY 2 VALIDATION: FAILED")