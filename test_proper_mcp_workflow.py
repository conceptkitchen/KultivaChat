#!/usr/bin/env python3
"""
TEST PROPER MCP WORKFLOW IMPLEMENTATION
Tests whether the grant + multi-event analysis follows proper MCP workflow:
1) Discover tables using INFORMATION_SCHEMA 
2) Construct SQL with real table names
3) Execute authentic queries without fake table names

EXPECTED WORKFLOW:
- Grant query detected → INFORMATION_SCHEMA discovery → Real table construction → Authentic SQL execution
- NOT: Grant query → Gemini AI → Fake table names → BigQuery 404 errors
"""

import requests
import json
import time

def test_proper_mcp_workflow():
    """Test that grant + multi-event analysis follows proper MCP workflow"""
    
    base_url = "http://localhost:8081"
    
    print("🧪 TESTING PROPER MCP WORKFLOW IMPLEMENTATION")
    print("=" * 60)
    
    # Test 1: Simple table discovery (should work)
    print("\n📋 TEST 1: Basic table discovery (baseline)")
    try:
        response = requests.post(f"{base_url}/api/query", 
                               json={"query": "show me my tables"}, 
                               timeout=10)
        result = response.json()
        print(f"✅ Status: {result.get('status')}")
        print(f"✅ Tables found: {result.get('total_tables', 0)}")
        print(f"✅ Query method: {result.get('routing_method')}")
    except Exception as e:
        print(f"❌ Table discovery failed: {e}")
        return False
    
    # Test 2: Grant + multi-event query (should use proper MCP workflow)
    print("\n🎯 TEST 2: Grant + multi-event analysis (CRITICAL TEST)")
    try:
        response = requests.post(f"{base_url}/api/query", 
                               json={"query": "Who applied to a Balay Kreative grant and went to events more than 2x?"}, 
                               timeout=15)
        result = response.json()
        
        print(f"Status: {result.get('status')}")
        print(f"Routing: {result.get('routing_method')}")
        
        # CRITICAL CHECK: Should NOT have fake table names in error
        if result.get('status') == 'error':
            error_msg = result.get('error_message', '')
            if 'attendee_data' in error_msg or 'vendor_sales_data' in error_msg:
                print("❌ CRITICAL FAILURE: System using FAKE TABLE NAMES")
                print(f"❌ Error contains fake tables: {error_msg[:200]}...")
                print("❌ MCP workflow NOT implemented - still using Gemini AI with fake tables")
                return False
            else:
                print("✅ No fake table names detected in error")
        
        # Check if proper workflow was used
        if 'proper_mcp_tool_direct' in result.get('routing_method', ''):
            print("✅ Proper MCP tool routing detected")
        else:
            print("❌ Not using proper MCP tool routing")
            
    except Exception as e:
        print(f"❌ Grant analysis failed: {e}")
        return False
    
    # Test 3: Direct INFORMATION_SCHEMA query (should work with proper dataset qualification)
    print("\n🔍 TEST 3: Direct INFORMATION_SCHEMA query")
    try:
        response = requests.post(f"{base_url}/api/query", 
                               json={"query": f"SELECT COUNT(*) as table_count FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES`"}, 
                               timeout=10)
        result = response.json()
        print(f"Status: {result.get('status')}")
        if result.get('status') == 'success':
            print("✅ INFORMATION_SCHEMA query works with proper qualification")
        else:
            print(f"❌ INFORMATION_SCHEMA error: {result.get('error_message', '')[:100]}...")
    except Exception as e:
        print(f"❌ INFORMATION_SCHEMA test failed: {e}")
    
    print("\n" + "=" * 60)
    print("🎯 MCP WORKFLOW ANALYSIS:")
    print("- Proper MCP workflow: Tool discovers tables FIRST, then constructs SQL")
    print("- Broken workflow: Gemini AI generates fake table names, BigQuery fails")
    print("- Expected behavior: Grant queries should trigger table discovery, not fake SQL generation")
    
    return True

if __name__ == "__main__":
    test_proper_mcp_workflow()