#!/usr/bin/env python3
"""
Test proper MCP workflow for cross-event analysis
This demonstrates how the system SHOULD work with semantic understanding and tool usage
"""

import requests
import json

def test_proper_mcp_workflow():
    """Test how MCP should handle cross-event queries with tool-based discovery"""
    
    # The query that's currently failing
    cross_event_query = "Who has attended events at Balay Kreative and UNDSCVRD in 2020?"
    
    print("🔍 TESTING PROPER MCP WORKFLOW")
    print(f"Query: {cross_event_query}")
    print()
    
    # Step 1: First discover what tables exist (what the MCP system should do automatically)
    print("Step 1: Table Discovery (what MCP should do first)")
    response = requests.post(
        "http://localhost:8081/api/query",
        headers={"Content-Type": "application/json"},
        json={"query": "Show me my attendee tables"}
    )
    
    if response.status_code == 200:
        result = response.json()
        print("✅ Table discovery successful")
        if 'data' in result:
            tables = result.get('data', [])
            print(f"Found {len(tables)} attendee records")
            
            # Show what attendee tables exist
            attendee_tables = [
                'Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders',
                'Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-'
            ]
            print(f"Real attendee tables: {attendee_tables}")
    else:
        print(f"❌ Table discovery failed: {response.status_code}")
    
    print()
    
    # Step 2: Show how Gemini AI should construct cross-event query with real tables
    print("Step 2: Proper Cross-Event Query Construction")
    proper_sql = """
    -- Find attendees who participated in both Balay Kreative and UNDISCOVERED events
    WITH balay_attendees AS (
        SELECT DISTINCT Billing_Email as email, Billing_Name as name
        FROM `kbc-use4-839-261b.WORKSPACE_23990909.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders`
        WHERE Billing_Email IS NOT NULL AND Billing_Email != ''
    ),
    undiscovered_attendees AS (
        SELECT DISTINCT Billing_Email as email, Billing_Name as name  
        FROM `kbc-use4-839-261b.WORKSPACE_23990909.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-`
        WHERE Billing_Email IS NOT NULL AND Billing_Email != ''
    )
    SELECT 
        b.name,
        b.email,
        'Attended Both Events' as status
    FROM balay_attendees b
    INNER JOIN undiscovered_attendees u ON b.email = u.email
    ORDER BY b.name
    LIMIT 20
    """
    
    print("Proper SQL using real table names:")
    print(proper_sql[:300] + "...")
    
    # Step 3: Test the proper query
    print("\nStep 3: Execute Proper Cross-Event Analysis")
    response = requests.post(
        "http://localhost:8081/api/query",
        headers={"Content-Type": "application/json"},
        json={"query": proper_sql}
    )
    
    if response.status_code == 200:
        result = response.json()
        if result.get('status') == 'success':
            data = result.get('data', [])
            print(f"✅ Cross-event analysis successful: Found {len(data)} attendees who attended both events")
            if data:
                print("Sample results:")
                for i, attendee in enumerate(data[:3]):
                    print(f"  {i+1}. {attendee.get('name', 'N/A')} - {attendee.get('email', 'N/A')}")
        else:
            print(f"❌ Query failed: {result.get('error_message', 'Unknown error')}")
    else:
        print(f"❌ Request failed: {response.status_code}")
    
    print()
    print("🎯 CONCLUSION:")
    print("The MCP system should:")
    print("1. Let Gemini AI discover tables first using tools")
    print("2. Use semantic understanding to identify cross-event intent")
    print("3. Construct queries with real table names, not hallucinated ones")
    print("4. Provide authentic cross-event analysis results")

if __name__ == "__main__":
    test_proper_mcp_workflow()