#!/usr/bin/env python3
def test_query2_fast():
    import requests
    import json
    
    print("=== QUERY 2 FAST TEST ===")
    print("Query: Which event from 2021 to 2024 made the most money for vendors?")
    
    # First, let's get the actual revenue data from the 10 tables (2023-2024 only)
    tables_2023_2024 = [
        "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal",
        "2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors", 
        "2023-09-16-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors",
        "2023-10-21-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors",
        "2024-07-13-Sulat_-Close-Out-Sales-START-HERE-Vendor-Close-Out-Sal",
        "2024-10-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors",
        "2024-10-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-Donita",
        "2024-10-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-Marissa", 
        "2024-10-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-Tiara"
    ]
    
    print(f"\nActual data range: 2023-2024 (NOT 2021-2024)")
    print(f"Total tables to analyze: {len(tables_2023_2024)}")
    
    # Test direct SQL to get revenue totals
    sql_query = f"""
    SELECT 
      '{tables_2023_2024[0]}' as event_table,
      SUM(CAST(REGEXP_REPLACE(COALESCE(Total_Sales, '0'), r'[^0-9.]', '') AS FLOAT64)) as total_revenue
    FROM `kbc-use4-839-261b.WORKSPACE_23990909.{tables_2023_2024[0]}`
    WHERE Total_Sales IS NOT NULL AND Total_Sales != ''
    """
    
    try:
        response = requests.post(
            "http://localhost:8081/api/query",
            json={"query": sql_query},
            timeout=15
        )
        
        if response.status_code == 200:
            result = response.json()
            print(f"\n=== SAMPLE REVENUE DATA ===")
            print(json.dumps(result, indent=2))
            
            # Now test the natural language query with corrected date range
            corrected_query = "Which event from 2023 to 2024 made the most money for vendors?"
            print(f"\n=== TESTING CORRECTED QUERY ===")
            print(f"Query: {corrected_query}")
            
            nl_response = requests.post(
                "http://localhost:8081/api/query", 
                json={"query": corrected_query},
                timeout=30
            )
            
            if nl_response.status_code == 200:
                nl_result = nl_response.json()
                print(f"\n=== NATURAL LANGUAGE RESPONSE ===")
                if 'response' in nl_result:
                    print(nl_result['response'])
                else:
                    print(json.dumps(nl_result, indent=2))
            else:
                print(f"NL Query failed: {nl_response.status_code}")
                
        else:
            print(f"SQL Query failed: {response.status_code}")
            
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    test_query2_fast()