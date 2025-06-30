#!/usr/bin/env python3
import requests
import json

def test_revenue_comparison():
    """Get actual revenue totals for each 2023-2024 event"""
    print("=== DIRECT REVENUE COMPARISON ===")
    
    events = [
        ("Lovers Mart 2023", "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal"),
        ("UNDISCOVERED 2023-08", "2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors"),
        ("UNDISCOVERED 2023-09", "2023-09-16-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors"),
        ("UNDISCOVERED 2023-10", "2023-10-21-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors"),
        ("Sulat 2024", "2024-07-13-Sulat_-Close-Out-Sales-START-HERE-Vendor-Close-Out-Sal"),
        ("UNDISCOVERED 2024-10", "2024-10-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors")
    ]
    
    results = []
    
    for event_name, table_name in events:
        sql = f"""
        SELECT 
          '{event_name}' as event_name,
          SUM(CAST(REGEXP_REPLACE(COALESCE(Total_Sales, '0'), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
          COUNT(*) as vendor_count
        FROM `kbc-use4-839-261b.WORKSPACE_23990909.{table_name}`
        WHERE Total_Sales IS NOT NULL AND Total_Sales != ''
        """
        
        try:
            response = requests.post(
                "http://localhost:8081/api/query",
                json={"query": sql},
                timeout=10
            )
            
            if response.status_code == 200:
                data = response.json()
                if data['data']:
                    result = data['data'][0]
                    results.append(result)
                    print(f"{event_name}: ${result['total_revenue']:,.2f} ({result['vendor_count']} vendors)")
            
        except Exception as e:
            print(f"Error for {event_name}: {e}")
    
    if results:
        # Find highest revenue event
        max_event = max(results, key=lambda x: x['total_revenue'])
        print(f"\n=== ANSWER ===")
        print(f"Highest revenue event: {max_event['event_name']}")
        print(f"Total revenue: ${max_event['total_revenue']:,.2f}")
        print(f"Number of vendors: {max_event['vendor_count']}")
        
        print(f"\n=== CORRECTED QUERY RESPONSE ===")
        print(f"Query: 'Which event from 2023 to 2024 made the most money for vendors?'")
        print(f"Answer: {max_event['event_name']} made the most money with ${max_event['total_revenue']:,.2f} in total vendor revenue from {max_event['vendor_count']} vendors.")

if __name__ == "__main__":
    test_revenue_comparison()