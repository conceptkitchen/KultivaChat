#!/usr/bin/env python3
"""
Data Source Analysis Tool
Shows exactly how the API chooses between your different data types:
1. Closeout Sales sheets
2. Typeform data  
3. Squarespace forms data
"""

import requests
import json
from typing import List, Dict

API_URL = "http://127.0.0.1:8081"

def get_all_tables() -> List[str]:
    """Get list of all available tables in BigQuery workspace"""
    response = requests.post(f"{API_URL}/api/sql", 
                           json={"sql": """
                               SELECT table_name 
                               FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` 
                               WHERE table_name NOT LIKE '-%'
                               ORDER BY table_name
                           """})
    
    if response.status_code == 200:
        data = response.json()
        if data.get('status') == 'success':
            return [row['table_name'] for row in data['data']]
    return []

def categorize_tables(tables: List[str]) -> Dict[str, List[str]]:
    """Categorize tables by data source type"""
    categories = {
        'closeout_sales': [],
        'typeform': [],
        'squarespace': [],
        'other': []
    }
    
    for table in tables:
        table_lower = table.lower()
        
        if any(keyword in table_lower for keyword in ['close-out', 'closeout', 'sales']):
            categories['closeout_sales'].append(table)
        elif 'typeform' in table_lower:
            categories['typeform'].append(table)
        elif 'squarespace' in table_lower:
            categories['squarespace'].append(table)
        else:
            categories['other'].append(table)
    
    return categories

def test_query_routing(query: str) -> Dict:
    """Test which data source the API chooses for a specific query"""
    response = requests.post(f"{API_URL}/api/query", 
                           json={"query": query})
    
    if response.status_code == 200:
        return response.json()
    else:
        return {"error": f"HTTP {response.status_code}"}

def main():
    print("📊 DATA SOURCE ANALYSIS REPORT")
    print("=" * 50)
    
    # Get all available tables
    print("\n1. DISCOVERING ALL DATA TABLES...")
    tables = get_all_tables()
    print(f"Found {len(tables)} total tables")
    
    # Categorize tables by type
    print("\n2. CATEGORIZING BY DATA SOURCE TYPE...")
    categories = categorize_tables(tables)
    
    for category, table_list in categories.items():
        print(f"\n{category.upper().replace('_', ' ')} ({len(table_list)} tables):")
        for table in table_list[:5]:  # Show first 5
            print(f"  • {table}")
        if len(table_list) > 5:
            print(f"  ... and {len(table_list) - 5} more")
    
    # Test query routing behavior
    print("\n3. TESTING QUERY ROUTING BEHAVIOR...")
    print("-" * 40)
    
    test_queries = [
        ("Generic vendor query", "Show me vendor data"),
        ("Specific closeout sales", "Show me closeout sales data"),
        ("Specific typeform", "Show me typeform submissions"),
        ("Specific squarespace", "Show me squarespace vendor applications"),
        ("Revenue question", "How much money did vendors make?"),
        ("Contact question", "What email addresses do we have?")
    ]
    
    for test_name, query in test_queries:
        print(f"\n{test_name}: '{query}'")
        result = test_query_routing(query)
        
        data_source = result.get('data_source', 'Unknown')
        print(f"  → Data Source Used: {data_source}")
        
        # Determine which category this table belongs to
        source_category = "Unknown"
        for cat, table_list in categories.items():
            if data_source in table_list:
                source_category = cat.replace('_', ' ').title()
                break
        
        print(f"  → Category: {source_category}")
        
        if 'business_intelligence' in result:
            summary = result['business_intelligence'][:100] + "..."
            print(f"  → Summary: {summary}")
    
    print("\n4. RECOMMENDATIONS FOR PRECISE DATA SOURCE CONTROL")
    print("-" * 50)
    
    print("\nTo ensure the API uses the correct data source:")
    print("• Use specific keywords in your queries")
    print("• 'closeout sales' → Targets closeout sales tables")
    print("• 'typeform' → Targets typeform submission tables") 
    print("• 'squarespace' → Targets squarespace form tables")
    print("• Generic queries default to the most relevant vendor/sales tables")
    
    print("\nExample precise queries:")
    print("• 'Show me typeform vendor applications'")
    print("• 'What closeout sales revenue do we have?'")
    print("• 'Give me squarespace form contact information'")

if __name__ == "__main__":
    main()