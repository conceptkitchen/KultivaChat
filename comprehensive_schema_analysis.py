#!/usr/bin/env python3
"""
COMPREHENSIVE SCHEMA ANALYSIS FOR KULTIVATE AI DATABASE
Systematically examines all tables and their column headers to create complete schema map
"""

import requests
import json
import time

def api_query(query):
    """Make API call to Kultivate AI backend"""
    try:
        response = requests.post(
            'http://localhost:8081/api/query',
            headers={'Content-Type': 'application/json'},
            json={'query': query},
            timeout=30
        )
        return response.json()
    except Exception as e:
        return {'error': str(e)}

def get_all_tables():
    """Get complete list of all tables"""
    query = "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
    result = api_query(query)
    if 'data' in result:
        return [row['table_name'] for row in result['data']]
    return []

def get_table_schema(table_name):
    """Get column information for a specific table"""
    query = f"""
    SELECT column_name, data_type, is_nullable 
    FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.COLUMNS` 
    WHERE table_name = '{table_name}' 
    ORDER BY ordinal_position
    """
    result = api_query(query)
    if 'data' in result:
        return result['data']
    return []

def categorize_table(table_name):
    """Categorize table by type based on name patterns"""
    name_lower = table_name.lower()
    
    if 'close-out-sales' in name_lower or 'vendor-close-out' in name_lower:
        return 'CLOSE_OUT_SALES'
    elif 'vendor-export' in name_lower or 'vendor' in name_lower:
        return 'VENDOR_REGISTRATION'
    elif 'attendee' in name_lower:
        return 'ATTENDEE_REGISTRATION'
    elif 'squarespace' in name_lower:
        return 'SQUARESPACE_FORMS'
    elif 'typeform' in name_lower:
        return 'TYPEFORM_RESPONSES'
    elif 'grant' in name_lower:
        return 'GRANT_APPLICATIONS'
    else:
        return 'OTHER'

def analyze_all_schemas():
    """Perform comprehensive schema analysis of all tables"""
    print("🔍 COMPREHENSIVE SCHEMA ANALYSIS FOR KULTIVATE AI DATABASE")
    print("=" * 70)
    
    # Get all tables
    print("\n📊 Step 1: Discovering all tables...")
    tables = get_all_tables()
    print(f"Found {len(tables)} tables in workspace")
    
    # Categorize tables
    categorized = {}
    for table in tables:
        category = categorize_table(table)
        if category not in categorized:
            categorized[category] = []
        categorized[category].append(table)
    
    print(f"\n📋 Step 2: Table categorization:")
    for category, table_list in categorized.items():
        print(f"  {category}: {len(table_list)} tables")
    
    # Analyze schemas by category
    schema_map = {}
    
    print(f"\n🔍 Step 3: Analyzing schemas by category...")
    
    for category, table_list in categorized.items():
        print(f"\n{'='*50}")
        print(f"CATEGORY: {category} ({len(table_list)} tables)")
        print(f"{'='*50}")
        
        category_schemas = {}
        
        for i, table_name in enumerate(table_list[:3]):  # Analyze first 3 tables per category
            print(f"\n📝 Table {i+1}: {table_name}")
            print("-" * 60)
            
            schema = get_table_schema(table_name)
            if schema:
                category_schemas[table_name] = schema
                print(f"Columns ({len(schema)}):")
                for col in schema:
                    print(f"  • {col['column_name']} ({col['data_type']})")
            else:
                print("  ❌ Could not retrieve schema")
            
            time.sleep(0.5)  # Rate limiting
        
        schema_map[category] = category_schemas
        
        if len(table_list) > 3:
            print(f"\n   ... and {len(table_list) - 3} more {category} tables")
    
    # Generate summary report
    print(f"\n{'='*70}")
    print("📊 COMPREHENSIVE SCHEMA SUMMARY")
    print(f"{'='*70}")
    
    total_columns = 0
    for category, schemas in schema_map.items():
        print(f"\n{category}:")
        for table_name, columns in schemas.items():
            print(f"  📄 {table_name}: {len(columns)} columns")
            total_columns += len(columns)
    
    print(f"\n✅ ANALYSIS COMPLETE")
    print(f"  📊 Total Tables Analyzed: {sum(len(schemas) for schemas in schema_map.values())}")
    print(f"  📋 Total Columns Mapped: {total_columns}")
    print(f"  🗂️ Categories Found: {len(categorized)}")
    
    # Save detailed report
    with open('schema_analysis_report.json', 'w') as f:
        json.dump({
            'table_categories': categorized,
            'schema_details': schema_map,
            'summary': {
                'total_tables': len(tables),
                'total_analyzed': sum(len(schemas) for schemas in schema_map.values()),
                'total_columns': total_columns,
                'categories': len(categorized)
            }
        }, f, indent=2)
    
    print(f"  💾 Detailed report saved to 'schema_analysis_report.json'")
    
    return schema_map, categorized

if __name__ == "__main__":
    schema_map, categorized = analyze_all_schemas()