#!/usr/bin/env python3
"""
Debug the table discovery issue for revenue analysis
"""
import requests
import json

def test_table_discovery():
    """Test different table discovery patterns"""
    
    # First get all tables to see what we're working with
    print("=== Getting all tables ===")
    response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
        json={'query': 'SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` ORDER BY table_name'}, 
        headers={'Content-Type': 'application/json'})
    
    if response.status_code == 200:
        data = response.json()
        if 'data' in data:
            all_tables = [row['table_name'] if isinstance(row, dict) else row[0] for row in data['data']]
            print(f"Found {len(all_tables)} total tables")
            
            # Analyze table patterns
            close_out_tables = [t for t in all_tables if 'close' in t.lower() and 'out' in t.lower()]
            kapwa_tables = [t for t in all_tables if 'kapwa' in t.lower()]
            vendor_tables = [t for t in all_tables if 'vendor' in t.lower()]
            sales_tables = [t for t in all_tables if 'sales' in t.lower()]
            transformation_tables = [t for t in all_tables if 'transformation' in t.lower()]
            
            print(f"\nTable Pattern Analysis:")
            print(f"Close-out tables: {len(close_out_tables)}")
            print(f"Kapwa tables: {len(kapwa_tables)}")
            print(f"Vendor tables: {len(vendor_tables)}")
            print(f"Sales tables: {len(sales_tables)}")  
            print(f"Transformation tables: {len(transformation_tables)}")
            
            print(f"\nSample table names:")
            for i, table in enumerate(all_tables[:15]):
                print(f"{i+1:2d}. {table}")
            
            # Test the current revenue analysis query pattern
            print(f"\n=== Testing current revenue analysis SQL pattern ===")
            test_sql = """
            SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` 
            WHERE LOWER(table_name) LIKE '%close%out%sales%' 
            OR LOWER(table_name) LIKE '%vendor%'
            OR LOWER(table_name) LIKE '%sales%'
            OR LOWER(table_name) LIKE '%kapwa%'
            OR LOWER(table_name) LIKE '%undiscovered%'
            OR LOWER(table_name) LIKE '%close%out%'
            ORDER BY table_name
            """
            
            response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
                json={'query': test_sql}, 
                headers={'Content-Type': 'application/json'})
            
            if response.status_code == 200:
                pattern_data = response.json()
                if 'data' in pattern_data:
                    pattern_tables = [row['table_name'] if isinstance(row, dict) else row[0] for row in pattern_data['data']]
                    print(f"Revenue analysis pattern matches: {len(pattern_tables)} tables")
                    
                    if pattern_tables:
                        print("Matching tables:")
                        for table in pattern_tables[:10]:
                            print(f"  - {table}")
                    else:
                        print("❌ No tables match the current pattern!")
                        
                        # Try broader patterns
                        print(f"\n=== Testing broader patterns ===")
                        broad_sql = """
                        SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES` 
                        WHERE LOWER(table_name) LIKE '%transformation%'
                        ORDER BY table_name
                        """
                        
                        broad_response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
                            json={'query': broad_sql}, 
                            headers={'Content-Type': 'application/json'})
                        
                        if broad_response.status_code == 200:
                            broad_data = broad_response.json()
                            if 'data' in broad_data:
                                transformation_tables = [row['table_name'] if isinstance(row, dict) else row[0] for row in broad_data['data']]
                                print(f"Transformation tables: {len(transformation_tables)}")
                                for table in transformation_tables[:5]:
                                    print(f"  - {table}")
                else:
                    print(f"No data in pattern response: {pattern_data}")
            else:
                print(f"Pattern test failed: {response.status_code}")
        else:
            print(f"No data in response: {data}")
    else:
        print(f"Failed to get tables: {response.status_code} - {response.text}")

if __name__ == "__main__":
    test_table_discovery()