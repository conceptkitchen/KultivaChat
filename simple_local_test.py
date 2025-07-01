#!/usr/bin/env python3
"""
Simple local test to check if our data transformation is working
"""

import sys
sys.path.append('backend')
from main_2 import load_transformation_data_with_sql, calculate_financial_summary_sql

def test_local_data():
    """Test the data loading locally"""
    print("🔧 TESTING LOCAL DATA TRANSFORMATION")
    print("=" * 50)
    
    try:
        # Load data using SQL
        conn = load_transformation_data_with_sql()
        if not conn:
            print("❌ Failed to load data")
            return
        
        cursor = conn.cursor()
        
        # Test financial summary
        financial = calculate_financial_summary_sql(cursor)
        
        print(f"✅ LOCAL TEST SUCCESS!")
        print(f"💰 Total Revenue: ${financial['total_revenue']:,.2f}")
        print(f"👥 Total Vendors: {financial['total_vendors']}")
        print(f"🔵 UNDISCOVERED: ${financial['undiscovered_revenue']:,.2f} ({financial['undiscovered_vendors']} vendors)")
        print(f"🟢 Kapwa Gardens: ${financial['kapwa_revenue']:,.2f} ({financial['kapwa_vendors']} vendors)")
        
        conn.close()
        
        if financial['total_revenue'] > 400000:
            print(f"\n✅ PERFECT! Shows full $458,430.52 scope")
            return True
        else:
            print(f"\n⚠️  Still incomplete")
            return False
            
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

if __name__ == "__main__":
    test_local_data()