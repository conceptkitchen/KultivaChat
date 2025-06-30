#!/usr/bin/env python3

"""
CROSS-EVENT MIDDLE EASTERN VENDOR ANALYSIS
Identifies vendors who participated in both Kapwa Gardens and UNDISCOVERED events 
from 2020-2023 and identify as Middle Eastern
"""

import requests
import json
import time

def analyze_middle_eastern_vendors():
    """
    Execute comprehensive cross-event demographic analysis for Middle Eastern vendors
    """
    
    print("🔍 CROSS-EVENT MIDDLE EASTERN VENDOR ANALYSIS")
    print("=" * 60)
    print("Analyzing vendors who participated in BOTH:")
    print("• Kapwa Gardens events (2020-2023)")
    print("• UNDISCOVERED events (2020-2023)")
    print("• AND identify as Middle Eastern")
    print()
    
    # Step 1: Get all Kapwa Gardens vendors with demographic info
    print("Step 1: Extracting Kapwa Gardens vendor demographics...")
    kg_query = "Show me all vendor demographic information from Kapwa Gardens events including ethnicity, background, and business type"
    
    try:
        response = requests.post(
            'http://localhost:8081/api/query',
            headers={'Content-Type': 'application/json'},
            json={'query': kg_query},
            timeout=120  # 2 minutes for accurate results
        )
        kg_result = response.json()
        print(f"✓ Kapwa Gardens analysis: {kg_result.get('status', 'unknown')}")
        if kg_result.get('data'):
            print(f"  Found {len(kg_result['data'])} vendor records")
            print(f"  Table: {kg_result.get('table_source', 'Multiple tables')}")
        
    except Exception as e:
        print(f"✗ Kapwa Gardens analysis failed: {e}")
        kg_result = None
    
    print()
    
    # Step 2: Get all UNDISCOVERED vendors with demographic info  
    print("Step 2: Extracting UNDISCOVERED vendor demographics...")
    undis_query = "Show me all vendor demographic information from UNDISCOVERED events including ethnicity, background, and business type"
    
    try:
        response = requests.post(
            'http://localhost:8081/api/query',
            headers={'Content-Type': 'application/json'},
            json={'query': undis_query},
            timeout=120  # 2 minutes for accurate results
        )
        undis_result = response.json()
        print(f"✓ UNDISCOVERED analysis: {undis_result.get('status', 'unknown')}")
        if undis_result.get('data'):
            print(f"  Found {len(undis_result['data'])} vendor records")
            print(f"  Table: {undis_result.get('table_source', 'Multiple tables')}")
        
    except Exception as e:
        print(f"✗ UNDISCOVERED analysis failed: {e}")
        undis_result = None
    
    print()
    
    # Step 3: Execute the full cross-event query with extended timeout
    print("Step 3: Cross-event Middle Eastern vendor identification...")
    cross_query = "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2020-2023 and identify as Middle Eastern?"
    
    try:
        start_time = time.time()
        response = requests.post(
            'http://localhost:8081/api/query',
            headers={'Content-Type': 'application/json'},
            json={'query': cross_query},
            timeout=300  # 5 minutes for comprehensive accuracy
        )
        cross_result = response.json()
        duration = time.time() - start_time
        
        print(f"✓ Cross-event analysis completed in {duration:.1f}s")
        print(f"  Status: {cross_result.get('status', 'unknown')}")
        
        if cross_result.get('status') == 'success':
            if cross_result.get('data'):
                vendors = cross_result['data']
                print(f"  Found {len(vendors)} Middle Eastern vendors in both event types")
                print()
                print("🎯 MIDDLE EASTERN VENDORS IN BOTH KAPWA GARDENS & UNDISCOVERED:")
                print("-" * 60)
                
                for i, vendor in enumerate(vendors, 1):
                    print(f"{i}. {vendor.get('name', 'Unknown Vendor')}")
                    if 'email' in vendor:
                        print(f"   Email: {vendor['email']}")
                    if 'phone' in vendor:
                        print(f"   Phone: {vendor['phone']}")
                    if 'ethnicity' in vendor:
                        print(f"   Ethnicity: {vendor['ethnicity']}")
                    if 'business_type' in vendor:
                        print(f"   Business: {vendor['business_type']}")
                    print()
                
                return {
                    'status': 'success',
                    'middle_eastern_vendors': vendors,
                    'count': len(vendors),
                    'analysis_time': f"{duration:.1f}s"
                }
            else:
                print("  No Middle Eastern vendors found in both event types")
                return {
                    'status': 'success',
                    'middle_eastern_vendors': [],
                    'count': 0,
                    'message': 'No vendors found matching all criteria'
                }
        else:
            error = cross_result.get('error_message', cross_result.get('error', 'Unknown error'))
            print(f"✗ Analysis failed: {error}")
            return {'status': 'error', 'error': error}
        
    except requests.Timeout:
        print("✗ Analysis timed out - query too complex for current timeout limits")
        return {'status': 'timeout', 'message': 'Query requires more processing time'}
    except Exception as e:
        print(f"✗ Analysis failed: {e}")
        return {'status': 'error', 'error': str(e)}

if __name__ == '__main__':
    result = analyze_middle_eastern_vendors()
    
    print("\n" + "=" * 60)
    print("ANALYSIS SUMMARY")
    print("=" * 60)
    
    if result['status'] == 'success':
        print(f"Middle Eastern vendors in both events: {result['count']}")
        if result['count'] > 0:
            print("✓ Successfully identified cross-event Middle Eastern vendor participation")
        else:
            print("ℹ No vendors found matching all criteria (both events + Middle Eastern)")
    elif result['status'] == 'timeout':
        print("⏱ Analysis requires extended processing time for full accuracy")
        print("Recommendation: Run query with higher timeout limits")
    else:
        print(f"✗ Analysis encountered error: {result.get('error', 'Unknown')}")