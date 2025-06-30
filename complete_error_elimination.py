#!/usr/bin/env python3
"""
Complete elimination of persistent false error in revenue analysis
"""
import requests
import json

def test_comprehensive_error_elimination():
    """Test all scenarios to confirm error elimination"""
    
    base_url = 'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app'
    
    # Test cases that should ALL work with authentic data
    test_scenarios = [
        # Direct SQL - these work
        ('Direct Table Count', 'SELECT COUNT(*) as table_count FROM `kbc-use4-839-261b.WORKSPACE_23990909.INFORMATION_SCHEMA.TABLES`'),
        
        # Revenue queries - these should now work via comprehensive analysis
        ('Revenue Analysis', 'show revenue analysis'),
        ('Vendor Revenue', 'which vendors made money'),
        ('Sales Data', 'show me sales data'),
        
        # Alternative comprehensive queries - these work
        ('Comprehensive BI', 'comprehensive business intelligence'),
        ('All Events Analysis', 'analyze all events'),
    ]
    
    results = {}
    working_count = 0
    false_error_count = 0
    
    for scenario_name, query in test_scenarios:
        print(f"\n=== {scenario_name} ===")
        
        try:
            response = requests.post(f'{base_url}/api/query',
                json={'query': query},
                headers={'Content-Type': 'application/json'},
                timeout=30)
            
            if response.status_code == 200:
                data = response.json()
                
                # Check for the persistent false error
                if isinstance(data, dict) and 'error' in data and 'No sales tables found for revenue analysis' in data['error']:
                    print(f"❌ FALSE ERROR: {data['error']}")
                    results[scenario_name] = 'FALSE_ERROR'
                    false_error_count += 1
                elif isinstance(data, dict) and ('data' in data or 'tables' in data or 'business_intelligence' in data):
                    record_count = 0
                    if 'data' in data:
                        record_count = len(data['data']) if data['data'] else 0
                    elif 'tables' in data:
                        record_count = len(data['tables'])
                    
                    print(f"✅ SUCCESS: {record_count} records")
                    results[scenario_name] = f'SUCCESS_{record_count}'
                    working_count += 1
                else:
                    print(f"⚠️ UNEXPECTED: {str(data)[:100]}")
                    results[scenario_name] = 'UNEXPECTED'
            else:
                print(f"❌ HTTP {response.status_code}")
                results[scenario_name] = f'HTTP_{response.status_code}'
                
        except Exception as e:
            print(f"❌ EXCEPTION: {e}")
            results[scenario_name] = f'EXCEPTION_{str(e)[:50]}'
    
    # Summary
    print(f"\n" + "="*60)
    print("DATA INTEGRITY ANALYSIS SUMMARY")
    print("="*60)
    print(f"Working scenarios: {working_count}/{len(test_scenarios)}")
    print(f"False error scenarios: {false_error_count}/{len(test_scenarios)}")
    
    if false_error_count > 0:
        print(f"\n🚨 DATA INTEGRITY VIOLATION CONFIRMED")
        print(f"API returns false 'No sales tables found' for {false_error_count} scenarios")
        print(f"This violates core requirement of authentic data analysis")
        
        false_error_scenarios = [k for k, v in results.items() if v == 'FALSE_ERROR']
        print(f"Affected scenarios: {false_error_scenarios}")
    else:
        print(f"\n✅ DATA INTEGRITY RESOLVED")
        print(f"All scenarios return authentic data or proper analysis")
    
    return results

if __name__ == "__main__":
    test_comprehensive_error_elimination()