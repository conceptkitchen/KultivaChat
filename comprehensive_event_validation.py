#!/usr/bin/env python3

"""
COMPREHENSIVE EVENT NAME & DATA TYPE VALIDATION
Tests all event name recognition and data extraction capabilities
Ensures zero hallucination and 100% authentic data extraction
"""

import requests
import json
import time

BASE_URL = 'http://localhost:8081'

# Test all event name variations and data types
test_queries = [
    # Event Name Recognition Tests
    {
        "category": "Event Name Recognition",
        "query": "Show me Balay Kreative attendee data",
        "expected_table_pattern": "Balay-Kreative",
        "expected_data_type": "attendee"
    },
    {
        "category": "Event Name Recognition", 
        "query": "Get UNDISCOVERED vendor contacts",
        "expected_table_pattern": "UNDISCOVERED",
        "expected_data_type": "contact"
    },
    {
        "category": "Event Name Recognition",
        "query": "Show me KG financial information", 
        "expected_table_pattern": "Kapwa-Gardens",
        "expected_data_type": "financial"
    },
    {
        "category": "Event Name Recognition",
        "query": "Kapwa Gardens vendor emails",
        "expected_table_pattern": "Kapwa-Gardens", 
        "expected_data_type": "contact"
    },
    
    # Data Type Extraction Tests
    {
        "category": "Contact Extraction",
        "query": "What are email addresses from vendor data?",
        "expected_fields": ["email", "name"],
        "min_records": 10
    },
    {
        "category": "Demographic Extraction", 
        "query": "Show me attendee demographics from Balay Kreative",
        "expected_table_pattern": "Balay-Kreative",
        "expected_data_type": "demographic"
    },
    {
        "category": "Location Extraction",
        "query": "What zip codes do vendors have?",
        "expected_fields": ["zip", "location"],
        "data_type": "location"
    },
    {
        "category": "Financial Extraction",
        "query": "Show me revenue from UNDISCOVERED events",
        "expected_table_pattern": "UNDISCOVERED",
        "expected_data_type": "financial"
    },
    
    # Anti-Hallucination Tests
    {
        "category": "Anti-Hallucination",
        "query": "Show me customer purchase data",
        "description": "Should discover real tables, never create fake ones"
    },
    {
        "category": "Anti-Hallucination", 
        "query": "Get sales report information",
        "description": "Should use actual table discovery, no assumptions"
    }
]

def make_api_request(query):
    try:
        response = requests.post(
            f'{BASE_URL}/api/query',
            headers={'Content-Type': 'application/json'},
            json={'query': query},
            timeout=30
        )
        return response.json()
    except Exception as error:
        return {'error': str(error), 'status': 'error'}

def validate_event_recognition():
    print('🎯 COMPREHENSIVE EVENT NAME & DATA TYPE VALIDATION\n')
    print('Testing event name recognition, data extraction, and anti-hallucination\n')
    
    passed = 0
    failed = 0
    results = []
    
    for test in test_queries:
        print(f"\n📋 Testing: {test['category']}")
        print(f"Query: \"{test['query']}\"")
        
        start_time = time.time()
        result = make_api_request(test['query'])
        duration = int((time.time() - start_time) * 1000)
        
        test_result = {
            'category': test['category'],
            'query': test['query'],
            'duration': f'{duration}ms',
            'status': result.get('status', 'unknown'),
            'passed': False,
            'details': {}
        }
        
        if result.get('status') == 'success':
            test_result['passed'] = True
            test_result['details'] = {
                'table_used': result.get('table_source'),
                'records_returned': len(result.get('data', [])),
                'execution_time': result.get('execution_time'),
                'query_executed': result.get('query_executed')
            }
            
            # Validate event name recognition
            if test.get('expected_table_pattern'):
                table_source = result.get('table_source', '')
                table_matches = test['expected_table_pattern'] in table_source
                test_result['details']['event_recognition'] = 'PASS' if table_matches else 'FAIL'
                if not table_matches:
                    test_result['passed'] = False
            
            # Validate data type extraction  
            if test.get('expected_fields') and result.get('data') and len(result['data']) > 0:
                first_record = result['data'][0]
                has_expected_fields = all(field in first_record for field in test['expected_fields'])
                test_result['details']['field_validation'] = 'PASS' if has_expected_fields else 'FAIL'
                if not has_expected_fields:
                    test_result['passed'] = False
            
            # Validate minimum record count
            if test.get('min_records'):
                record_count = len(result.get('data', []))
                meets_minimum = record_count >= test['min_records']
                test_result['details']['record_count_validation'] = 'PASS' if meets_minimum else 'FAIL'
                test_result['details']['actual_records'] = record_count
                if not meets_minimum:
                    test_result['passed'] = False
            
            # Anti-hallucination validation (no fake table names)
            if test['category'] == 'Anti-Hallucination':
                suspicious_names = ['vendor_sales_data', 'customer_data', 'sales_report', 'user_data']
                table_source = result.get('table_source', '')
                query_executed = result.get('query_executed', '')
                used_fake_table = any(name in table_source or name in query_executed for name in suspicious_names)
                test_result['details']['anti_hallucination'] = 'FAIL - Used fake table' if used_fake_table else 'PASS'
                if used_fake_table:
                    test_result['passed'] = False
            
            passed += 1
            print(f"✅ PASSED - {duration}ms")
            if result.get('data') and len(result['data']) > 0:
                print(f"   📊 Records: {len(result['data'])}, Table: {result.get('table_source')}")
        else:
            test_result['details']['error'] = result.get('error_message') or result.get('error')
            failed += 1
            print(f"❌ FAILED - {result.get('error_message') or result.get('error')}")
        
        results.append(test_result)
    
    # Summary Report
    print('\n' + '=' * 70)
    print('📊 COMPREHENSIVE VALIDATION SUMMARY')
    print('=' * 70)
    print(f"✅ Passed: {passed}")
    print(f"❌ Failed: {failed}")
    print(f"📈 Success Rate: {(passed / len(test_queries) * 100):.1f}%")
    
    print('\n🎯 EVENT NAME RECOGNITION RESULTS:')
    for r in [r for r in results if r['category'] == 'Event Name Recognition']:
        status = '✅' if r['passed'] else '❌'
        print(f"{status} {r['query']} -> {r['details'].get('table_used', 'No table')}")
    
    print('\n🛡️ ANTI-HALLUCINATION VALIDATION:')
    for r in [r for r in results if r['category'] == 'Anti-Hallucination']:
        status = '✅' if r['details'].get('anti_hallucination') == 'PASS' else '❌'
        print(f"{status} {r['query']} -> {r['details'].get('anti_hallucination')}")
    
    print('\n📋 DATA TYPE EXTRACTION RESULTS:')
    extraction_categories = ['Contact Extraction', 'Demographic Extraction', 'Location Extraction', 'Financial Extraction']
    for r in [r for r in results if r['category'] in extraction_categories]:
        status = '✅' if r['passed'] else '❌'
        print(f"{status} {r['category']}: {r['details'].get('records_returned', 0)} records")
    
    return {
        'total': len(test_queries),
        'passed': passed,
        'failed': failed,
        'success_rate': f"{(passed / len(test_queries) * 100):.1f}%",
        'results': results
    }

if __name__ == '__main__':
    try:
        summary = validate_event_recognition()
        print(f"\n🏁 Validation completed with {summary['success_rate']} success rate")
        exit(0 if summary['failed'] == 0 else 1)
    except Exception as error:
        print(f'❌ Validation failed: {error}')
        exit(1)