#!/usr/bin/env python3
"""
Debug the exact matching logic for meta-data queries
"""

def test_matching():
    """Test the exact meta-data query matching logic"""
    
    original_query = "What years of sales data do we have?"
    query_lower = original_query.lower()
    
    meta_data_indicators = [
        'what years', 'which years', 'years of sales data', 'years of data',
        'what data do we have', 'data coverage', 'available years',
        'how many years', 'data range', 'time period'
    ]
    
    print(f"Query: '{original_query}'")
    print(f"Query Lower: '{query_lower}'")
    print("\nTesting indicators:")
    
    matched_indicators = []
    for indicator in meta_data_indicators:
        if indicator in query_lower:
            print(f"✅ MATCH: '{indicator}' found in query")
            matched_indicators.append(indicator)
        else:
            print(f"❌ NO MATCH: '{indicator}' not found in query")
    
    is_meta_data_query = any(indicator in query_lower for indicator in meta_data_indicators)
    
    print(f"\nFinal result:")
    print(f"is_meta_data_query = {is_meta_data_query}")
    print(f"Matched indicators: {matched_indicators}")
    
    # Test a few other variations
    test_queries = [
        "Which years of data do we have?",
        "What data do we have?",
        "Data coverage please",
        "Available years?",
        "How many years of data are available?"
    ]
    
    print(f"\nTesting other queries:")
    for test_query in test_queries:
        test_lower = test_query.lower()
        test_match = any(indicator in test_lower for indicator in meta_data_indicators)
        print(f"'{test_query}' -> {test_match}")

if __name__ == "__main__":
    test_matching()