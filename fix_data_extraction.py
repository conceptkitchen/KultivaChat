#!/usr/bin/env python3
"""
Comprehensive fix for data extraction bug in Kultivate AI API.
The issue: SQL queries execute successfully but data isn't captured from Gemini function responses.
"""

import re

# Fix for the data extraction logic in backend/main_2.py
EXTRACTION_FIX = '''
# COMPREHENSIVE FIX: Enhanced data extraction from Gemini function responses
def extract_data_from_function_response(tool_result):
    """Extract data from nested Gemini function response structure"""
    if not isinstance(tool_result, dict):
        return None
        
    # Handle nested result structure: {'result': {'status': 'success', 'data': [...]}}
    if 'result' in tool_result and isinstance(tool_result['result'], dict):
        nested_result = tool_result['result']
        status = nested_result.get('status')
        data = nested_result.get('data')
        
        if status == 'success' and data and isinstance(data, list) and len(data) > 0:
            return data
    
    # Handle direct structure: {'status': 'success', 'data': [...]}
    elif tool_result.get('status') == 'success':
        data = tool_result.get('data')
        if data and isinstance(data, list) and len(data) > 0:
            return data
    
    # Handle list responses directly
    elif isinstance(tool_result, list) and len(tool_result) > 0:
        return tool_result
        
    return None

# Modified extraction logic in chat history processing
for msg in reversed(history):
    if hasattr(msg, 'parts') and msg.parts:
        for part in msg.parts:
            if hasattr(part, 'function_response') and part.function_response:
                tool_name = part.function_response.name
                tool_result = part.function_response.response
                
                if tool_name == 'internal_execute_sql_query':
                    extracted_data = extract_data_from_function_response(tool_result)
                    if extracted_data:
                        query_data = extracted_data
                        tool_display_title = "SQL Query Results"
                        app.logger.info(f"EXTRACTION SUCCESS: {len(query_data)} rows")
                        break
'''

print("Data extraction fix ready for implementation")
print("Issue: Gemini function responses have nested structure that current extraction logic doesn't handle")
print("Solution: Enhanced extraction function to handle both nested and direct response structures")