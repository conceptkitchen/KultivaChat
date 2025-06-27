#!/usr/bin/env python3
"""
Kultivate AI MCP Server
Standalone Model Context Protocol server for business intelligence and data analysis
Provides clean API endpoints for external frontend applications
"""

import os
import sys
import json
import logging
from datetime import datetime
from flask import Flask, jsonify, request

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Import backend functionality by copying the essential functions
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'backend'))

try:
    from main_2 import (
        internal_execute_sql_query,
        execute_complex_business_query, 
        execute_comprehensive_analysis,
        get_zip_codes_for_city,
        get_current_time,
        get_keboola_table_detail
    )
    logger.info("Successfully imported backend functions")
except ImportError as e:
    logger.error(f"Failed to import backend functions: {e}")
    # Define fallback functions if import fails
    def internal_execute_sql_query(query):
        return {"error": "Backend functions not available", "query": query}
    
    def execute_complex_business_query(query):
        return {"error": "Backend functions not available", "query": query}
    
    def execute_comprehensive_analysis(table_name, analysis_type="overview"):
        return {"error": "Backend functions not available", "table": table_name}
    
    def get_zip_codes_for_city(city_name, state_code=None):
        return {"error": "Backend functions not available", "city": city_name}
    
    def get_current_time():
        return {"current_time": datetime.now().isoformat()}
    
    def get_keboola_table_detail(bucket_id, table_name):
        return {"error": "Backend functions not available", "bucket": bucket_id, "table": table_name}

# Create MCP server app with CORS headers
app = Flask(__name__)

@app.after_request
def after_request(response):
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS')
    return response

@app.route('/', methods=['GET'])
def root():
    """MCP Server root endpoint"""
    return jsonify({
        "service": "Kultivate AI MCP Server",
        "version": "1.0.0",
        "description": "Business Intelligence API for data analysis",
        "endpoints": {
            "/health": "Server health check",
            "/api/query": "Natural language data queries",
            "/api/sql": "Direct SQL execution",
            "/api/tables": "Table discovery and schema",
            "/api/tools": "Available analysis tools"
        }
    })

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({
        "status": "healthy",
        "service": "kultivate-mcp-server",
        "timestamp": get_current_time()['current_time']
    })

@app.route('/api/tools', methods=['GET'])
def list_tools():
    """List available MCP tools"""
    return jsonify({
        "tools": [
            {
                "name": "sql_query",
                "description": "Execute SQL queries against BigQuery workspace",
                "endpoint": "/api/sql"
            },
            {
                "name": "business_query", 
                "description": "Complex business intelligence analysis",
                "endpoint": "/api/query"
            },
            {
                "name": "table_discovery",
                "description": "Discover available tables and schemas",
                "endpoint": "/api/tables"
            },
            {
                "name": "geographic_analysis",
                "description": "City and zip code analysis",
                "endpoint": "/api/query"
            },
            {
                "name": "keboola_metadata",
                "description": "Keboola table schema and metadata",
                "endpoint": "/api/keboola/table"
            }
        ]
    })

@app.route('/api/sql', methods=['POST'])
def execute_sql():
    """Execute direct SQL queries"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
            
        result = internal_execute_sql_query(data['query'])
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/query', methods=['POST'])
def natural_language_query():
    """Process natural language business queries"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
            
        query = data['query']
        
        # Route to appropriate analysis tool based on query complexity
        if any(keyword in query.lower() for keyword in ['complex', 'analysis', 'revenue', 'attendees']):
            result = execute_complex_business_query(query)
        else:
            result = internal_execute_sql_query(f"""
                SELECT table_name 
                FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` 
                WHERE table_name LIKE '%{query}%'
                LIMIT 10
            """)
            
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/tables', methods=['GET'])
def list_tables():
    """Discover available tables in BigQuery workspace"""
    try:
        result = internal_execute_sql_query("""
            SELECT table_name, table_type, creation_time
            FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES`
            ORDER BY table_name
        """)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/keboola/table', methods=['POST'])
def get_table_metadata():
    """Get Keboola table metadata and schema"""
    try:
        data = request.get_json()
        if not data or 'bucket_id' not in data or 'table_name' not in data:
            return jsonify({"error": "Missing 'bucket_id' or 'table_name' parameters"}), 400
            
        result = get_keboola_table_detail(data['bucket_id'], data['table_name'])
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/analysis', methods=['POST'])
def comprehensive_analysis():
    """Perform comprehensive data analysis"""
    try:
        data = request.get_json()
        if not data or 'table_name' not in data:
            return jsonify({"error": "Missing 'table_name' parameter"}), 400
            
        table_name = data['table_name']
        analysis_type = data.get('analysis_type', 'overview')
        
        result = execute_comprehensive_analysis(table_name, analysis_type)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/geography', methods=['POST'])
def geographic_lookup():
    """Get zip codes for cities"""
    try:
        data = request.get_json()
        if not data or 'city_name' not in data:
            return jsonify({"error": "Missing 'city_name' parameter"}), 400
            
        city_name = data['city_name']
        state_code = data.get('state_code')
        
        result = get_zip_codes_for_city(city_name, state_code)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 8081))
    print(f"🚀 Kultivate AI MCP Server starting on port {port}")
    print("📊 Ready for external API connections")
    
    app.run(
        host='0.0.0.0',
        port=port,
        debug=False,
        use_reloader=False,
        threaded=True
    )