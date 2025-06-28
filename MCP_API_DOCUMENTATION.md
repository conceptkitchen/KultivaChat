# Kultivate AI MCP Server API Documentation

## Overview

The Kultivate AI MCP (Model Context Protocol) Server is a standalone backend API service providing business intelligence and data analysis capabilities. This server exposes clean REST endpoints for external frontend applications to connect to your BigQuery workspace and Keboola data sources.

## Base URL
```
https://kultiva-chatv-2-mcp-conceptkitchen.replit.app
```

## Authentication
All endpoints are currently open access. For production deployment, implement API key authentication as needed.

## API Endpoints

### Health Check
**GET `/health`**

Check server status and availability.

**Response:**
```json
{
  "status": "healthy",
  "service": "kultivate-mcp-server", 
  "timestamp": "2025-06-27 21:47:21 UTC"
}
```

### Service Information
**GET `/`**

Get service information and available endpoints.

**Response:**
```json
{
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
}
```

### Available Tools
**GET `/api/tools`**

List all available MCP analysis tools.

**Response:**
```json
{
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
    }
  ]
}
```

### Direct SQL Execution
**POST `/api/sql`**

Execute direct SQL queries against your BigQuery workspace.

**Request Body:**
```json
{
  "query": "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 5"
}
```

**Response:**
```json
{
  "status": "success",
  "data": [
    {"table_name": "Balay-Kreative---attendees---all-orders"},
    {"table_name": "Kapwa-Gardens-2023---Close-Out-Sale"},
    {"table_name": "UNDSCVRD---Vendor-Export"}
  ],
  "row_count": 3
}
```

### Natural Language Queries  
**POST `/api/query`**

Process natural language business questions with AI-powered analysis.

**Request Body:**
```json
{
  "query": "Show me revenue data from vendor sales"
}
```

**Response:**
```json
{
  "status": "success", 
  "analysis": "Revenue analysis for vendor sales data",
  "data": [...],
  "insights": ["Key finding 1", "Key finding 2"]
}
```

### Table Discovery
**GET `/api/tables`**

Discover all available tables in your BigQuery workspace.

**Response:**
```json
{
  "status": "success",
  "data": [
    {
      "table_name": "Balay-Kreative---attendees---all-orders",
      "table_type": "BASE TABLE", 
      "creation_time": "2023-08-15T10:30:00Z"
    }
  ],
  "total_tables": 64
}
```

### Keboola Table Metadata
**POST `/api/keboola/table`**

Get detailed schema and metadata for Keboola tables.

**Request Body:**
```json
{
  "bucket_id": "in.c-workspace",
  "table_name": "vendor_data"
}
```

**Response:**
```json
{
  "status": "success",
  "schema": [...],
  "metadata": {...}
}
```

### Comprehensive Analysis
**POST `/api/analysis`**

Perform comprehensive data analysis on specific tables.

**Request Body:**
```json
{
  "table_name": "Kapwa-Gardens-2023---Close-Out-Sale",
  "analysis_type": "revenue"
}
```

**Response:**
```json
{
  "status": "success",
  "analysis_type": "revenue",
  "summary": {...},
  "details": [...],
  "recommendations": [...]
}
```

### Geographic Analysis
**POST `/api/geography`**

Get zip codes and geographic data for cities.

**Request Body:**
```json
{
  "city_name": "San Francisco",
  "state_code": "CA"
}
```

**Response:**
```json
{
  "status": "success",
  "city": "San Francisco",
  "state": "CA", 
  "zip_codes": ["94102", "94103", "94104"]
}
```

## Error Handling

All endpoints return consistent error responses:

```json
{
  "error": "Error description",
  "status": "error"
}
```

Common HTTP status codes:
- `200` - Success
- `400` - Bad Request (missing parameters)
- `500` - Internal Server Error

## Integration Examples

### JavaScript/Node.js
```javascript
// Health check
const response = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/health');
const health = await response.json();

// Natural language query
const queryResponse = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ query: "Show me sales data" })
});
const results = await queryResponse.json();
```

### Python
```python
import requests

# Table discovery
response = requests.get('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/tables')
tables = response.json()

# SQL execution
sql_response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/sql', 
  json={'query': 'SELECT * FROM table LIMIT 10'})
data = sql_response.json()
```

### cURL
```bash
# Health check
curl -X GET https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/health

# Natural language query
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me revenue trends"}'

# SQL execution
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/sql \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT table_name FROM INFORMATION_SCHEMA.TABLES"}'
```

## Data Sources

The MCP server connects to:
- **BigQuery Workspace**: `kbc-use4-839-261b.WORKSPACE_21894820`
- **Keboola Storage API**: For table metadata and schema information
- **Google Gemini AI**: For natural language processing

## Available Data Tables

Your workspace contains 64+ tables including:
- Vendor sales data (Kapwa Gardens, Balay Kreative, UNDSCVRD)
- Attendee information and orders
- Close-out sale records
- Market event data
- Financial transaction records

## Startup

To start the MCP server:

```bash
# Using the startup script
./start_mcp.sh

# Or directly with Python
python mcp_server.py
```

The server runs on port 8081 and is ready for external API connections.