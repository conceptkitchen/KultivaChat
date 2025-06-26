# Kultivate AI Simplified API Documentation

## Overview

This API provides direct access to BigQuery Keboola Workspace data without requiring client-side credential management. All authentication is handled server-side, making integration straightforward and secure.

**Base URL:** `https://kultivate-chat-ck.replit.app`

---

## How It Works

### Server-Side Architecture
The API uses a secure server-side credential system where:

1. **Credentials are pre-configured** on the server (Google Cloud service account, Keboola tokens)
2. **No client credentials needed** - you just send your query
3. **Direct BigQuery access** to Keboola Workspace (64 tables available)
4. **Intelligent routing** automatically selects the best processing method

### Data Source
- **BigQuery Project:** `kbc-use4-839-261b`
- **Dataset:** `WORKSPACE_21894820` 
- **Tables:** 64 business data tables (Kapwa Gardens, Balay Kreative, vendor data, events, sales)
- **No Keboola Storage buckets** - only BigQuery workspace data

---

## API Endpoints

### 1. Health Check
**Endpoint:** `GET /api/health`

**Purpose:** Verify API service status

**Request:**
```bash
curl https://kultivate-chat-ck.replit.app/api/health
```

**Response:**
```json
{
  "backend": "running",
  "status": "healthy"
}
```

---

### 2. Get All Available Tables
**Endpoint:** `POST /api/v1/data/tables`

**Purpose:** Retrieve list of all 64 available data tables

**Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/tables \
  -H "Content-Type: application/json" \
  -d '{}'
```

**How it works internally:**
```sql
SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name
```

**Response:**
```json
{
  "success": true,
  "data": [
    {"table_name": "Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders"},
    {"table_name": "Vendor-Close-Out---Dye-Hard--2023-04-02---Kapwa-Gardens-New-close-out-Dye-Hard"},
    {"table_name": "Close-Outs---Yum-Yams---2023-05-13---Kapwa-Gardens-All-Vendor-Close-Out-Sales"}
  ],
  "timestamp": "2025-06-26T00:13:06.687138"
}
```

---

### 3. Execute SQL Queries
**Endpoint:** `POST /api/v1/data/sql`

**Purpose:** Run direct SQL queries against BigQuery Keboola Workspace

**Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/sql \
  -H "Content-Type: application/json" \
  -d '{
    "sql": "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 5"
  }'
```

**How it works internally:**
1. Validates SQL syntax
2. Executes query using server-side BigQuery client
3. Returns actual business data records

**Response:**
```json
{
  "success": true,
  "data": [
    {
      "Event_Name": "Barya Kitchen & An Choi: Filipino x Vietnamese Kamayan - 3/20/2020",
      "Lineitem_price": "75",
      "Lineitem_quantity": "2",
      "Order_ID": "8",
      "Event_Date": "3/20/2020"
    }
  ],
  "rows_returned": 5,
  "error": null,
  "timestamp": "2025-06-26T00:13:16.770172"
}
```

---

### 4. Natural Language Query Endpoint (Recommended)
**Endpoint:** `POST /api/v1/data/query`

**Purpose:** Ask business questions in plain English - the AI handles everything automatically

**Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{
    "query": "show me tables"
  }'
```

**How the routing works:**

#### Table Discovery Routing
Detects phrases like:
- "show me tables"
- "list all tables"
- "what tables are available"

**Internally routes to:** Table discovery endpoint

#### SQL Query Routing
Detects queries starting with:
- `SELECT`
- `CREATE` 
- `INSERT`
- `UPDATE`
- `DELETE`
- `WITH`

**Internally routes to:** SQL execution endpoint

#### Natural Language Routing
Everything else routes to advanced AI processing with full Gemini 2.0 Flash integration for business intelligence queries

**Response includes routing transparency:**
```json
{
  "success": true,
  "route_used": "tables",
  "data": [...],
  "timestamp": "2025-06-26T00:07:01.265849"
}
```

---

## Code Implementation Details

### Backend Architecture

**File:** `backend/main_2.py`

**Key Function:** `internal_execute_sql_query()`
```python
def internal_execute_sql_query(sql_query: str) -> dict:
    """Executes BigQuery SQL against Keboola workspace"""
    query_job = bigquery_client.query(sql_query)
    results = query_job.result(timeout=60)
    rows_list = []
    for row in results:
        row_dict = {}
        for key, value in dict(row).items():
            if hasattr(value, '__class__') and 'Decimal' in str(type(value)):
                row_dict[key] = float(value)
            else:
                row_dict[key] = value
        rows_list.append(row_dict)
    return {"status": "success", "data": rows_list}
```

**Credential Management:**
- Google Cloud service account JSON loaded server-side
- BigQuery client initialized once at startup
- No client-side authentication required

**Route Decision Logic:**
```python
def determine_query_route(query):
    """Intelligent routing based on query content"""
    query_lower = query.lower().strip()
    
    # Table discovery patterns
    table_patterns = ['show me tables', 'list tables', 'what tables', 'available tables']
    if any(pattern in query_lower for pattern in table_patterns):
        return {'route': 'tables', 'reason': 'Table discovery request detected'}
    
    # SQL query patterns  
    sql_patterns = ['select ', 'create ', 'insert ', 'update ', 'delete ', 'with ']
    if any(query_lower.startswith(pattern) for pattern in sql_patterns):
        return {'route': 'sql', 'reason': 'Direct SQL query detected'}
    
    # Default to natural language
    return {'route': 'nlp', 'reason': 'Natural language query requiring AI processing'}
```

---

## Working Examples

### Get Available Tables
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "show me tables"}'
```

### Get Balay Kreative Attendee Data
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/sql \
  -H "Content-Type: application/json" \
  -d '{
    "sql": "SELECT Event_Name, Event_Date, Lineitem_price, Order_ID FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 10"
  }'
```

### Get Revenue Analysis
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/sql \
  -H "Content-Type: application/json" \
  -d '{
    "sql": "SELECT Event_Name, SUM(CAST(Lineitem_price AS FLOAT64)) as total_revenue FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` GROUP BY Event_Name ORDER BY total_revenue DESC"
  }'
```

---

## Integration Code Examples

### JavaScript/Node.js
```javascript
async function getBusinessData() {
  const response = await fetch('https://kultivate-chat-ck.replit.app/api/v1/data/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      query: 'show me tables'
    })
  });
  const data = await response.json();
  console.log('Available tables:', data.data.length);
  return data;
}

async function querySpecificData() {
  const response = await fetch('https://kultivate-chat-ck.replit.app/api/v1/data/sql', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      sql: "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 5"
    })
  });
  return await response.json();
}
```

### Python
```python
import requests

def get_tables():
    response = requests.post(
        'https://kultivate-chat-ck.replit.app/api/v1/data/query',
        json={'query': 'show me tables'}
    )
    return response.json()

def query_business_data(sql_query):
    response = requests.post(
        'https://kultivate-chat-ck.replit.app/api/v1/data/sql',
        json={'sql': sql_query}
    )
    return response.json()

# Example usage
tables = get_tables()
print(f"Found {len(tables['data'])} tables")

# Get actual business records
data = query_business_data("""
    SELECT Event_Name, Event_Date, Lineitem_price 
    FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` 
    LIMIT 10
""")
print(f"Retrieved {data['rows_returned']} records")
```

---

## Security Features

### Server-Side Credential Management
- All sensitive credentials stored securely on server
- No API keys or tokens exposed to clients
- Google Cloud service account with minimal required permissions

### Query Validation
- SQL injection protection through parameterized queries
- Query timeout limits (60 seconds)
- Error handling prevents credential exposure

### Rate Limiting
- Built-in request throttling
- Resource usage monitoring
- Graceful error responses

---

## Data Available

### Business Tables Include:
- **Event Data:** Attendee lists, registrations, event details
- **Sales Data:** Revenue, transactions, order details  
- **Vendor Data:** Close-out sales, vendor information
- **Market Data:** Event performance, attendance tracking

### Table Naming Convention:
- `Balay-Kreative---attendees---all-orders-*`
- `Vendor-Close-Out---*---Kapwa-Gardens-*`
- `Close-Outs---*---Kapwa-Gardens-All-Vendor-Close-Out-Sales`

### Data Types Available:
- Customer information
- Event attendance records
- Financial transactions
- Vendor performance data
- Market analytics

---

## Response Format

All API responses follow this structure:

```json
{
  "success": true|false,
  "data": [...],           // Actual records or results
  "route_used": "tables|sql|nlp",  // Which processing method was used
  "rows_returned": 0,      // Number of records returned
  "error": null|"message", // Error message if applicable
  "timestamp": "2025-06-26T00:13:16.770172"
}
```

---

## Best Practices

### 1. Start with Table Discovery
Always begin by getting available tables:
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "show me tables"}'
```

### 2. Use Fully Qualified Table Names
```sql
SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME` LIMIT 10
```

### 3. Limit Result Sets
```sql
SELECT * FROM table_name LIMIT 100
```

### 4. Handle Errors Gracefully
```javascript
if (response.success) {
  console.log('Data:', response.data);
} else {
  console.error('Error:', response.error);
}
```

---

## Simple Natural Language Usage

The API now supports natural language queries just like talking to a business analyst:

### Business Questions You Can Ask:
```bash
# Revenue analysis
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much revenue did we generate from Balay Kreative events?"}'

# Customer insights  
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me customer data from Kapwa Gardens orders"}'

# Product analysis
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the top selling products across all vendors?"}'

# Event performance
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which events had the highest attendance rates?"}'
```

### How Natural Language Works:
- Ask questions in plain English about your business data
- The AI understands context about vendors, events, customers, and revenue
- Automatically finds relevant tables and executes appropriate queries
- Returns actual business data, not generic responses
- Powered by advanced AI that understands your specific business entities

## Current Limitations

1. **Result Set Size:** No enforced limits, use LIMIT clauses for large datasets
2. **Query Complexity:** Standard BigQuery limitations apply
3. **Concurrent Requests:** Reasonable limits for production use

---

Last Updated: June 26, 2025