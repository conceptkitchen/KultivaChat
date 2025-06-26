# Kultivate AI API Usage Guide

## Base URL
```
https://kultivate-chat-ck.replit.app
```

## Authentication
All API requests require a `credentials` object in the request body. Currently accepts empty credentials object `{}`.

---

## API Endpoints

### 1. Health Check
**Endpoint:** `GET /api/health`

**Purpose:** Check if the API service is running

**Example:**
```bash
curl -X GET https://kultivate-chat-ck.replit.app/api/health
```

**Response:**
```json
{
  "backend": "running",
  "status": "healthy"
}
```

---

### 2. Table Discovery
**Endpoint:** `POST /api/v1/data/tables`

**Purpose:** Get a list of all available database tables

**Request Body:**
```json
{
  "credentials": {}
}
```

**Example:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/tables \
  -H "Content-Type: application/json" \
  -d '{"credentials":{}}'
```

**Response:**
```json
{
  "success": true,
  "data": [
    {"table_name": "2023-09-16-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors"},
    {"table_name": "Vendor-Close-Out---Dye-Hard--2023-04-02---Kapwa-Gardens-New-close-out-Dye-Hard"}
  ],
  "timestamp": "2025-06-26T00:07:01.265849"
}
```

---

### 3. Direct SQL Execution
**Endpoint:** `POST /api/v1/data/sql`

**Purpose:** Execute SQL queries directly against BigQuery

**Request Body:**
```json
{
  "sql": "YOUR_SQL_QUERY",
  "credentials": {}
}
```

#### ✅ ALLOWED SQL Queries

**Simple SELECT queries:**
```sql
SELECT 1 as test_number, 'Hello World' as test_text
```

**Table information queries:**
```sql
SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 5
```

**Data retrieval from specific tables:**
```sql
SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.table_name` LIMIT 10
```

#### ❌ RESTRICTED SQL Queries

**CREATE TABLE without qualified dataset:**
```sql
CREATE TABLE test AS SELECT 1 as id
-- Error: Table must be qualified with dataset
```

**Invalid syntax:**
```sql
INVALID SQL SYNTAX
-- Error: Syntax error returned
```

**Example Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/sql \
  -H "Content-Type: application/json" \
  -d '{"sql":"SELECT 1 as test","credentials":{}}'
```

**Response:**
```json
{
  "success": true,
  "data": [{"test": 1}],
  "rows_returned": 1,
  "error": null,
  "timestamp": "2025-06-26T00:07:01.265849"
}
```

---

### 4. Intelligent Query Router
**Endpoint:** `POST /api/v1/data/query`

**Purpose:** Single endpoint that automatically routes queries to the appropriate processing method

**Request Body:**
```json
{
  "query": "YOUR_QUERY_TEXT",
  "credentials": {}
}
```

#### Query Types and Routing Logic

**Table Discovery Queries** (Routes to `/tables` endpoint):
- ✅ "show me tables"
- ✅ "list all tables" 
- ✅ "what tables are available"
- ✅ "display tables"
- ✅ "get table list"

**SQL Queries** (Routes to `/sql` endpoint):
- ✅ Queries starting with "SELECT"
- ✅ Queries starting with "CREATE" 
- ✅ Queries starting with "INSERT"
- ✅ Queries starting with "UPDATE"
- ✅ Queries starting with "DELETE"
- ✅ Queries starting with "WITH"

**Natural Language Queries** (Currently returns 503 - Service Temporarily Unavailable):
- ❌ "hello"
- ❌ "analyze my data"
- ❌ "show me sales information"
- ❌ General conversational queries

#### Examples

**Table Discovery:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query":"show me tables","credentials":{}}'
```

**SQL Execution:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query":"SELECT 1 as test","credentials":{}}'
```

**Response Format:**
All responses include a `route_used` field showing which processing method was selected:
```json
{
  "success": true,
  "route_used": "tables",
  "data": [...],
  "timestamp": "2025-06-26T00:07:01.265849"
}
```

---

## Error Handling

### Common Error Responses

**Missing Query Parameter:**
```json
{
  "success": false,
  "error": "Query parameter is required"
}
```

**SQL Syntax Error:**
```json
{
  "success": false,
  "error": "Unknown error",
  "data": [],
  "rows_returned": 0,
  "timestamp": "2025-06-26T00:07:30.379041"
}
```

**Natural Language Processing Unavailable:**
```json
{
  "success": false,
  "error": "Natural language processing via API v1 route temporarily unavailable. Please use table discovery or direct SQL endpoints.",
  "route_used": "nlp",
  "suggestion": "Try 'show me tables' for table discovery or use direct SQL queries starting with SELECT",
  "timestamp": "2025-06-26T00:07:22.600126"
}
```

---

## Data Schema Information

### Available Tables
The API provides access to 64 business data tables including:
- Close-out sales data
- Vendor information
- Market event data
- Attendee records
- Transaction data

### Table Naming Convention
Tables follow this pattern:
- Event dates (e.g., "2023-09-16-UNDISCOVERED-SF")
- Business names (e.g., "Kapwa-Gardens", "Balay-Kreative")
- Data types (e.g., "Close-Out-Sales", "attendees", "All-data-orders")

---

## Best Practices

### 1. Start with Table Discovery
Always begin by discovering available tables:
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query":"show me tables","credentials":{}}'
```

### 2. Use Fully Qualified Table Names
When querying specific tables, use the complete BigQuery path:
```sql
SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.table_name` LIMIT 10
```

### 3. Limit Result Sets
Always use LIMIT clauses to avoid large result sets:
```sql
SELECT * FROM table_name LIMIT 100
```

### 4. Handle Errors Gracefully
Check the `success` field in responses and handle errors appropriately.

---

## Current Limitations

1. **Natural Language Processing:** Currently returns 503 status with helpful error messages
2. **CREATE Operations:** Must use fully qualified dataset names
3. **Authentication:** Currently accepts empty credentials object
4. **Result Set Size:** No explicit limits enforced, but recommended to use LIMIT clauses

---

## Integration Examples

### JavaScript/Node.js
```javascript
const response = await fetch('https://kultivate-chat-ck.replit.app/api/v1/data/query', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    query: 'show me tables',
    credentials: {}
  })
});
const data = await response.json();
console.log(data);
```

### Python
```python
import requests

response = requests.post(
    'https://kultivate-chat-ck.replit.app/api/v1/data/query',
    json={
        'query': 'SELECT 1 as test',
        'credentials': {}
    }
)
print(response.json())
```

### cURL
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query":"show me tables","credentials":{}}'
```

---

## Support

For API issues or questions, the service provides detailed error messages and suggestions for resolution. The intelligent query router automatically selects the best processing method based on your query content.

Last Updated: June 26, 2025