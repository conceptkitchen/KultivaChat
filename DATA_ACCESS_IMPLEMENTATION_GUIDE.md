# How to Access Actual Data Beyond Table Metadata

Based on your screenshot showing table listings and metadata, here's exactly how this Kultivate AI project enables deep data access to view actual records within tables.

## Core Implementation: SQL Query Execution

### 1. **Direct BigQuery SQL Tool**

The key component is the `internal_execute_sql_query` function that directly executes SQL against BigQuery:

```python
def internal_execute_sql_query(sql_query: str) -> dict:
    """Execute SQL query against BigQuery and return structured results."""
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result()
        
        # Convert to list of dictionaries for frontend display
        rows = []
        for row in results:
            row_dict = {}
            for key, value in row.items():
                row_dict[key] = str(value) if value is not None else None
            rows.append(row_dict)
        
        return {
            "status": "success",
            "data": rows,
            "row_count": len(rows),
            "display_type": "table",
            "display_title": f"Query Results ({len(rows)} rows)"
        }
    except Exception as e:
        return {"status": "error", "error_message": str(e)}
```

### 2. **Natural Language to SQL Translation**

When users ask for data, the AI converts requests to SQL:

**User Input:** "Show me customer data from Kapwa Gardens"

**AI Processing:**
1. Identifies "customer" keyword
2. Searches for relevant tables using: `SELECT table_name FROM INFORMATION_SCHEMA.TABLES`
3. Finds table like `OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS`
4. Generates: `SELECT * FROM kbc-use4-839-261b.WORKSPACE_21894820.OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS LIMIT 10`
5. Executes query and returns actual customer records

### 3. **Table Discovery and Schema Inspection**

```python
# Step 1: Find available tables
table_query = f"""
SELECT table_name 
FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
ORDER BY table_name
"""

# Step 2: Get schema for specific table
schema_query = f"""
SELECT column_name, data_type 
FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
WHERE table_name = '{table_name}'
"""

# Step 3: Query actual data
data_query = f"""
SELECT * 
FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table_name}` 
LIMIT 10
"""
```

## What You Need to Implement

### 1. **BigQuery Client Setup**
```python
from google.cloud import bigquery
import os

# Initialize BigQuery client
credentials_path = os.getenv('GOOGLE_APPLICATION_CREDENTIALS')
bigquery_client = bigquery.Client.from_service_account_json(credentials_path)
```

### 2. **SQL Execution Function**
```python
def execute_query(sql_query: str):
    """Execute SQL against BigQuery and return results as JSON."""
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result()
        
        # Convert to displayable format
        data = []
        for row in results:
            data.append(dict(row))
        
        return {
            "success": True,
            "data": data,
            "row_count": len(data)
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e)
        }
```

### 3. **AI Tool Integration**
Register the SQL function as an AI tool:

```python
def internal_execute_sql_query(sql_query: str) -> dict:
    """Tool for AI to execute SQL queries against BigQuery.
    
    Args:
        sql_query: The SQL query to execute
        
    Returns:
        dict: Query results with data and metadata
    """
    result = execute_query(sql_query)
    
    if result["success"]:
        return {
            "status": "success",
            "data": result["data"],
            "display_type": "table",
            "display_title": f"Query Results ({result['row_count']} rows)"
        }
    else:
        return {
            "status": "error", 
            "error_message": result["error"]
        }

# Register with AI system
gemini_tools = [internal_execute_sql_query]
```

### 4. **Frontend Data Display**
In your React frontend, handle the data response:

```typescript
interface QueryResult {
  status: string;
  data: Record<string, any>[];
  display_type: string;
  display_title: string;
}

// Render data table
function DataTable({ data }: { data: QueryResult }) {
  if (data.status === "success") {
    return (
      <table>
        <thead>
          <tr>
            {Object.keys(data.data[0] || {}).map(key => (
              <th key={key}>{key}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {data.data.map((row, index) => (
            <tr key={index}>
              {Object.values(row).map((value, i) => (
                <td key={i}>{value}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    );
  }
  return <div>Error: {data.error_message}</div>;
}
```

## Sample Queries That Work

### Basic Data Retrieval
- "Show me 10 rows from the customers table"
- "What data is in the orders table?"
- "Display recent sales data"

### Filtered Queries  
- "Show customers from California"
- "Find orders over $1000"
- "Display users who joined last month"

### Analytics Queries
- "Count total customers by state"
- "Average order value by month" 
- "Top 10 products by revenue"

## Key Differences from Your Current Setup

1. **Direct SQL Execution**: Your dashboard shows metadata; this system executes actual SQL queries
2. **AI Query Generation**: Instead of manual SQL, AI converts natural language to queries
3. **Real-time Data Access**: Live connection to BigQuery, not cached metadata
4. **Interactive Tables**: Users can drill down from high-level requests to specific data

## Environment Variables Needed

```bash
GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
GOOGLE_PROJECT_ID=your-project-id
KBC_WORKSPACE_ID=your-workspace-id
BIGQUERY_DATASET=your-dataset-name
```

## Authentication Requirements

1. **Google Cloud Service Account** with BigQuery permissions
2. **Keboola Cloud API Token** for metadata access
3. **Project access** to the specific BigQuery dataset

The key breakthrough is moving from metadata queries (`INFORMATION_SCHEMA.TABLES`) to actual data queries (`SELECT * FROM table_name`), combined with AI that can intelligently construct these queries from natural language requests.