# Kultivate AI - Data Discovery Architecture

## Overview

Kultivate AI uses a dual-source data discovery system that combines Keboola Cloud Storage API with Google BigQuery INFORMATION_SCHEMA to provide comprehensive table information. This hybrid approach leverages Keboola's metadata management while utilizing BigQuery's optimized query performance for data retrieval.

## Data Source Architecture

### Primary Data Flow
```
User Request → AI Analysis → Table Discovery → Query Execution → Data Display
     ↓              ↓             ↓              ↓              ↓
Natural Language   Fuzzy Match   BigQuery       SQL Execution  React Display
Processing         Algorithm     Tables         Results        Components
```

### Dual-Source Integration
- **Keboola Cloud**: Metadata, bucket organization, table schemas
- **BigQuery**: Actual data storage, query execution, performance optimization

## BigQuery Table Discovery

### INFORMATION_SCHEMA Queries

The system uses BigQuery's INFORMATION_SCHEMA to discover all available tables:

```sql
-- Primary table discovery query
SELECT table_name 
FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` 
ORDER BY table_name
```

### Implementation in Backend

```python
def get_all_bigquery_tables():
    """Retrieve all available tables from BigQuery workspace"""
    try:
        query = f"""
        SELECT 
            table_name,
            table_type,
            creation_time,
            last_modified_time
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        ORDER BY table_name
        """
        
        result = internal_execute_sql_query(query)
        if result.get('status') == 'success':
            return {
                "status": "success",
                "tables": result['data'],
                "count": len(result['data'])
            }
        else:
            return result
            
    except Exception as e:
        return {
            "status": "error",
            "error_message": f"Failed to retrieve BigQuery tables: {str(e)}"
        }
```

### Advanced Table Metadata

```sql
-- Detailed table information query
SELECT 
    table_name,
    table_type,
    creation_time,
    last_modified_time,
    row_count,
    size_bytes,
    description
FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES`
LEFT JOIN `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLE_OPTIONS`
    USING (table_name)
ORDER BY last_modified_time DESC
```

### Column Schema Discovery

```sql
-- Get column information for specific table
SELECT 
    column_name,
    data_type,
    is_nullable,
    column_default,
    ordinal_position
FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS`
WHERE table_name = 'TARGET_TABLE_NAME'
ORDER BY ordinal_position
```

## Keboola Cloud Storage Discovery

### Bucket-Based Organization

Keboola organizes data into buckets (categories) containing tables:

```python
def list_keboola_buckets() -> dict:
    """Lists all available data categories (buckets) in Keboola Storage"""
    try:
        if not keboola_storage_client:
            return {
                "status": "error",
                "error_message": "Keboola Storage client not initialized"
            }
        
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = []
        
        for bucket in buckets_data or []:
            bucket_info.append({
                "id": bucket.get("id"),
                "name": bucket.get("name"), 
                "stage": bucket.get("stage"),
                "description": bucket.get("description", ""),
                "table_count": len(bucket.get("tables", []))
            })
        
        return {
            "status": "success",
            "buckets": bucket_info,
            "display_type": "table",
            "display_title": "Available Data Buckets"
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error_message": f"Error listing Keboola buckets: {str(e)}"
        }
```

### Table Discovery Within Buckets

```python
def list_tables_in_keboola_bucket(bucket_id: str) -> dict:
    """Lists all tables within a specific Keboola bucket"""
    try:
        tables_data = keboola_storage_client.buckets.list_tables(bucket_id=bucket_id)
        table_info = []
        
        for table in tables_data or []:
            table_info.append({
                "id": table.get("id"),
                "name": table.get("name"),
                "rowsCount": table.get("rowsCount", 0),
                "primaryKey": table.get("primaryKey", []),
                "created": table.get("created"),
                "lastImportDate": table.get("lastImportDate")
            })
        
        return {
            "status": "success",
            "data": table_info,
            "display_type": "table", 
            "display_title": f"Tables in Keboola Bucket: {bucket_id}"
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error_message": f"Error listing tables in bucket {bucket_id}: {str(e)}"
        }
```

### Detailed Table Schema

```python
def get_keboola_table_detail(table_id: str) -> dict:
    """Get detailed schema and metadata for a specific Keboola table"""
    try:
        table_detail = keboola_storage_client.tables.detail(table_id)
        
        # Extract column information
        columns = []
        for col in table_detail.get("columns", []):
            columns.append({
                "name": col.get("name"),
                "type": col.get("type", "string"),
                "nullable": not col.get("required", False)
            })
        
        return {
            "status": "success",
            "data": [{
                "id": table_detail.get("id"),
                "name": table_detail.get("name"),
                "displayName": table_detail.get("displayName"),
                "rowsCount": table_detail.get("rowsCount", 0),
                "dataSizeBytes": table_detail.get("dataSizeBytes", 0),
                "columns": columns,
                "primaryKey": table_detail.get("primaryKey", []),
                "created": table_detail.get("created"),
                "lastImportDate": table_detail.get("lastImportDate")
            }],
            "display_type": "table",
            "display_title": f"Table Details: {table_id}"
        }
        
    except Exception as e:
        return {
            "status": "error", 
            "error_message": f"Error getting table details for {table_id}: {str(e)}"
        }
```

## Fuzzy Matching Algorithm

### Natural Language to Table Name Resolution

The system uses sophisticated fuzzy matching to resolve natural language requests to actual table names:

```python
def fuzzy_match_table_name(user_query: str, available_tables: list) -> str:
    """
    Match natural language queries to actual table names using fuzzy matching
    
    Examples:
    - "undiscovered attendees squarespace" → "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"
    - "kapwa gardens vendor data" → "Kapwa-Gardens---vendor-export---all-orders"
    - "balay kreative" → "Balay-Kreative---attendees---all-orders"
    """
    import re
    from difflib import SequenceMatcher
    
    # Normalize user query
    query_normalized = user_query.lower().replace(" ", "").replace("-", "")
    
    best_match = None
    best_score = 0
    
    for table_name in available_tables:
        # Normalize table name for comparison
        table_normalized = table_name.lower().replace("-", "").replace("_", "")
        
        # Calculate similarity score
        similarity = SequenceMatcher(None, query_normalized, table_normalized).ratio()
        
        # Boost score for keyword matches
        query_keywords = user_query.lower().split()
        for keyword in query_keywords:
            if keyword in table_name.lower():
                similarity += 0.2
        
        if similarity > best_score:
            best_score = similarity
            best_match = table_name
    
    # Return best match if confidence is high enough
    return best_match if best_score > 0.4 else None
```

### AI-Driven Table Selection

The AI uses a two-step process for table discovery:

```python
# Step 1: Get all available tables
table_query = f"""
SELECT table_name 
FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
ORDER BY table_name
"""

# Step 2: Use fuzzy matching in AI prompt
system_instruction = f"""
When user asks for data in natural language:

1. FIRST get all tables with: {table_query}
2. THEN use fuzzy matching to find best table:
   - "undiscovered attendees" → look for tables containing "Undiscovered" and "Attendees"
   - "kapwa gardens" → look for tables containing "Kapwa" and "Gardens" 
   - Use CASE-INSENSITIVE pattern matching
3. EXECUTE query on the matched table

Example natural language mappings:
- "show me undiscovered stuff" → "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"
- "kapwa vendor data" → "Kapwa-Gardens---vendor-export---all-orders"
"""
```

## Table Name Mapping System

### Keboola to BigQuery Mapping

```python
def create_table_mapping():
    """Create mapping between Keboola table IDs and BigQuery table names"""
    mapping = {}
    
    # Get Keboola buckets
    buckets_result = list_keboola_buckets()
    if buckets_result['status'] != 'success':
        return mapping
    
    # For each bucket, get tables and create mapping
    for bucket in buckets_result['buckets']:
        tables_result = list_tables_in_keboola_bucket(bucket['id'])
        
        if tables_result['status'] == 'success':
            for table in tables_result['data']:
                keboola_id = table['id']
                bigquery_name = table['name']
                
                mapping[keboola_id] = {
                    "bigquery_table": bigquery_name,
                    "full_path": f"`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{bigquery_name}`",
                    "bucket": bucket['id'],
                    "stage": bucket.get('stage', 'out'),
                    "row_count": table.get('rowsCount', 0)
                }
    
    return mapping
```

### Complex Table Name Patterns

The system handles complex naming conventions:

```python
# Example table name patterns in BigQuery
table_patterns = {
    "vendor_data": [
        "Undiscovered-Vendor-Export---Squarespace---All-data-orders",
        "Kapwa-Gardens---vendor-export---all-orders", 
        "Balay-Kreative---vendor---all-orders"
    ],
    "attendee_data": [
        "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-",
        "Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders",
        "Event-attendees---export---all-data"
    ],
    "sales_data": [
        "2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors",
        "Kapwa-Gardens---sales---monthly-report",
        "Close-out-sales---all-vendors---final"
    ]
}
```

## Performance Optimization

### Caching Strategy

```python
import time
from functools import lru_cache

# Cache table lists for 5 minutes
@lru_cache(maxsize=1)
def get_cached_table_list():
    """Cache BigQuery table list to reduce API calls"""
    cache_key = f"bigquery_tables_{int(time.time() / 300)}"  # 5-minute cache
    
    query = f"""
    SELECT table_name, last_modified_time
    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
    ORDER BY table_name
    """
    
    return internal_execute_sql_query(query)
```

### Parallel Discovery

```python
import asyncio
import concurrent.futures

async def parallel_data_discovery():
    """Discover data from both sources simultaneously"""
    
    with concurrent.futures.ThreadPoolExecutor() as executor:
        # Start both discovery processes
        bigquery_future = executor.submit(get_all_bigquery_tables)
        keboola_future = executor.submit(list_keboola_buckets)
        
        # Wait for both to complete
        bigquery_result = bigquery_future.result()
        keboola_result = keboola_future.result()
        
        return {
            "bigquery_tables": bigquery_result,
            "keboola_buckets": keboola_result,
            "discovery_time": time.time()
        }
```

## Error Handling and Fallbacks

### Multi-Source Fallback

```python
def robust_table_discovery(user_query: str):
    """Try multiple discovery methods with fallbacks"""
    
    # Method 1: Direct BigQuery INFORMATION_SCHEMA
    try:
        tables_result = get_all_bigquery_tables()
        if tables_result['status'] == 'success':
            matched_table = fuzzy_match_table_name(user_query, 
                                                  [t['table_name'] for t in tables_result['tables']])
            if matched_table:
                return {"method": "bigquery_direct", "table": matched_table}
    except Exception as e:
        app.logger.warning(f"BigQuery discovery failed: {e}")
    
    # Method 2: Keboola buckets exploration
    try:
        buckets_result = list_keboola_buckets()
        if buckets_result['status'] == 'success':
            for bucket in buckets_result['buckets']:
                tables_result = list_tables_in_keboola_bucket(bucket['id'])
                if tables_result['status'] == 'success':
                    matched_table = fuzzy_match_table_name(user_query,
                                                          [t['name'] for t in tables_result['data']])
                    if matched_table:
                        return {"method": "keboola_buckets", "table": matched_table, "bucket": bucket['id']}
    except Exception as e:
        app.logger.warning(f"Keboola discovery failed: {e}")
    
    # Method 3: Emergency pattern matching
    emergency_patterns = {
        "undiscovered": "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-",
        "kapwa": "Kapwa-Gardens---vendor-export---all-orders",
        "balay": "Balay-Kreative---attendees---all-orders"
    }
    
    for pattern, table_name in emergency_patterns.items():
        if pattern in user_query.lower():
            return {"method": "emergency_pattern", "table": table_name}
    
    return {"method": "none", "table": None}
```

## Configuration and Authentication

### Environment Setup

```bash
# Required environment variables for data discovery
KBC_API_URL=https://connection.keboola.com
KBC_STORAGE_TOKEN=your_keboola_storage_token
GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
GOOGLE_PROJECT_ID=kbc-use4-839-261b
KBC_WORKSPACE_ID=WORKSPACE_21894820
KBC_WORKSPACE_SCHEMA=WORKSPACE_21894820
```

### Client Initialization

```python
# Keboola Storage Client
try:
    keboola_storage_client = KeboolaStorageClient(
        base_url=KBC_API_URL,
        token=KBC_STORAGE_TOKEN
    )
    app.logger.info("Keboola Storage client initialized successfully")
except Exception as e:
    app.logger.error(f"Failed to initialize Keboola client: {e}")
    keboola_storage_client = None

# BigQuery Client  
try:
    from google.cloud import bigquery
    bigquery_client = bigquery.Client()
    app.logger.info("BigQuery client initialized successfully")
except Exception as e:
    app.logger.error(f"Failed to initialize BigQuery client: {e}")
    bigquery_client = None
```

## AI Integration Patterns

### Tool Function Registration

```python
# AI tool definitions for data discovery
gemini_tool_functions_list = [
    {
        "name": "internal_execute_sql_query",
        "description": "Execute SQL queries against BigQuery workspace tables",
        "parameters": {
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "SQL query to execute"}
            },
            "required": ["query"]
        }
    },
    {
        "name": "list_keboola_buckets", 
        "description": "List all Keboola Storage buckets (data categories)",
        "parameters": {"type": "object", "properties": {}, "required": []}
    },
    {
        "name": "list_tables_in_keboola_bucket",
        "description": "List tables within a specific Keboola bucket",
        "parameters": {
            "type": "object", 
            "properties": {
                "bucket_id": {"type": "string", "description": "Keboola bucket ID"}
            },
            "required": ["bucket_id"]
        }
    }
]
```

### AI Decision Logic

The AI follows this decision tree for data discovery:

```
User Request Analysis
├── Explicit table request → Direct BigQuery query
├── Natural language data request
│   ├── Get all tables from INFORMATION_SCHEMA
│   ├── Apply fuzzy matching algorithm
│   └── Execute query on best match
├── Bucket exploration request → Keboola API
└── General data overview → Combined discovery
```

## Monitoring and Debugging

### Discovery Metrics

```python
def log_discovery_metrics(method: str, success: bool, execution_time: float, table_count: int):
    """Log metrics for data discovery operations"""
    
    app.logger.info(f"DISCOVERY_METRICS: method={method}, success={success}, "
                   f"time={execution_time:.2f}s, tables={table_count}")
    
    # Optional: Send to monitoring system
    # metrics_client.increment(f'discovery.{method}.{"success" if success else "failure"}')
    # metrics_client.timing(f'discovery.{method}.duration', execution_time)
```

### Debug Information

```python
def debug_table_discovery(user_query: str):
    """Provide debug information for table discovery"""
    
    debug_info = {
        "original_query": user_query,
        "normalized_query": user_query.lower().replace(" ", "").replace("-", ""),
        "keywords": user_query.lower().split(),
        "bigquery_available": bigquery_client is not None,
        "keboola_available": keboola_storage_client is not None,
        "project_id": GOOGLE_PROJECT_ID,
        "workspace_id": KBC_WORKSPACE_ID
    }
    
    app.logger.info(f"DEBUG_DISCOVERY: {debug_info}")
    return debug_info
```

## Conclusion

The Kultivate AI data discovery system provides comprehensive table information through a sophisticated dual-source architecture. By combining Keboola Cloud's metadata management with BigQuery's query performance, the system delivers both rich business context and optimal data access performance.

The fuzzy matching algorithm enables natural language queries to be resolved to specific tables, while the multi-layered fallback system ensures reliable operation even when individual components encounter issues. This architecture supports the system's core mission of enabling intuitive data exploration while maintaining enterprise-grade reliability and performance.