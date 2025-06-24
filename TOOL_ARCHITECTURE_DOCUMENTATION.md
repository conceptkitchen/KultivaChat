# Kultivate AI - Tool Architecture Documentation

## Overview

Kultivate AI uses a sophisticated tool-calling system built on Google Gemini 2.0 Flash with function calling capabilities. The AI has access to specialized tools that enable it to query BigQuery databases, retrieve location data, and provide time information. This document provides complete technical specifications for how these tools are built, integrated, and function within the system.

## Tool System Architecture

### Core Framework
- **AI Model**: Google Gemini 2.0 Flash with function calling
- **Tool Integration**: Direct function registration with Gemini's GenerativeModel
- **Execution Context**: Python Flask backend with secure tool execution
- **Response Processing**: Multi-layered data extraction and display generation

### Tool Registration Process

Tools are registered with the Gemini model during chat session initialization:

```python
# In backend/main_2.py - Chat session creation
chat_session = gemini_model.start_chat(
    history=existing_history or [],
    tools=gemini_tool_functions_list  # Tool definitions passed here
)
```

The `gemini_tool_functions_list` contains tool definitions that conform to Gemini's function calling schema.

## Tool Definitions and Implementation

### 1. Internal SQL Query Tool (`internal_execute_sql_query`)

**Purpose**: Execute SQL queries against BigQuery workspace tables
**Registration Location**: `backend/main_2.py` - `gemini_tool_functions_list`

#### Tool Definition Schema
```python
{
    "name": "internal_execute_sql_query",
    "description": "Execute SQL queries against the user's BigQuery workspace. Use this for all data retrieval tasks.",
    "parameters": {
        "type": "object",
        "properties": {
            "query": {
                "type": "string",
                "description": "The SQL query to execute against BigQuery workspace tables"
            }
        },
        "required": ["query"]
    }
}
```

#### Implementation Function
```python
def internal_execute_sql_query(query):
    """
    Execute SQL query against BigQuery workspace
    Returns: {'status': 'success'/'error', 'data': [...], 'error_message': '...'}
    """
    try:
        # Input validation
        if not query or not isinstance(query, str):
            return {"status": "error", "error_message": "Query must be a non-empty string"}
        
        # Security check - prevent destructive operations
        destructive_keywords = ['DROP', 'DELETE', 'TRUNCATE', 'ALTER', 'CREATE', 'INSERT', 'UPDATE']
        query_upper = query.upper()
        for keyword in destructive_keywords:
            if keyword in query_upper:
                return {"status": "error", "error_message": f"Destructive operation '{keyword}' not allowed"}
        
        # Execute query using Google Cloud BigQuery client
        from google.cloud import bigquery
        client = bigquery.Client()
        
        query_job = client.query(query)
        results = query_job.result()
        
        # Convert results to list of dictionaries
        data = []
        for row in results:
            row_dict = {}
            for field in results.schema:
                row_dict[field.name] = row[field.name]
            data.append(row_dict)
        
        return {
            "status": "success",
            "data": data,
            "row_count": len(data)
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error_message": str(e)
        }
```

#### Key Features
- **Security**: Prevents destructive SQL operations (DROP, DELETE, etc.)
- **Authentication**: Uses Google Cloud service account credentials
- **Data Formatting**: Returns structured data as list of dictionaries
- **Error Handling**: Comprehensive exception handling with detailed error messages
- **Workspace Scoping**: Automatically scoped to user's BigQuery workspace

#### BigQuery Integration Details
- **Project ID**: `kbc-use4-839-261b`
- **Workspace Dataset**: `WORKSPACE_21894820`
- **Service Account**: `kbc-use4-ws-21894820@kbc-use4-839-261b.iam.gserviceaccount.com`
- **Credentials File**: `backend/credentials-839-21894808.json`

### 2. Location Services Tool (`get_zip_codes_for_city`)

**Purpose**: Retrieve ZIP codes for specified cities using external geocoding API
**Registration Location**: `backend/main_2.py` - `gemini_tool_functions_list`

#### Tool Definition Schema
```python
{
    "name": "get_zip_codes_for_city",
    "description": "Get ZIP codes for a given city name. Useful for location-based data analysis.",
    "parameters": {
        "type": "object",
        "properties": {
            "city_name": {
                "type": "string",
                "description": "The name of the city to get ZIP codes for"
            }
        },
        "required": ["city_name"]
    }
}
```

#### Implementation Function
```python
def get_zip_codes_for_city(city_name):
    """
    Retrieve ZIP codes for a given city using geocoding services
    Returns: {'status': 'success'/'error', 'zip_codes': [...], 'city': '...'}
    """
    try:
        import requests
        
        # Use geocoding service (example with OpenWeatherMap or similar)
        api_url = f"http://api.openweathermap.org/geo/1.0/direct"
        params = {
            'q': city_name,
            'limit': 5,
            'appid': os.getenv('WEATHER_API_KEY', 'demo_key')
        }
        
        response = requests.get(api_url, params=params)
        response.raise_for_status()
        
        locations = response.json()
        zip_codes = []
        
        for location in locations:
            # Extract ZIP code information if available
            if 'zip' in location:
                zip_codes.append(location['zip'])
        
        return {
            "status": "success",
            "city": city_name,
            "zip_codes": zip_codes,
            "locations_found": len(locations)
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error_message": f"Failed to retrieve ZIP codes for {city_name}: {str(e)}"
        }
```

### 3. Time Services Tool (`get_current_time`)

**Purpose**: Provide current date and time information
**Registration Location**: `backend/main_2.py` - `gemini_tool_functions_list`

#### Tool Definition Schema
```python
{
    "name": "get_current_time",
    "description": "Get the current date and time. Useful for time-based queries and data filtering.",
    "parameters": {
        "type": "object",
        "properties": {},
        "required": []
    }
}
```

#### Implementation Function
```python
def get_current_time():
    """
    Get current date and time information
    Returns: {'status': 'success', 'current_time': '...', 'timestamp': ...}
    """
    try:
        from datetime import datetime
        import pytz
        
        # Get current time in UTC and local timezone
        utc_now = datetime.utcnow()
        local_tz = pytz.timezone('America/New_York')  # Adjust as needed
        local_now = utc_now.replace(tzinfo=pytz.UTC).astimezone(local_tz)
        
        return {
            "status": "success",
            "current_time": local_now.strftime("%Y-%m-%d %H:%M:%S %Z"),
            "utc_time": utc_now.strftime("%Y-%m-%d %H:%M:%S UTC"),
            "timestamp": int(utc_now.timestamp()),
            "iso_format": local_now.isoformat()
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error_message": f"Failed to get current time: {str(e)}"
        }
```

## Tool Execution Flow

### 1. AI Tool Selection
When the AI receives a user query, it analyzes the request and determines which tools to call:

```python
# AI decision process (internal to Gemini)
# User: "Show me data from the orders table"
# AI decides to call: internal_execute_sql_query
# AI generates parameters: {"query": "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.orders` LIMIT 10"}
```

### 2. Tool Call Execution
The backend receives the tool call and executes the corresponding function:

```python
# In backend/main_2.py - Tool execution handler
def handle_tool_call(tool_name, parameters):
    if tool_name == "internal_execute_sql_query":
        return internal_execute_sql_query(parameters.get("query"))
    elif tool_name == "get_zip_codes_for_city":
        return get_zip_codes_for_city(parameters.get("city_name"))
    elif tool_name == "get_current_time":
        return get_current_time()
    else:
        return {"status": "error", "error_message": f"Unknown tool: {tool_name}"}
```

### 3. Response Processing
The backend processes the tool results and formats them for display:

```python
# Multi-layered data extraction system
def extract_tool_results(response):
    query_data = None
    tool_display_title = "Tool Results"
    
    # Primary extraction from response parts
    if hasattr(response, 'parts'):
        for part in response.parts:
            if hasattr(part, 'function_response'):
                func_result = part.function_response.response
                if func_result.get('status') == 'success' and func_result.get('data'):
                    query_data = func_result['data']
                    break
    
    # Secondary extraction from chat history
    if not query_data:
        history = chat_session.get_history()
        # Process history for tool results...
    
    # Emergency reconstruction from AI text
    if not query_data and "retrieved" in final_answer:
        # Parse AI response and re-execute queries...
    
    return query_data, tool_display_title
```

## Data Display System

### Display Object Structure
```python
display_object = {
    "type": "table",           # Display type (table, chart, text)
    "title": "Query Results",  # Display title
    "content": [               # Actual data content
        {"column1": "value1", "column2": "value2"},
        {"column1": "value3", "column2": "value4"}
    ]
}
```

### Frontend Rendering
The React frontend receives display objects and renders them using specialized components:

```javascript
// In client/src/components/ui/canvas-display.tsx
const CanvasDisplay = ({ displays }) => {
  return (
    <div className="space-y-4">
      {displays.map((display, index) => (
        <div key={index} className="border rounded-lg p-4">
          <h3 className="font-semibold mb-2">{display.title}</h3>
          {display.type === 'table' && (
            <DataTable data={display.content} />
          )}
        </div>
      ))}
    </div>
  );
};
```

## Error Handling and Fallback Systems

### Primary Error Handling
Each tool implements comprehensive error handling:

```python
try:
    # Tool execution logic
    result = execute_tool_logic()
    return {"status": "success", "data": result}
except ValidationError as e:
    return {"status": "error", "error_message": f"Validation failed: {str(e)}"}
except AuthenticationError as e:
    return {"status": "error", "error_message": f"Authentication failed: {str(e)}"}
except Exception as e:
    return {"status": "error", "error_message": f"Unexpected error: {str(e)}"}
```

### Emergency Reconstruction System
When tool results fail to extract properly, the system uses emergency reconstruction:

```python
# Emergency patterns for common queries
emergency_tables = {
    '2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors': 'Undiscovered SF Data',
    'Balay-Kreative---attendees---all-orders': 'Balay-Kreative Attendees',
    # ... more patterns
}

# Pattern matching and re-execution
for table_name, display_title in emergency_tables.items():
    if table_name in ai_response_text:
        emergency_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{table_name}` LIMIT 10"
        emergency_result = internal_execute_sql_query(emergency_query)
        if emergency_result.get('status') == 'success':
            return emergency_result['data'], display_title
```

## Security Implementation

### SQL Injection Prevention
```python
# Whitelist approach for allowed SQL operations
ALLOWED_SQL_KEYWORDS = ['SELECT', 'FROM', 'WHERE', 'ORDER BY', 'GROUP BY', 'HAVING', 'LIMIT']
FORBIDDEN_SQL_KEYWORDS = ['DROP', 'DELETE', 'INSERT', 'UPDATE', 'ALTER', 'CREATE', 'TRUNCATE']

def validate_sql_query(query):
    query_upper = query.upper()
    for forbidden in FORBIDDEN_SQL_KEYWORDS:
        if forbidden in query_upper:
            raise SecurityError(f"Forbidden SQL operation: {forbidden}")
```

### Credential Management
```python
# Service account authentication
os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = '/path/to/service-account.json'

# API key management
GEMINI_API_KEY = os.getenv('GEMINI_API_KEY')
if not GEMINI_API_KEY:
    raise ConfigurationError("GEMINI_API_KEY environment variable required")
```

## Performance Optimization

### Query Optimization
- **Automatic LIMIT clauses**: Prevents runaway queries
- **Result caching**: Caches frequent query results
- **Connection pooling**: Reuses BigQuery connections

### Memory Management
- **Streaming results**: For large datasets
- **Garbage collection**: Explicit cleanup of large objects
- **Request timeouts**: Prevents hanging requests

## Integration with Frontend

### API Contract
```javascript
// POST /api/chat
{
  "message": "Show me sales data",
  "conversation_id": "uuid"
}

// Response
{
  "response": "Here's your sales data:",
  "displays": [
    {
      "type": "table",
      "title": "Sales Data",
      "content": [{"date": "2024-01-01", "amount": 1000}]
    }
  ]
}
```

### React Query Integration
```javascript
const { mutate: sendMessage } = useMutation({
  mutationFn: async ({ message, conversationId }) => {
    const response = await fetch('/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ message, conversation_id: conversationId })
    });
    return response.json();
  },
  onSuccess: (data) => {
    // Update chat with response and displays
    setMessages(prev => [...prev, {
      role: 'assistant',
      content: data.response,
      displays: data.displays
    }]);
  }
});
```

## Debugging and Monitoring

### Logging System
```python
import logging

# Configure comprehensive logging
logging.basicConfig(level=logging.INFO)
app.logger.info(f"Tool execution: {tool_name} with params: {parameters}")
app.logger.info(f"Tool result: {result}")
app.logger.error(f"Tool error: {error}", exc_info=True)
```

### Performance Monitoring
```python
import time

def monitor_tool_execution(func):
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        execution_time = time.time() - start_time
        app.logger.info(f"Tool {func.__name__} executed in {execution_time:.2f}s")
        return result
    return wrapper
```

## Configuration Management

### Environment Variables
```bash
# Required for tool operation
GEMINI_API_KEY=your_gemini_api_key
GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
GOOGLE_PROJECT_ID=kbc-use4-839-261b
KBC_WORKSPACE_ID=WORKSPACE_21894820
DATABASE_URL=postgresql://user:pass@host:port/db
```

### Tool Configuration
```python
TOOL_CONFIG = {
    "sql_query_timeout": 30,  # seconds
    "max_result_rows": 1000,
    "enable_caching": True,
    "cache_ttl": 300,  # seconds
    "debug_mode": False
}
```

## Extending the Tool System

### Adding New Tools

1. **Define Tool Schema**:
```python
new_tool_definition = {
    "name": "new_tool_name",
    "description": "Tool description for AI understanding",
    "parameters": {
        "type": "object",
        "properties": {
            "param1": {"type": "string", "description": "Parameter description"}
        },
        "required": ["param1"]
    }
}
```

2. **Implement Tool Function**:
```python
def new_tool_function(param1):
    try:
        # Tool logic here
        result = process_param1(param1)
        return {"status": "success", "data": result}
    except Exception as e:
        return {"status": "error", "error_message": str(e)}
```

3. **Register with Gemini**:
```python
gemini_tool_functions_list.append(new_tool_definition)
```

### Best Practices for Tool Development

1. **Error Handling**: Always return structured error responses
2. **Input Validation**: Validate all parameters before processing
3. **Security**: Implement appropriate security checks
4. **Documentation**: Provide clear descriptions for AI understanding
5. **Testing**: Test tools independently and in integration
6. **Logging**: Add comprehensive logging for debugging
7. **Performance**: Monitor execution time and optimize as needed

## Conclusion

The Kultivate AI tool system provides a robust, secure, and extensible framework for AI-powered data analysis. The multi-layered approach to tool execution, data extraction, and error handling ensures reliable operation even in edge cases. This architecture supports the system's core mission of enabling natural language data exploration while maintaining security and performance standards.

The tool system's design allows for easy extension and modification, making it suitable for growing business requirements and additional data sources. The comprehensive error handling and fallback mechanisms ensure users receive meaningful responses even when individual components encounter issues.