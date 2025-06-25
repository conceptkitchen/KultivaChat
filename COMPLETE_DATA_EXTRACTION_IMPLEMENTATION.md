# Complete Data Extraction Implementation Guide

Based on your progress, here's the exact working code pattern from this Kultivate AI project that solves the data extraction and display issue.

## The Complete Backend Implementation

### 1. Chat Endpoint with Data Extraction
```python
@app.route('/api/chat', methods=['POST'])
def handle_chat():
    try:
        # Get user message
        request_data = request.get_json()
        user_message = request_data['message']
        
        # Send to AI with tools
        response = ai_client.send_message_with_tools(user_message)
        
        # Extract data from AI tool responses
        displays = extract_data_from_ai_response(response)
        
        # Return structured response
        return jsonify({
            "reply": response.text,
            "displays": displays
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500
```

### 2. Data Extraction Function (Key Implementation)
```python
def extract_data_from_ai_response(ai_response):
    """Extract actual data from AI tool calls - THIS IS THE CRITICAL FUNCTION"""
    displays = []
    query_data = None
    
    try:
        # Get AI's chat history to find tool responses
        history = ai_response.get_history()
        
        # Look through messages in reverse order
        for msg in reversed(history):
            if hasattr(msg, 'parts') and msg.parts:
                for part in msg.parts:
                    if hasattr(part, 'function_response') and part.function_response:
                        tool_name = part.function_response.name
                        tool_result = part.function_response.response
                        
                        # Check for SQL query tool
                        if tool_name == 'execute_sql_query':
                            if isinstance(tool_result, dict):
                                status = tool_result.get('status')
                                data = tool_result.get('data')
                                
                                if status == 'success' and data and isinstance(data, list):
                                    query_data = data
                                    break
            if query_data:
                break
    except Exception as e:
        print(f"Data extraction failed: {e}")
    
    # Create display objects from extracted data
    if query_data and isinstance(query_data, list) and query_data:
        displays.append({
            "type": "table",
            "title": "Query Results",
            "content": query_data
        })
    
    return displays
```

### 3. SQL Execution Tool
```python
def execute_sql_query(sql_query: str) -> dict:
    """SQL tool that AI can call"""
    try:
        # Execute against your database
        query_job = bigquery_client.query(sql_query)
        results = query_job.result()
        
        # Convert to list of dictionaries
        rows = []
        for row in results:
            row_dict = {}
            for key, value in dict(row).items():
                row_dict[key] = value
            rows.append(row_dict)
        
        return {
            "status": "success",
            "data": rows
        }
    except Exception as e:
        return {
            "status": "error",
            "error_message": str(e)
        }
```

### 4. AI Tool Registration
```python
# Register the SQL tool with your AI
ai_tools = [
    {
        "name": "execute_sql_query",
        "description": "Execute SQL queries against BigQuery",
        "parameters": {
            "type": "object",
            "properties": {
                "sql_query": {
                    "type": "string",
                    "description": "The SQL query to execute"
                }
            },
            "required": ["sql_query"]
        },
        "function": execute_sql_query
    }
]
```

### 5. Updated AI Instructions
```
You are a data analyst. When users ask for data:

1. Execute SQL queries using execute_sql_query tool
2. DO NOT describe what "would be shown" 
3. Simply say: "I found your data! The results are displayed below."
4. The system will automatically extract and display your query results

Example:
User: "Show me Balay Kreative data"
You: Execute: SELECT * FROM balay_kreative_table LIMIT 10
You: "I found your Balay Kreative data! The results are displayed below."
```

## Frontend Implementation

### 6. Frontend Response Handling
```javascript
async function sendMessage(message) {
    const response = await fetch('/api/chat', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message })
    });
    
    const data = await response.json();
    
    // Display AI reply
    displayMessage(data.reply, 'assistant');
    
    // Render any data tables
    if (data.displays) {
        data.displays.forEach(display => {
            if (display.type === 'table') {
                renderTable(display.content, display.title);
            }
        });
    }
}

function renderTable(data, title) {
    if (!data || data.length === 0) return;
    
    // Create table HTML
    const table = document.createElement('table');
    table.className = 'data-table';
    
    // Header row
    const headerRow = table.insertRow();
    Object.keys(data[0]).forEach(key => {
        const th = document.createElement('th');
        th.textContent = key;
        headerRow.appendChild(th);
    });
    
    // Data rows
    data.forEach(row => {
        const tr = table.insertRow();
        Object.values(row).forEach(value => {
            const td = tr.insertCell();
            td.textContent = value;
        });
    });
    
    // Add to chat
    const container = document.createElement('div');
    container.innerHTML = `<h4>${title}</h4>`;
    container.appendChild(table);
    document.getElementById('chat-container').appendChild(container);
}
```

## Key Implementation Points

1. **Data Extraction Location**: Extract data from AI's chat history after it executes tools
2. **Response Structure**: Always return `{reply, displays}` format
3. **Display Objects**: Use `{type: "table", title: "...", content: [...]}` structure
4. **AI Instructions**: Focus on execution, not description
5. **Frontend Rendering**: Handle displays array separately from text reply

## Debugging Tips

1. **Log Tool Responses**: Always log what your AI tool returns
2. **Check Data Format**: Ensure extracted data is array of objects
3. **Verify Tool Registration**: Make sure AI can actually call your SQL tool
4. **Test Extraction**: Log whether data extraction finds anything

This is the exact pattern that makes data tables appear in the chat interface instead of just text descriptions.