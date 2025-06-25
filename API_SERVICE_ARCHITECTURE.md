# Kultivate AI as API Service - Complete Implementation Guide

## Overview
Transform your Kultivate AI project into a standalone API service that your other product can consume. The current architecture is already perfect for this - you just need to expose the right endpoints.

## Current Architecture Analysis
Your system already has:
- ✅ Python backend (Flask) with AI and data processing
- ✅ Node.js proxy server handling requests
- ✅ Keboola Cloud and BigQuery integration
- ✅ Data extraction and display formatting
- ✅ Authentication and session management

## API Service Implementation

### 1. Core Data Query API Endpoint

```javascript
// Add to server/index.ts
app.post('/api/v1/data/query', async (req: Request, res: Response) => {
  try {
    const { query, credentials } = req.body;
    
    // Validate request
    if (!query) {
      return res.status(400).json({ error: 'Query parameter required' });
    }
    
    // Forward to Python backend with credentials
    const response = await fetch('http://localhost:8081/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        message: query,
        api_mode: true,
        credentials: credentials // Pass through Keboola/BigQuery creds
      })
    });
    
    const data = await response.json();
    
    // Return structured API response
    res.json({
      success: true,
      query: query,
      response: data.reply,
      data: data.displays || [],
      timestamp: new Date().toISOString()
    });
    
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});
```

### 2. Direct SQL Execution Endpoint

```javascript
// Add to server/index.ts
app.post('/api/v1/data/sql', async (req: Request, res: Response) => {
  try {
    const { sql, credentials } = req.body;
    
    if (!sql) {
      return res.status(400).json({ error: 'SQL query required' });
    }
    
    // Direct SQL execution through backend
    const response = await fetch('http://localhost:8081/api/execute_sql', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sql_query: sql,
        credentials: credentials
      })
    });
    
    const result = await response.json();
    
    res.json({
      success: result.status === 'success',
      data: result.data || [],
      error: result.error_message || null,
      rows_returned: result.data ? result.data.length : 0,
      timestamp: new Date().toISOString()
    });
    
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});
```

### 3. Table Discovery Endpoint

```javascript
// Add to server/index.ts
app.get('/api/v1/data/tables', async (req: Request, res: Response) => {
  try {
    const { credentials } = req.body;
    
    // Get all available tables
    const response = await fetch('http://localhost:8081/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        message: "show me all available tables",
        api_mode: true,
        credentials: credentials
      })
    });
    
    const data = await response.json();
    
    res.json({
      success: true,
      tables: data.displays || [],
      total_tables: data.displays ? data.displays.length : 0,
      timestamp: new Date().toISOString()
    });
    
  } catch (error) {
    res.status(500).json({
      success: false,
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});
```

### 4. Backend API Mode Support

Add to `backend/main_2.py`:

```python
@app.route('/api/execute_sql', methods=['POST'])
def execute_sql_direct():
    """Direct SQL execution for API consumers"""
    try:
        data = request.json
        sql_query = data.get('sql_query')
        credentials = data.get('credentials', {})
        
        if not sql_query:
            return jsonify({'status': 'error', 'error_message': 'SQL query required'})
        
        # Execute SQL with provided credentials
        result = internal_execute_sql_query(sql_query)
        
        return jsonify(result)
        
    except Exception as e:
        return jsonify({
            'status': 'error',
            'error_message': str(e)
        })

@app.route('/api/chat', methods=['POST'])
def handle_chat():
    """Enhanced chat endpoint with API mode support"""
    try:
        request_data = request.get_json()
        user_message = request_data['message']
        api_mode = request_data.get('api_mode', False)
        credentials = request_data.get('credentials', {})
        
        # Set credentials if provided
        if credentials:
            # Update environment with provided credentials
            os.environ.update(credentials)
        
        # Process message through AI
        # ... existing chat logic ...
        
        # Return appropriate format
        if api_mode:
            return jsonify({
                "success": True,
                "reply": final_answer,
                "displays": displays,
                "api_mode": True
            })
        else:
            return jsonify({
                "reply": final_answer,
                "displays": displays
            })
            
    except Exception as e:
        return jsonify({
            "success": False,
            "error": str(e)
        })
```

## Usage Examples for Your Other Product

### 1. Natural Language Data Query

```javascript
// In your other product
async function queryKultivatAI(question) {
  const response = await fetch('https://Kultivate-chat-ck.replit.app/api/v1/data/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      query: question,
      credentials: {
        KBC_STORAGE_TOKEN: 'your-token',
        GEMINI_API_KEY: 'your-key'
      }
    })
  });
  
  const data = await response.json();
  return data;
}

// Usage
const result = await queryKultivatAI("Show me Balay Kreative customer analytics");
console.log(result.response); // AI explanation
console.log(result.data); // Actual table data
```

### 2. Direct SQL Execution

```javascript
async function executeSQLQuery(sql) {
  const response = await fetch('https://Kultivate-chat-ck.replit.app/api/v1/data/sql', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      sql: sql,
      credentials: {
        GOOGLE_APPLICATION_CREDENTIALS: 'your-service-account.json'
      }
    })
  });
  
  return await response.json();
}

// Usage
const customers = await executeSQLQuery("SELECT * FROM balay_kreative_customers LIMIT 10");
```

### 3. Table Discovery

```javascript
async function getAvailableTables() {
  const response = await fetch('https://Kultivate-chat-ck.replit.app/api/v1/data/tables', {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      credentials: {
        KBC_STORAGE_TOKEN: 'your-token'
      }
    })
  });
  
  return await response.json();
}
```

## Deployment as API Service

### 1. Update replit.toml for API Service

```toml
[deployment]
run = ["npm", "run", "start"]
deploymentTarget = "autoscale"

[[ports]]
localPort = 5000
externalPort = 80
```

### 2. Environment Variables for API Service

```bash
# Add these to Replit secrets
KBC_API_URL=https://connection.keboola.com
KBC_STORAGE_TOKEN=your-default-token
GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
GEMINI_API_KEY=your-gemini-key
DATABASE_URL=your-postgres-url
SESSION_SECRET=your-session-secret
API_MODE=true
```

### 3. API Authentication (Optional)

```javascript
// Add API key authentication
app.use('/api/v1', (req, res, next) => {
  const apiKey = req.headers['x-api-key'];
  
  if (!apiKey || apiKey !== process.env.API_KEY) {
    return res.status(401).json({ error: 'Invalid API key' });
  }
  
  next();
});
```

## Benefits of This Approach

1. **Reuse Existing Logic**: All your AI and data processing logic stays the same
2. **Flexible Integration**: Your other product just makes HTTP requests
3. **Credential Management**: Each request can provide its own credentials
4. **Scalable**: Can handle multiple products consuming the same API
5. **Maintainable**: One service handles all Keboola/BigQuery complexity

## Next Steps

1. Add the API endpoints to your server/index.ts
2. Update backend to support API mode
3. Deploy as separate service
4. Test from your other product
5. Add authentication if needed

This transforms your Kultivate AI into a powerful data API that any product can consume!