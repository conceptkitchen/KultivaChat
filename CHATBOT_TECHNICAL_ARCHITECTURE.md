# Kultivate AI Chatbot - Technical Architecture Documentation

## Overview

Kultivate AI is an intelligent data platform that enables natural language interaction with enterprise data through a sophisticated chatbot system. The chatbot serves as the primary interface between users and their data stored in Keboola Cloud and Google BigQuery, utilizing Google's Gemini 2.0 Flash AI model for natural language processing and intelligent tool execution.

## System Architecture

### Core Components

1. **Frontend Chat Interface** (React/TypeScript)
2. **Proxy Server** (Node.js/Express) 
3. **AI Backend** (Python/Flask with Gunicorn)
4. **Database Layer** (PostgreSQL)
5. **External Data Sources** (Keboola Cloud, BigQuery)
6. **AI Engine** (Google Gemini 2.0 Flash)

### Component Interaction Flow

```
User Input → React Chat → Node.js Proxy → Python Backend → Gemini AI → Data Sources → Response Chain
```

## Detailed Component Breakdown

### 1. Frontend Chat Interface (`client/src/components/chat.tsx`)

**Purpose**: Provides real-time chat interface with conversation management

**Key Features**:
- Real-time message display with typing indicators
- Canvas display system for data visualizations and tables
- Conversation history and sidebar navigation
- Authentication integration
- Message persistence across sessions

**Technical Implementation**:
- Built with React 18 and TypeScript
- Uses TanStack Query for state management and API calls
- Wouter for client-side routing
- Tailwind CSS with shadcn/ui components
- Real-time updates through HTTP polling

**API Integration**:
```typescript
// Message sending flow
const mutation = useMutation({
  mutationFn: async (message: string) => {
    return apiRequest(`/api/chat`, {
      method: 'POST',
      body: { message, conversationId }
    });
  }
});
```

### 2. Proxy Server (`server/index.ts`)

**Purpose**: Routes requests between frontend and Python backend, handles authentication

**Key Features**:
- HTTP proxy routing with error handling
- Backend health monitoring and readiness detection
- Session management and authentication
- Static file serving for React build
- Request/response logging and debugging

**Technical Implementation**:
```typescript
class PythonBackendService {
  private backendProcess: ChildProcess | null = null;
  private isReady: boolean = false;

  start() {
    this.backendProcess = spawn('gunicorn', [
      '--bind', '0.0.0.0:8081',
      '--timeout', '300',
      'wsgi:app'
    ]);
  }

  isBackendReady(): boolean {
    return this.backendProcess && !this.backendProcess.killed;
  }
}
```

**Proxy Logic**:
- Intercepts `/api/*` routes and forwards to Python backend on port 8081
- Handles JSON parsing and error recovery
- Implements retry logic for backend connectivity

### 3. AI Backend (`backend/main_2.py`)

**Purpose**: Core AI processing engine with data source integration

**Key Architecture**:
- Flask application with Gunicorn WSGI server
- Google Gemini 2.0 Flash integration with function calling
- Keboola Cloud API client for data bucket operations
- Google BigQuery client for SQL query execution
- PostgreSQL integration for conversation persistence

**AI Tool System**:
The backend implements a sophisticated tool system that allows Gemini AI to execute functions:

```python
gemini_tool_functions_list = [
    internal_execute_sql_query,
    get_zip_codes_for_city,
    get_current_time
]

# Function definitions for Gemini
tool_function_declarations = [
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
    }
]
```

**Data Processing Pipeline**:
1. Receive user message from proxy
2. Load conversation history from PostgreSQL
3. Create Gemini chat session with tools and context
4. Process AI response and extract function calls
5. Execute tool functions (SQL queries, data retrieval)
6. Format response with displays and visualizations
7. Save conversation to database
8. Return structured response to frontend

### 4. Database Schema (`shared/schema.ts`)

**PostgreSQL Tables**:

```sql
-- Conversations table
CREATE TABLE conversations (
    id VARCHAR PRIMARY KEY,
    title VARCHAR NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Messages table  
CREATE TABLE messages (
    id VARCHAR PRIMARY KEY,
    conversation_id VARCHAR REFERENCES conversations(id),
    role VARCHAR NOT NULL, -- 'user' or 'assistant'
    content TEXT NOT NULL,
    displays JSONB, -- For data visualizations
    created_at TIMESTAMP DEFAULT NOW()
);

-- Users table (for authentication)
CREATE TABLE users (
    id VARCHAR PRIMARY KEY,
    username VARCHAR UNIQUE NOT NULL,
    password_hash VARCHAR NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);
```

**Data Persistence Flow**:
- Every chat interaction is stored with full context
- Conversation titles automatically update from first user message
- Message displays (tables, charts) stored as JSONB
- Support for 160+ concurrent conversations with full history

### 5. External Data Integration

#### Keboola Cloud Integration
```python
class KeboolaStorageClient:
    def __init__(self, api_url: str, token: str):
        self.api_url = api_url
        self.token = token
        self.session = requests.Session()
        self.session.headers.update({
            'X-StorageApi-Token': token,
            'Content-Type': 'application/json'
        })
    
    def list_buckets(self):
        response = self.session.get(f"{self.api_url}/v2/storage/buckets")
        return response.json()
```

#### BigQuery Integration
```python
def internal_execute_sql_query(query: str) -> Dict[str, Any]:
    try:
        # Execute query against BigQuery workspace
        query_job = bigquery_client.query(query)
        results = query_job.result()
        
        # Convert to list of dictionaries
        data = [dict(row) for row in results]
        
        return {
            "status": "success",
            "data": data,
            "query": query,
            "row_count": len(data)
        }
    except Exception as e:
        return {
            "status": "error", 
            "error": str(e),
            "query": query
        }
```

### 6. AI Processing Engine

**Gemini 2.0 Flash Configuration**:
```python
SYSTEM_INSTRUCTION_PROMPT = f"""
You are an intelligent assistant for Kultivate AI, specializing in data analysis and business insights for project {GOOGLE_PROJECT_ID} workspace {KBC_WORKSPACE_ID}.

CORE CAPABILITIES:
1. Execute SQL queries against BigQuery tables using internal_execute_sql_query tool
2. Analyze and visualize data from 64+ business tables
3. Provide actionable insights from Keboola Cloud data buckets
4. Support natural language data requests

AVAILABLE TOOLS:
- internal_execute_sql_query: Query BigQuery workspace tables directly
- get_zip_codes_for_city: Location-based data analysis
- get_current_time: Temporal context for data analysis

RESPONSE FORMAT:
Always provide clear, actionable insights. When data is retrieved, format it for optimal readability.
"""

# Create chat session with history and tools
chat_session = gemini_sdk_client.chats.create(
    model='gemini-2.0-flash',
    config=gemini_generation_config_with_tools,
    history=full_history
)
```

**Natural Language Processing Flow**:
1. User asks: "show me undiscovered attendees data"
2. AI interprets request and identifies relevant table
3. Constructs SQL query: `SELECT * FROM Undiscovered---Attendees-Export---Squarespace---All-data-orders--2- LIMIT 10`
4. Executes query through tool system
5. Processes results and formats for display
6. Returns structured response with data visualization

## Data Flow Architecture

### Request Processing Pipeline

```
1. User Input Processing
   ├── Frontend validates and formats message
   ├── TanStack Query handles API state
   └── Sends POST /api/chat with conversationId

2. Proxy Layer Processing  
   ├── Validates backend readiness
   ├── Routes to Python backend on port 8081
   └── Handles error recovery and logging

3. Backend AI Processing
   ├── Loads conversation history from PostgreSQL
   ├── Creates Gemini chat session with tools
   ├── Processes natural language request
   └── Executes appropriate tool functions

4. Data Retrieval Layer
   ├── SQL queries against BigQuery workspace
   ├── Keboola bucket operations (if needed)
   └── External API calls for enrichment

5. Response Generation
   ├── AI formats response with insights
   ├── Creates display objects for visualizations
   ├── Saves conversation to database
   └── Returns structured JSON response

6. Frontend Rendering
   ├── Updates conversation state
   ├── Renders messages and displays
   └── Provides interactive data tables
```

### Error Handling and Fallbacks

**Backend Error Recovery**:
```python
# Multi-level fallback system for data extraction
try:
    # Primary: Direct function response extraction
    query_data = extract_from_function_response(chat_session)
except Exception:
    try:
        # Secondary: History parsing fallback  
        query_data = parse_chat_history(chat_session.get_history())
    except Exception:
        # Tertiary: Text analysis emergency fallback
        query_data = analyze_response_text(final_answer)
```

**Frontend Error Handling**:
- Automatic retry for failed API calls
- Graceful degradation for connectivity issues
- Error boundary components for React crashes
- User-friendly error messages with recovery options

## Environment Configuration

### Required Environment Variables

```bash
# Database
DATABASE_URL=postgresql://username:password@host:port/database

# Keboola Cloud
KBC_API_URL=https://connection.us-east4.gcp.keboola.com
KBC_STORAGE_TOKEN=your_keboola_token
KBC_WORKSPACE_SCHEMA=your_workspace_schema

# Google Cloud
GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
GOOGLE_PROJECT_ID=your-gcp-project-id
KBC_WORKSPACE_ID=your_workspace_id

# AI
GEMINI_API_KEY=your_gemini_api_key

# Security
SESSION_SECRET=your_session_secret
```

### Service Account Permissions

**Required Google Cloud IAM Roles**:
- BigQuery Data Viewer
- BigQuery Job User  
- BigQuery User

**Keboola Cloud Permissions**:
- Storage API access
- Bucket read permissions
- Table detail access

## Deployment Architecture

### Development Environment
```bash
npm run dev
├── Vite dev server (frontend)
├── tsx server/index.ts (proxy server)  
└── Python backend with auto-reload
```

### Production Environment
```bash
npm run build && npm start
├── Express static server (port 5000)
├── Gunicorn WSGI server (port 8081)
└── PostgreSQL database connection
```

### Process Management
- Frontend served on external port 80 → internal port 5000
- Backend API internal only on port 8081
- Health checks and automatic restart capabilities
- Concurrent server execution with shared database

## Performance Considerations

### Optimization Strategies

1. **Database Query Optimization**:
   - Indexed conversation and message lookups
   - Efficient pagination for conversation history
   - JSONB indexing for display data

2. **AI Response Caching**:
   - Conversation context preserved across sessions
   - Tool execution results cached when appropriate
   - Fallback mechanisms for faster response times

3. **Frontend Performance**:
   - React Query for efficient state management
   - Lazy loading for conversation history
   - Optimized bundle splitting with Vite

4. **Backend Scaling**:
   - Gunicorn worker processes for concurrent requests
   - Connection pooling for database operations
   - Async processing for non-blocking tool execution

## Security Implementation

### Authentication System
- Passport.js with local strategy
- Scrypt password hashing with salt
- PostgreSQL session storage
- Protected API routes with middleware

### Data Security
- Environment variable configuration for secrets
- Service account authentication for GCP
- Token-based authentication for Keboola
- CORS configuration for frontend/backend communication

## Monitoring and Debugging

### Logging Strategy
```python
# Comprehensive logging throughout backend
app.logger.info(f"Processing message: {user_message_text[:100]}...")
app.logger.info(f"Tool execution result: {len(query_data)} rows")
app.logger.error(f"Error in AI processing: {str(e)}", exc_info=True)
```

### Key Metrics Tracked
- Conversation creation rate (164+ conversations)
- Message processing time and success rate
- Data query execution performance
- AI tool usage patterns
- Error rates and recovery success

## Rebuild Instructions

To recreate this system, implement components in this order:

1. **Database Setup**: Create PostgreSQL with schema from `shared/schema.ts`
2. **Backend Core**: Implement Flask app with Gemini integration
3. **Tool System**: Add BigQuery and Keboola client implementations  
4. **Proxy Server**: Create Node.js proxy with backend process management
5. **Frontend**: Build React chat interface with TanStack Query
6. **Authentication**: Integrate Passport.js authentication system
7. **Deployment**: Configure dual-server architecture with health checks

Each component is modular and can be developed independently, with clear interfaces defined by the API contracts documented above.

## Data Examples

The system successfully handles 64+ business data tables including:
- Event attendee exports from Squarespace
- Vendor data and close-out sales information  
- Market recap and analytics data
- Customer order and transaction records
- Geographic and demographic analysis data

Natural language queries like "show me undiscovered attendees data" automatically resolve to the correct BigQuery table and return formatted results with 10+ rows of relevant business data.