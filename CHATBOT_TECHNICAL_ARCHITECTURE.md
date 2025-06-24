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

**Overview**: Keboola Cloud serves as the primary data platform that orchestrates ETL processes, data storage, and workspace management. The chatbot integrates with Keboola's Storage API to access data buckets, table metadata, and workspace configurations.

**Authentication & Configuration**:
```python
class KeboolaStorageClient:
    def __init__(self, api_url: str, token: str):
        self.api_url = api_url  # https://connection.us-east4.gcp.keboola.com
        self.token = token      # Storage API token from Keboola project
        self.session = requests.Session()
        self.session.headers.update({
            'X-StorageApi-Token': token,
            'Content-Type': 'application/json',
            'User-Agent': 'Kultivate-AI-Chatbot/1.0'
        })
```

**Core API Operations**:

1. **Storage Bucket Management**:
```python
def list_buckets(self) -> Dict[str, Any]:
    """Retrieve all storage buckets in the workspace"""
    try:
        response = self.session.get(f"{self.api_url}/v2/storage/buckets")
        response.raise_for_status()
        buckets = response.json()
        
        return {
            "status": "success",
            "buckets": buckets,
            "count": len(buckets)
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}

def get_bucket_detail(self, bucket_id: str) -> Dict[str, Any]:
    """Get detailed information about a specific bucket"""
    try:
        response = self.session.get(f"{self.api_url}/v2/storage/buckets/{bucket_id}")
        response.raise_for_status()
        bucket_detail = response.json()
        
        return {
            "status": "success",
            "bucket": bucket_detail,
            "table_count": len(bucket_detail.get('tables', [])),
            "size_bytes": bucket_detail.get('sizeBytes', 0)
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}
```

2. **Table Operations**:
```python
def list_tables_in_bucket(self, bucket_id: str) -> Dict[str, Any]:
    """List all tables within a specific bucket"""
    try:
        response = self.session.get(f"{self.api_url}/v2/storage/buckets/{bucket_id}/tables")
        response.raise_for_status()
        tables = response.json()
        
        return {
            "status": "success",
            "tables": tables,
            "count": len(tables),
            "bucket_id": bucket_id
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}

def get_table_detail(self, table_id: str) -> Dict[str, Any]:
    """Get comprehensive table metadata including schema and statistics"""
    try:
        response = self.session.get(f"{self.api_url}/v2/storage/tables/{table_id}")
        response.raise_for_status()
        table_detail = response.json()
        
        return {
            "status": "success",
            "table": table_detail,
            "columns": table_detail.get('columns', []),
            "row_count": table_detail.get('rowsCount', 0),
            "size_bytes": table_detail.get('dataSizeBytes', 0),
            "created": table_detail.get('created'),
            "last_import": table_detail.get('lastImportDate')
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}
```

3. **Data Export Operations**:
```python
def export_table_data(self, table_id: str, limit: int = 1000) -> Dict[str, Any]:
    """Export table data with optional row limit"""
    try:
        params = {
            'limit': limit,
            'format': 'json'
        }
        response = self.session.get(
            f"{self.api_url}/v2/storage/tables/{table_id}/data-preview",
            params=params
        )
        response.raise_for_status()
        
        return {
            "status": "success",
            "data": response.json(),
            "table_id": table_id,
            "row_count": len(response.json())
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}
```

**Workspace Integration**:
```python
def get_workspace_info(self) -> Dict[str, Any]:
    """Retrieve workspace configuration and metadata"""
    try:
        # Get workspace details from token info
        response = self.session.get(f"{self.api_url}/v2/storage/tokens/verify")
        response.raise_for_status()
        token_info = response.json()
        
        workspace_info = {
            "project_id": token_info.get('owner', {}).get('id'),
            "project_name": token_info.get('owner', {}).get('name'),
            "token_description": token_info.get('description'),
            "permissions": token_info.get('bucketPermissions', []),
            "expires": token_info.get('expires'),
            "created": token_info.get('created')
        }
        
        return {
            "status": "success",
            "workspace": workspace_info
        }
    except requests.exceptions.RequestException as e:
        return {"status": "error", "error": str(e)}
```

**Integration with BigQuery Workspace**:

The Keboola Cloud platform automatically provisions BigQuery datasets that mirror the bucket structure:

```python
# Environment Configuration
KBC_WORKSPACE_SCHEMA = "WORKSPACE_21894820"  # BigQuery dataset
GOOGLE_PROJECT_ID = "kbc-use4-839-261b"      # GCP project managed by Keboola

def get_bigquery_table_mapping(self) -> Dict[str, str]:
    """Map Keboola table IDs to BigQuery table names"""
    buckets = self.list_buckets()
    
    table_mapping = {}
    if buckets['status'] == 'success':
        for bucket in buckets['buckets']:
            bucket_tables = self.list_tables_in_bucket(bucket['id'])
            if bucket_tables['status'] == 'success':
                for table in bucket_tables['tables']:
                    keboola_table_id = table['id']
                    bigquery_table_name = table['name']  # Transformed table name
                    
                    table_mapping[keboola_table_id] = {
                        "bigquery_name": bigquery_table_name,
                        "bucket": bucket['id'],
                        "stage": bucket.get('stage', 'out'),
                        "full_bigquery_path": f"`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.{bigquery_table_name}`"
                    }
    
    return table_mapping
```

**Error Handling and Rate Limiting**:
```python
import time
from functools import wraps

def retry_on_rate_limit(max_retries=3, backoff_factor=2):
    def decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            for attempt in range(max_retries):
                try:
                    result = func(self, *args, **kwargs)
                    if isinstance(result, dict) and result.get('status') == 'error':
                        if 'rate limit' in result.get('error', '').lower() and attempt < max_retries - 1:
                            sleep_time = backoff_factor ** attempt
                            time.sleep(sleep_time)
                            continue
                    return result
                except requests.exceptions.RequestException as e:
                    if attempt == max_retries - 1:
                        return {"status": "error", "error": f"Max retries exceeded: {str(e)}"}
                    time.sleep(backoff_factor ** attempt)
            
            return {"status": "error", "error": "Unexpected error in retry logic"}
        return wrapper
    return decorator

# Apply to API methods
list_buckets = retry_on_rate_limit()(list_buckets)
get_table_detail = retry_on_rate_limit()(get_table_detail)
```

**Chatbot Integration with Keboola Tools**:

The chatbot system integrates Keboola operations through AI tools, though the current implementation primarily uses direct BigQuery access for performance:

```python
# Legacy tool functions (now simplified in current implementation)
def list_keboola_buckets() -> Dict[str, Any]:
    """AI tool function to list all Keboola buckets"""
    try:
        result = keboola_client.list_buckets()
        if result['status'] == 'success':
            bucket_summary = []
            for bucket in result['buckets']:
                bucket_summary.append({
                    "id": bucket['id'],
                    "stage": bucket.get('stage', 'unknown'),
                    "name": bucket.get('displayName', bucket['id']),
                    "table_count": len(bucket.get('tables', [])),
                    "description": bucket.get('description', 'No description')
                })
            
            return {
                "status": "success",
                "message": f"Found {len(bucket_summary)} buckets in workspace",
                "buckets": bucket_summary
            }
        else:
            return result
    except Exception as e:
        return {"status": "error", "error": str(e)}

def get_keboola_table_detail(table_id: str) -> Dict[str, Any]:
    """AI tool function to get detailed table information"""
    try:
        result = keboola_client.get_table_detail(table_id)
        if result['status'] == 'success':
            table = result['table']
            return {
                "status": "success",
                "message": f"Table {table_id} has {result['row_count']} rows and {len(result['columns'])} columns",
                "table_info": {
                    "id": table['id'],
                    "name": table.get('displayName', table['name']),
                    "rows": result['row_count'],
                    "columns": [col['name'] for col in result['columns']],
                    "size_mb": round(result['size_bytes'] / (1024*1024), 2),
                    "last_update": result['last_import']
                }
            }
        else:
            return result
    except Exception as e:
        return {"status": "error", "error": str(e)}
```

**Current Implementation Strategy**:

The chatbot system has evolved to use direct BigQuery access for optimal performance, while maintaining Keboola integration for metadata and workspace management:

1. **Data Queries**: Direct BigQuery SQL execution via `internal_execute_sql_query`
2. **Table Discovery**: BigQuery INFORMATION_SCHEMA queries for table listings
3. **Metadata**: Keboola API for business context and data lineage
4. **Workspace Management**: Keboola API for bucket organization and permissions

**Table Name Resolution**:

The system handles the complex table naming conventions between Keboola and BigQuery:

```python
# Example table name transformations
KEBOOLA_TABLE_ID = "out.c-main.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"
BIGQUERY_TABLE_NAME = "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"

# Natural language resolution
def resolve_table_from_query(user_query: str) -> str:
    """Resolve user's natural language to actual BigQuery table name"""
    query_lower = user_query.lower()
    
    # Pattern matching for common requests
    if "undiscovered" in query_lower and "attendees" in query_lower:
        return "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"
    elif "balay" in query_lower or "kreative" in query_lower:
        return "Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders"
    elif "vendor" in query_lower and "export" in query_lower:
        return "Undiscovered-Vendor-Export---Squarespace---All-data-orders"
    
    # Fallback to table list query
    return None
```

**Data Pipeline Integration**:

Keboola Cloud orchestrates the complete data pipeline that feeds the chatbot:

1. **Data Ingestion**: Keboola extractors pull data from Squarespace, Shopify, and other sources
2. **Data Transformation**: Keboola transformations clean and structure the data
3. **BigQuery Sync**: Processed data automatically syncs to BigQuery workspace
4. **Chatbot Access**: AI queries the BigQuery mirror for real-time analysis
5. **Metadata Enrichment**: Keboola API provides business context and data lineage

This architecture allows the chatbot to provide intelligent data analysis while maintaining the full data governance and processing capabilities of the Keboola platform.

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

### Required Environment Variables and Private Keys

**CRITICAL SECURITY NOTE**: All private keys and tokens must be stored securely in environment variables or Replit Secrets, never in code files.

```bash
# Database Connection
DATABASE_URL=postgresql://username:password@host:port/database
# Example: postgresql://postgres:secretpassword@localhost:5432/kultivate_ai

# Keboola Cloud Configuration
KBC_API_URL=https://connection.us-east4.gcp.keboola.com
KBC_STORAGE_TOKEN=1234567890abcdef1234567890abcdef12345678
KBC_WORKSPACE_SCHEMA=WORKSPACE_21894820
KBC_WORKSPACE_ID=WORKSPACE_21894820

# Google Cloud Platform
GOOGLE_APPLICATION_CREDENTIALS=/home/runner/workspace/backend/credentials-839-21894808.json
GOOGLE_PROJECT_ID=kbc-use4-839-261b

# Google AI/Gemini
GEMINI_API_KEY=AIzaSyABC123def456GHI789jkl012MNO345pqr
# Get from: https://aistudio.google.com/app/apikey

# Express Session Security
SESSION_SECRET=your-super-secure-random-session-secret-minimum-32-characters
# Generate with: openssl rand -base64 32

# Optional: Backend Configuration
BACKEND_URL=http://localhost:8081
NODE_ENV=development|production
```

### Private Key and Credential Requirements

**1. Keboola Cloud Storage API Token**
- **Location**: KBC_STORAGE_TOKEN environment variable
- **Format**: 40-character hexadecimal string
- **Permissions Required**:
  - `storage:read` for all buckets
  - `canReadAllFileUploads` for file access
- **How to Generate**:
  1. Log into Keboola Cloud project
  2. Navigate to Settings → API Tokens
  3. Create new token with description "Kultivate AI Integration"
  4. Copy the generated token immediately (only shown once)

**2. Google Cloud Service Account JSON**
- **Location**: File path specified in GOOGLE_APPLICATION_CREDENTIALS
- **File**: `backend/credentials-839-21894808.json`
- **Required IAM Roles**:
  - BigQuery Data Viewer
  - BigQuery Job User
  - BigQuery User
- **How to Generate**:
  1. Go to Google Cloud Console → IAM & Admin → Service Accounts
  2. Create new service account or use existing
  3. Add required BigQuery roles
  4. Generate JSON key and download
  5. Place in backend directory with secure file permissions

**3. Google Gemini API Key**
- **Location**: GEMINI_API_KEY environment variable
- **Format**: AIzaSy... (starts with AIzaSy)
- **How to Generate**:
  1. Visit https://aistudio.google.com/app/apikey
  2. Create new API key or use existing
  3. Ensure Gemini API is enabled in your project
  4. Copy the generated key

**4. PostgreSQL Database Credentials**
- **Location**: DATABASE_URL environment variable
- **Format**: postgresql://username:password@host:port/database
- **Requirements**:
  - Full read/write access to database
  - Ability to create tables and indexes
  - Connection pooling support

**5. Express Session Secret**
- **Location**: SESSION_SECRET environment variable
- **Requirements**: 
  - Minimum 32 characters
  - Cryptographically random
  - Unique per deployment
- **Generate with**: `openssl rand -base64 32`

### Keboola Cloud API Authentication

**Storage API Token Setup**:
1. Navigate to Keboola Cloud project settings
2. Generate new Storage API token with required permissions:
   - `storage:read` - Read access to buckets and tables
   - `storage:write` - Write access for data exports (if needed)
   - `canReadAllFileUploads` - Access to file storage
   - `canPurgeTrash` - Maintenance operations

**Token Configuration Example**:
```json
{
  "id": "12345",
  "description": "Kultivate AI Chatbot Integration",
  "bucketPermissions": {
    "out": "read",
    "in": "read", 
    "sys": "read"
  },
  "expires": null,
  "canReadAllFileUploads": true,
  "canPurgeTrash": false
}
```

**Workspace Schema Mapping**:
The `KBC_WORKSPACE_SCHEMA` corresponds to the BigQuery dataset where Keboola automatically syncs all workspace data:
- Format: `WORKSPACE_{PROJECT_ID}`
- Example: `WORKSPACE_21894820` for Keboola project ID 21894820
- BigQuery path: `kbc-use4-839-261b.WORKSPACE_21894820.{table_name}`

### Service Account Permissions

**Required Google Cloud IAM Roles**:
- BigQuery Data Viewer (for querying workspace tables)
- BigQuery Job User (for executing SQL queries)
- BigQuery User (for basic BigQuery access)
- Service Account Token Creator (if using service account impersonation)

**Keboola Cloud API Permissions**:
- Storage API Token with `storage:read` permission
- Bucket-level read access for all relevant data buckets:
  - `out.c-main` - Primary output tables with business data
  - `in.c-shopify` - E-commerce platform input data  
  - `in.c-squarespace` - Website and event data
  - `sys.c-logs` - System logs and metadata (optional)
- File storage access for data exports and backups
- Workspace-level read permissions for metadata operations

**Service Account JSON Structure**:
```json
{
  "type": "service_account",
  "project_id": "kbc-use4-839-261b",
  "private_key_id": "a1b2c3d4e5f6789012345678901234567890abcd",
  "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC...[PRIVATE_KEY_DATA]...==\n-----END PRIVATE KEY-----\n",
  "client_email": "kultivate-ai-service@kbc-use4-839-261b.iam.gserviceaccount.com",
  "client_id": "123456789012345678901",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "universe_domain": "googleapis.com"
}
```

**Critical Private Key Security Requirements**:

1. **File Permissions**: Service account JSON must have restricted permissions
   ```bash
   chmod 600 backend/credentials-839-21894808.json
   chown app:app backend/credentials-839-21894808.json
   ```

2. **Never Commit to Git**: Add to .gitignore
   ```gitignore
   backend/credentials-*.json
   backend/.env
   .env
   ```

3. **Secure Storage in Production**:
   - Use Replit Secrets for environment variables
   - Upload service account JSON via secure file upload
   - Enable automatic secret rotation if available

4. **Access Logging**: Monitor service account usage in Google Cloud Console
   - Check IAM audit logs for unauthorized access
   - Set up alerts for unusual BigQuery usage patterns

**Default Authentication Configuration**:
- **Admin Username**: admin
- **Admin Password**: admin123 (CHANGE IN PRODUCTION)
- **Password Hash**: Generated using scrypt with salt
- **Session Duration**: 24 hours (configurable)

**Backup and Recovery Keys**:
- Database backup encryption key (if using encrypted backups)
- SSL/TLS certificates for HTTPS (in production)
- Additional service account keys for failover scenarios

**Keboola-BigQuery Data Sync Configuration**:
The workspace automatically maintains bidirectional sync between Keboola storage and BigQuery:
- **Real-time sync**: Data changes in Keboola appear in BigQuery within minutes
- **Schema preservation**: Column types and constraints maintained across platforms
- **Table naming**: Keboola table IDs transform to BigQuery-compatible names
- **Access control**: Service account permissions apply to both platforms

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

### Phase 1: Credential Setup and Security
1. **Generate Service Account JSON**:
   - Create Google Cloud service account with BigQuery permissions
   - Download JSON credentials file
   - Place in `backend/credentials-839-21894808.json`
   - Set secure file permissions (600)

2. **Obtain Keboola Storage Token**:
   - Access Keboola Cloud project settings
   - Generate Storage API token with read permissions
   - Document token ID and expiration date

3. **Generate Gemini API Key**:
   - Visit Google AI Studio
   - Create new API key with Gemini access
   - Test key with simple API call

4. **Setup Environment Variables**:
   ```bash
   export DATABASE_URL="postgresql://..."
   export KBC_STORAGE_TOKEN="..."
   export GEMINI_API_KEY="..."
   export SESSION_SECRET="$(openssl rand -base64 32)"
   export GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account.json"
   ```

### Phase 2: Database and Backend
1. **Database Setup**: Create PostgreSQL with schema from `shared/schema.ts`
2. **Backend Core**: Implement Flask app with Gemini integration
3. **Tool System**: Add BigQuery and Keboola client implementations  
4. **Test Connectivity**: Verify all API connections work

### Phase 3: Application Layer
1. **Proxy Server**: Create Node.js proxy with backend process management
2. **Frontend**: Build React chat interface with TanStack Query
3. **Authentication**: Integrate Passport.js authentication system
4. **End-to-End Testing**: Verify complete chat flow

### Phase 4: Production Deployment
1. **Security Hardening**: Change default passwords, enable HTTPS
2. **Secret Management**: Move all credentials to secure environment
3. **Monitoring**: Setup logging and error tracking
4. **Deployment**: Configure dual-server architecture with health checks

### Security Checklist Before Deployment
- [ ] All default passwords changed
- [ ] Service account JSON has minimal required permissions
- [ ] Keboola token has read-only access where possible
- [ ] SESSION_SECRET is cryptographically random
- [ ] Database connection uses SSL
- [ ] HTTPS enabled for frontend
- [ ] Credentials never committed to version control
- [ ] Regular security updates scheduled

Each component is modular and can be developed independently, with clear interfaces defined by the API contracts documented above.

## Data Examples

The system successfully handles 64+ business data tables including:
- Event attendee exports from Squarespace
- Vendor data and close-out sales information  
- Market recap and analytics data
- Customer order and transaction records
- Geographic and demographic analysis data

Natural language queries like "show me undiscovered attendees data" automatically resolve to the correct BigQuery table and return formatted results with 10+ rows of relevant business data.