# Kultivate AI API Service - Complete Documentation

## Overview

Kultivate AI is a production-ready API service that transforms natural language queries into intelligent data insights using Google Gemini AI, Keboola Cloud, and BigQuery integration. The service provides three core endpoints for external applications to consume sophisticated data analysis capabilities.

**Base URL:** `https://Kultivate-chat-ck.replit.app`

## Authentication & Credentials

All API endpoints support credential injection, allowing external applications to provide their own authentication tokens:

```json
{
  "credentials": {
    "KBC_STORAGE_TOKEN": "your-keboola-token",
    "GEMINI_API_KEY": "your-gemini-key", 
    "GOOGLE_APPLICATION_CREDENTIALS": "path-to-service-account.json",
    "DATABASE_URL": "your-postgres-url"
  }
}
```

## API Endpoints

### 1. Natural Language Data Query

**Endpoint:** `POST /api/v1/data/query`

Transform natural language questions into intelligent data responses with actual table displays.

#### Request Format
```json
{
  "query": "Show me Balay Kreative customer analytics",
  "credentials": {
    "KBC_STORAGE_TOKEN": "your-token",
    "GEMINI_API_KEY": "your-key"
  }
}
```

#### Response Format
```json
{
  "success": true,
  "query": "Show me Balay Kreative customer analytics", 
  "response": "I found your Balay Kreative customer data! Here are the analytics:",
  "data": [
    {
      "type": "table",
      "title": "Balay Kreative Customer Analytics",
      "content": [
        {
          "customer_id": "123",
          "customer_name": "John Smith",
          "revenue": 1250,
          "signup_date": "2024-06-15",
          "campaign_source": "Google Ads"
        },
        {
          "customer_id": "124", 
          "customer_name": "Sarah Johnson",
          "revenue": 890,
          "signup_date": "2024-06-14",
          "campaign_source": "Facebook"
        }
      ]
    }
  ],
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

#### Supported Query Types

**Basic Data Operations:**
- **Data Discovery:** "Show me all available tables", "What data do I have?"
- **Specific Queries:** "Show me Kapwa Gardens sales data", "Get Undiscovered event attendees"
- **Sample Data:** "Show me balay kreative attendee data" (returns 10-50 rows)

**Advanced Analytics & Comprehensive Analysis:**
- **Revenue Analysis:** "Show me revenue trends for all events", "Which events generated the most income?"
- **Attendance Analytics:** "Event attendance patterns over time", "Compare attendee engagement across events"
- **Financial Insights:** "Calculate average ticket prices by event type", "Revenue per attendee analysis"
- **Trend Analysis:** "Seasonal patterns in event attendance", "Growth trends year over year"
- **Statistical Aggregations:** "Total revenue by event category", "Average customer lifetime value"
- **Multi-table Analysis:** "Cross-reference vendor data with sales performance"
- **Comprehensive Reports:** "Give me a complete analysis of balay kreative with trends and financials"

**Enhanced Data Capabilities:**
- **Large Datasets:** Up to 500 rows for detailed analysis (vs standard 10-row samples)
- **Complex Aggregations:** GROUP BY, SUM, COUNT, AVG operations
- **Time-Series Analysis:** Multi-year tracking and historical comparisons
- **Business Intelligence:** ROI calculations, profitability metrics, performance indicators

### 2. Direct SQL Execution

**Endpoint:** `POST /api/v1/data/sql`

Execute SQL queries directly against BigQuery with structured response formatting.

#### Request Format
```json
{
  "sql": "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative-Customer-Data` LIMIT 10",
  "credentials": {
    "GOOGLE_APPLICATION_CREDENTIALS": "path-to-service-account.json"
  }
}
```

#### Response Format
```json
{
  "success": true,
  "data": [
    {
      "customer_id": "123",
      "customer_name": "John Smith", 
      "email": "john@example.com",
      "revenue": 1250
    }
  ],
  "error": null,
  "rows_returned": 10,
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

#### SQL Guidelines

**Basic Requirements:**
- Always use fully qualified table names: `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME`
- Tables often contain special characters (e.g., `Balay-Kreative---Customer-Data`)
- Use LIMIT for large datasets to control response size
- Available schemas: Use `INFORMATION_SCHEMA.TABLES` for table discovery

**Advanced Analytics Support:**
- **Large Dataset Queries:** Use `LIMIT 100` or `LIMIT 500` for comprehensive analysis
- **Financial Aggregations:** `SUM(CAST(Lineitem_price AS FLOAT64))` for revenue calculations
- **Trend Analysis:** `GROUP BY Event_Date ORDER BY Event_Date` for time-based patterns
- **Statistical Operations:** `COUNT(*), AVG(), MAX(), MIN()` for detailed insights
- **Multi-table Joins:** Join attendee data with vendor information for complex analysis

**Example Advanced Queries:**
```sql
-- Revenue analysis by event
SELECT Event_Name, COUNT(*) as attendees, 
       SUM(CAST(Lineitem_price AS FLOAT64)) as revenue 
FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders` 
WHERE Lineitem_price IS NOT NULL AND Lineitem_price != '' 
GROUP BY Event_Name ORDER BY revenue DESC LIMIT 20

-- Time-based attendance trends
SELECT Event_Date, COUNT(*) as total_attendees,
       AVG(CAST(Lineitem_price AS FLOAT64)) as avg_price
FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders`
GROUP BY Event_Date ORDER BY Event_Date

-- Comprehensive data export
SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME` LIMIT 500
```

### 3. Table Discovery

**Endpoint:** `POST /api/v1/data/tables`

Discover all available data tables with intelligent categorization.

#### Request Format
```json
{
  "credentials": {
    "KBC_STORAGE_TOKEN": "your-token"
  }
}
```

#### Response Format
```json
{
  "success": true,
  "tables": [
    {
      "type": "table",
      "title": "Available Data Tables",
      "content": [
        {
          "Business Area": "Balay Kreative",
          "Data Type": "Customer Analytics", 
          "Table Name": "Balay-Kreative---Customer-Data",
          "Description": "Creative agency customer data and analytics"
        },
        {
          "Business Area": "Kapwa Gardens",
          "Data Type": "Sales Data",
          "Table Name": "Kapwa-Gardens---Sales-Export",
          "Description": "Community platform sales and transactions"
        }
      ]
    }
  ],
  "total_tables": 64,
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

## Integration Examples

### JavaScript/Node.js Integration

```javascript
class KultivateAI {
  constructor(baseUrl = 'https://Kultivate-chat-ck.replit.app/api/v1') {
    this.baseUrl = baseUrl;
    this.credentials = {};
  }

  setCredentials(credentials) {
    this.credentials = credentials;
  }

  async query(question) {
    const response = await fetch(`${this.baseUrl}/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query: question,
        credentials: this.credentials
      })
    });
    
    return await response.json();
  }

  async executeSQL(sql) {
    const response = await fetch(`${this.baseUrl}/data/sql`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sql: sql,
        credentials: this.credentials
      })
    });
    
    return await response.json();
  }

  async getTables() {
    const response = await fetch(`${this.baseUrl}/data/tables`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        credentials: this.credentials
      })
    });
    
    return await response.json();
  }
}

// Usage Example
const ai = new KultivateAI();
ai.setCredentials({
  KBC_STORAGE_TOKEN: 'your-token',
  GEMINI_API_KEY: 'your-key'
});

const result = await ai.query('Show me recent Balay Kreative sales data');
console.log('AI Response:', result.response);
console.log('Data Tables:', result.data);
```

### Python Integration

```python
import requests
import json

class KultivateAI:
    def __init__(self, base_url='https://Kultivate-chat-ck.replit.app/api/v1'):
        self.base_url = base_url
        self.credentials = {}
    
    def set_credentials(self, credentials):
        self.credentials = credentials
    
    def query(self, question):
        response = requests.post(
            f'{self.base_url}/data/query',
            json={
                'query': question,
                'credentials': self.credentials
            }
        )
        return response.json()
    
    def execute_sql(self, sql):
        response = requests.post(
            f'{self.base_url}/data/sql',
            json={
                'sql': sql,
                'credentials': self.credentials
            }
        )
        return response.json()
    
    def get_tables(self):
        response = requests.get(
            f'{self.base_url}/data/tables',
            json={'credentials': self.credentials}
        )
        return response.json()

# Usage Example
ai = KultivateAI()
ai.set_credentials({
    'KBC_STORAGE_TOKEN': 'your-token',
    'GEMINI_API_KEY': 'your-key'
})

result = ai.query('Show me Kapwa Gardens customer insights')
print('AI Response:', result['response'])
print('Data:', result['data'])
```

## Data Sources & Schema

### BigQuery Integration
- **Project:** `kbc-use4-839-261b`
- **Dataset:** `WORKSPACE_21894820`
- **Tables:** 64+ business data tables including:
  - Balay Kreative (creative agency data)
  - Kapwa Gardens (community platform data)
  - Kultivate Labs (innovation project data)
  - Undiscovered (media platform data)
  - Survey and document data

### Keboola Cloud Integration
- Storage API for raw data access
- Bucket-based data organization
- Table metadata and schema information
- ETL pipeline integration

### AI Processing
- Google Gemini 2.0 Flash for natural language understanding
- Function calling for SQL execution
- Intelligent table name matching and fuzzy search
- Automatic data extraction and formatting

## Error Handling

### Common Error Responses

#### 400 Bad Request
```json
{
  "success": false,
  "error": "Query parameter required",
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

#### 503 Service Unavailable
```json
{
  "success": false,
  "error": "Backend starting up...",
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

#### 500 Internal Server Error
```json
{
  "success": false,
  "error": "SQL execution failed: Invalid table name",
  "timestamp": "2025-06-25T19:30:00.000Z"
}
```

### Best Practices
- Always check the `success` field in responses
- Handle network timeouts (queries can take 10-30 seconds)
- Implement retry logic for temporary failures
- Validate credentials before making requests
- Use appropriate LIMIT clauses for large datasets

## Rate Limiting & Performance

- **Concurrent Requests:** Up to 10 simultaneous requests
- **Query Timeout:** 120 seconds maximum
- **Response Size:** Limited to 10MB per response
- **Rate Limiting:** 100 requests per minute per IP

## Security Considerations

- Credentials are processed server-side and not stored
- All API communication uses HTTPS
- SQL injection protection through parameterized queries
- Access control through credential validation

## Troubleshooting

### Common Issues

1. **"Backend starting up" errors**
   - Wait 30-60 seconds for backend initialization
   - Retry the request

2. **SQL execution failures**
   - Verify table names use correct format with special characters
   - Check credentials have appropriate BigQuery permissions
   - Use INFORMATION_SCHEMA queries to discover available tables

3. **Empty data responses**
   - Verify the AI found relevant tables for your query
   - Try more specific table names or business area references
   - Check that tables contain actual data records

4. **Credential issues**
   - Ensure all required credentials are provided
   - Verify token permissions and expiration
   - Test credentials directly against source APIs

### Support

For technical support or integration assistance:
- Review the API test script: `test-api.js`
- Check server logs for detailed error information
- Verify credential permissions and access rights

## Changelog

- **June 25, 2025:** Initial API service release
- **Features:** Natural language queries, direct SQL execution, table discovery
- **Integration:** Credential management and external product support