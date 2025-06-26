# Kultivate AI Enhanced API Documentation - Natural Language Processing

## Overview

The Kultivate AI API v1 endpoint now provides **full natural language processing capabilities** identical to the main chat interface, powered by Gemini 2.0 Flash AI with direct BigQuery integration.

**Base URL:** `https://kultivate-chat-ck.replit.app`

---

## Enhanced Natural Language Capabilities

### Business Intelligence Features
- **Automatic table discovery** from 64+ BigQuery business tables
- **Smart entity matching** for Balay Kreative, Kapwa Gardens, vendors, events
- **Revenue analysis** with real financial calculations
- **Attendee data extraction** from event management systems
- **Full SQL generation** for complex business queries

### AI Processing Pipeline
1. **Natural language understanding** via Gemini 2.0 Flash
2. **Intelligent table matching** using fuzzy search algorithms
3. **Automatic SQL generation** for BigQuery WORKSPACE_21894820
4. **Data extraction** with comprehensive fallback mechanisms
5. **Business context analysis** for meaningful insights

---

## API Endpoint: Enhanced Query Processing

### Endpoint
**POST** `/api/v1/data/query`

### Enhanced Request Format
```json
{
  "query": "show me revenue from Balay Kreative events this year"
}
```

### Natural Language Query Examples

#### 1. Business Entity Analysis
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "show me data from Balay Kreative events"}'
```

**Enhanced Response:**
```json
{
  "success": true,
  "query": "show me data from Balay Kreative events",
  "data": [
    {
      "event_name": "Halo Halo Holidays",
      "date": "2023-12-09",
      "venue": "Kapwa Gardens",
      "attendees": 85,
      "revenue": 1250.00
    },
    {
      "event_name": "Spring Market",
      "date": "2024-03-15", 
      "venue": "Balay Kreative",
      "attendees": 120,
      "revenue": 2150.00
    }
  ],
  "table_name": "-Balay-Kreative--Close-Out-Sales---Halo-Halo-Holidays---2023-12-09---Kapwa-Gardens-KG-Costs",
  "rows_returned": 2,
  "ai_analysis": true,
  "timestamp": "2025-06-26T05:04:00.000Z"
}
```

#### 2. Revenue Analysis Queries
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "what revenue did we generate from Kapwa Gardens events?"}'
```

#### 3. Table Discovery
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "show me available tables"}'
```

**Enhanced Response:**
```json
{
  "success": true,
  "data": [
    {"table_name": "-Balay-Kreative--Close-Out-Sales---Halo-Halo-Holidays---2023-12-09---Kapwa-Gardens-KG-Costs"},
    {"table_name": "Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders"},
    {"table_name": "Kapwa-Gardens---Close-Out-Market-RECAP---NEW---April-Events-RECAP"},
    {"table_name": "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"}
  ],
  "total_tables": 64,
  "message": "Available business data tables from BigQuery",
  "timestamp": "2025-06-26T05:04:00.000Z"
}
```

#### 4. Complex Business Questions
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "analyze attendee trends across all events"}'
```

---

## Enhanced Response Structure

### Successful Data Retrieval
```json
{
  "success": true,
  "query": "original user query",
  "response": "AI-generated analysis text",
  "data": [
    {
      "column1": "value1",
      "column2": "value2"
    }
  ],
  "table_name": "source_table_identifier",
  "rows_returned": 10,
  "ai_analysis": true,
  "extraction_method": "primary|fallback_table_reference",
  "timestamp": "2025-06-26T05:04:00.000Z"
}
```

### AI Processing Response
```json
{
  "success": true,
  "query": "user query",
  "response": "Natural language AI response with business insights",
  "data": [],
  "ai_analysis": true,
  "suggestion": "Try asking about specific business entities or use direct SQL",
  "timestamp": "2025-06-26T05:04:00.000Z"
}
```

---

## Business Use Cases

### 1. Event Management
- **Query:** "show me all Balay Kreative events from last quarter"
- **Returns:** Event details, attendance, revenue, dates
- **Use Case:** Performance tracking, planning future events

### 2. Revenue Analysis
- **Query:** "what's our total revenue from Kapwa Gardens?"
- **Returns:** Financial summaries, transaction details, trends
- **Use Case:** Financial reporting, ROI analysis

### 3. Customer Insights
- **Query:** "list all attendees from recent events"
- **Returns:** Customer data, contact information, purchase history
- **Use Case:** Marketing campaigns, customer retention

### 4. Vendor Performance
- **Query:** "show me vendor sales data"
- **Returns:** Vendor revenue, product performance, commission tracking
- **Use Case:** Vendor relationship management, inventory planning

---

## Technical Implementation

### AI Processing Flow
1. **Query Classification:** Determines if query is table discovery, business entity, or general analysis
2. **Entity Recognition:** Identifies business entities (Balay Kreative, Kapwa Gardens, etc.)
3. **Table Matching:** Uses fuzzy search to find relevant data tables
4. **SQL Generation:** Creates optimized BigQuery queries automatically
5. **Data Extraction:** Multiple extraction methods ensure reliable data retrieval
6. **Response Formatting:** Structures data for easy integration

### Performance Optimization
- **Server-side credential management** eliminates client-side authentication
- **Intelligent caching** reduces query response times
- **Fallback mechanisms** ensure reliable data access
- **Timeout protection** prevents hanging requests

### Security Features
- **No client credentials required** - all authentication handled server-side
- **Rate limiting** prevents abuse
- **Input validation** protects against injection attacks
- **Secure BigQuery connections** with service account authentication

---

## Integration Examples

### JavaScript/Node.js
```javascript
async function queryKultivateAI(businessQuestion) {
  const response = await fetch('https://kultivate-chat-ck.replit.app/api/v1/data/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: businessQuestion })
  });
  
  const result = await response.json();
  
  if (result.success && result.data.length > 0) {
    console.log(`Found ${result.rows_returned} records from ${result.table_name}`);
    return result.data;
  } else {
    console.log('AI Response:', result.response);
    return result;
  }
}

// Usage examples
queryKultivateAI("show me Balay Kreative event revenue");
queryKultivateAI("what vendors performed best last month?");
queryKultivateAI("list all customer orders from Kapwa Gardens");
```

### Python
```python
import requests

def analyze_business_data(question):
    response = requests.post(
        'https://kultivate-chat-ck.replit.app/api/v1/data/query',
        json={'query': question},
        timeout=30
    )
    
    if response.status_code == 200:
        result = response.json()
        
        if result['success'] and result.get('data'):
            print(f"Retrieved {len(result['data'])} records")
            return result['data']
        else:
            print(f"AI Analysis: {result.get('response')}")
            return result
    
    return None

# Usage examples
revenue_data = analyze_business_data("show me revenue from all events")
customer_data = analyze_business_data("list top customers by purchase amount")
vendor_performance = analyze_business_data("analyze vendor sales performance")
```

---

## Verification Status

### ✅ Successfully Implemented
- **Natural language processing** with Gemini 2.0 Flash AI
- **Business entity recognition** for Balay Kreative, Kapwa Gardens, vendors
- **Automatic table discovery** from 64+ BigQuery tables
- **Smart data extraction** with multiple fallback mechanisms
- **Real-time business analysis** with authentic data sources

### ✅ Production Ready Features
- **Server-side credential management** eliminates client complexity
- **Intelligent query routing** automatically selects optimal processing method
- **Comprehensive error handling** with helpful suggestions
- **Performance optimization** with caching and timeout protection

### 📊 Test Results
- **Table Discovery:** ✅ Returns 64 BigQuery business tables
- **Business Entity Queries:** ✅ Successfully extracts Balay Kreative event data
- **SQL Generation:** ✅ Automatically creates optimized BigQuery queries
- **AI Processing:** ✅ Full Gemini 2.0 integration with business context

---

## Next Steps for Integration

1. **Test your specific business questions** using the enhanced API endpoint
2. **Review the AI responses** to understand data structure and insights
3. **Implement error handling** for timeout and processing scenarios
4. **Scale your integration** with batch processing for multiple queries
5. **Monitor performance** and adjust query complexity as needed

The Kultivate AI API now provides enterprise-grade business intelligence capabilities through simple natural language queries, making complex data analysis accessible to any application or workflow.