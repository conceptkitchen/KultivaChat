# Kultivate AI Enhanced API Documentation

## Overview

This API provides intelligent access to BigQuery Keboola Workspace data with **full natural language processing capabilities**. All authentication is handled server-side, and the AI can understand complex business questions and automatically execute appropriate queries.

**Base URL:** `https://kultivate-chat-ck.replit.app`

---

## Natural Language Intelligence

### ✨ **What's New: Full AI Understanding**

The API now has the same natural language capabilities as the main chat interface:

- **Complex Business Questions**: "Show me revenue trends from Balay Kreative events"
- **Casual Table References**: "Get attendee data from that Filipino event"
- **Analytical Requests**: "Analyze sales performance across all vendors"
- **Contextual Queries**: "How many people attended the March events?"

---

## API Endpoints

### 1. Health Check
**Endpoint:** `GET /api/health`

**Purpose:** Verify API service status

**Request:**
```bash
curl https://kultivate-chat-ck.replit.app/api/health
```

**Response:**
```json
{
  "backend": "running",
  "status": "healthy"
}
```

---

### 2. **Intelligent Query Router** (Recommended)
**Endpoint:** `POST /api/v1/data/query`

**Purpose:** Single endpoint that understands any type of query - natural language, SQL, or table discovery

**Natural Language Examples:**
```bash
# Business question
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me revenue from Balay Kreative events"}'

# Casual request
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many people attended Kapwa Gardens events?"}'

# Analysis request
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Analyze vendor performance in the last quarter"}'
```

**Table Discovery:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "show me all tables"}'
```

**Direct SQL:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 10"}'
```

**Enhanced Response Format:**
```json
{
  "success": true,
  "query": "Show me revenue from Balay Kreative events",
  "response": "I found revenue data from Balay Kreative events. Here's the analysis of 166 attendees across multiple events with total revenue of $3,954.",
  "data": [
    {
      "type": "table",
      "title": "Balay Kreative Revenue Analysis",
      "content": [
        {
          "Event_Name": "Barya Kitchen & An Choi: Filipino x Vietnamese Kamayan - 3/20/2020",
          "Event_Date": "3/20/2020",
          "Lineitem_price": "75",
          "Total_Revenue": "150"
        }
      ]
    }
  ],
  "route_used": "nlp",
  "rows_returned": 10,
  "timestamp": "2025-06-26T00:50:12.345Z"
}
```

---

### 3. Table Discovery
**Endpoint:** `POST /api/v1/data/tables`

**Purpose:** Get list of all available data tables

**Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/tables \
  -H "Content-Type: application/json" \
  -d '{}'
```

**Response:**
```json
{
  "success": true,
  "data": [
    {"table_name": "Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders"},
    {"table_name": "Vendor-Close-Out---Dye-Hard--2023-04-02---Kapwa-Gardens-New-close-out-Dye-Hard"}
  ],
  "timestamp": "2025-06-26T00:50:12.345Z"
}
```

---

### 4. Direct SQL Execution
**Endpoint:** `POST /api/v1/data/sql`

**Purpose:** Execute SQL queries directly against BigQuery

**Request:**
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/sql \
  -H "Content-Type: application/json" \
  -d '{
    "sql": "SELECT Event_Name, SUM(CAST(Lineitem_price AS FLOAT64)) as revenue FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` GROUP BY Event_Name ORDER BY revenue DESC LIMIT 5"
  }'
```

**Response:**
```json
{
  "success": true,
  "data": [
    {
      "Event_Name": "Barya Kitchen & An Choi: Filipino x Vietnamese Kamayan - 3/20/2020",
      "revenue": 450.0
    }
  ],
  "rows_returned": 5,
  "error": null,
  "timestamp": "2025-06-26T00:50:12.345Z"
}
```

---

## Natural Language Capabilities

### 🧠 **AI Understanding Examples**

**Business Analysis:**
- "What's the total revenue from all events?"
- "Show me the most popular events by attendance"
- "Compare vendor performance between Q1 and Q2"
- "Which events had the highest ticket prices?"

**Data Exploration:**
- "Show me recent vendor data"
- "Get customer information from Filipino events"
- "Find all events in March 2020"
- "List attendees who spent more than $100"

**Trend Analysis:**
- "Analyze attendance trends over time"
- "Show revenue growth by month"
- "Compare event performance year over year"
- "Identify peak attendance periods"

### 🎯 **Smart Data Recognition**

The AI automatically recognizes:
- **Event Names**: "Balay Kreative", "Kapwa Gardens", "Filipino events"
- **Vendor References**: "Dye Hard", "Yum Yams", vendor close-outs
- **Time Periods**: "March 2020", "last quarter", "recent events"
- **Business Metrics**: revenue, attendance, ticket prices, orders

---

## Enhanced Integration Examples

### JavaScript/Node.js
```javascript
async function askBusinessQuestion(question) {
  const response = await fetch('https://kultivate-chat-ck.replit.app/api/v1/data/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: question })
  });
  
  const result = await response.json();
  
  if (result.success) {
    console.log('AI Response:', result.response);
    
    // Process data tables
    result.data.forEach(display => {
      if (display.type === 'table') {
        console.log(`\n${display.title}:`);
        console.table(display.content);
      }
    });
    
    return result;
  } else {
    console.error('Query failed:', result.error);
  }
}

// Usage examples
await askBusinessQuestion("What's the total revenue from Balay Kreative events?");
await askBusinessQuestion("Show me the top 5 highest-attended events");
await askBusinessQuestion("Analyze vendor performance trends");
```

### Python
```python
import requests

def ask_business_question(question):
    response = requests.post(
        'https://kultivate-chat-ck.replit.app/api/v1/data/query',
        json={'query': question}
    )
    
    result = response.json()
    
    if result['success']:
        print(f"AI Response: {result['response']}")
        
        # Process data tables
        for display in result.get('data', []):
            if display['type'] == 'table':
                print(f"\n{display['title']}:")
                for row in display['content']:
                    print(row)
        
        return result
    else:
        print(f"Query failed: {result['error']}")
        return None

# Usage examples
ask_business_question("What's the average ticket price across all events?")
ask_business_question("Show me customer demographics from event data")
ask_business_question("Find events with highest profit margins")
```

### PHP
```php
function askBusinessQuestion($question) {
    $data = json_encode(['query' => $question]);
    
    $context = stream_context_create([
        'http' => [
            'method' => 'POST',
            'header' => "Content-Type: application/json\r\n",
            'content' => $data
        ]
    ]);
    
    $response = file_get_contents(
        'https://kultivate-chat-ck.replit.app/api/v1/data/query',
        false,
        $context
    );
    
    $result = json_decode($response, true);
    
    if ($result['success']) {
        echo "AI Response: " . $result['response'] . "\n";
        
        foreach ($result['data'] as $display) {
            if ($display['type'] === 'table') {
                echo "\n" . $display['title'] . ":\n";
                foreach ($display['content'] as $row) {
                    print_r($row);
                }
            }
        }
        
        return $result;
    } else {
        echo "Query failed: " . $result['error'] . "\n";
        return null;
    }
}

// Usage examples
askBusinessQuestion("Calculate total revenue by event type");
askBusinessQuestion("Show me repeat customers from event data");
```

---

## Advanced Query Examples

### Revenue Analysis
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Calculate total revenue and profit margins for each event, showing ROI analysis"}'
```

### Customer Insights
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Analyze customer behavior patterns - who are the repeat attendees and what events do they prefer?"}'
```

### Market Analysis
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Compare market performance between Filipino and Vietnamese events, including attendance and revenue metrics"}'
```

### Vendor Performance
```bash
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Evaluate vendor close-out sales performance - which vendors had the best conversion rates?"}'
```

---

## Data Sources Available

### Business Data Tables (64 total)
- **Event Attendance**: Complete attendee records with registration details
- **Revenue Data**: Ticket sales, pricing, and financial performance
- **Vendor Information**: Close-out sales, vendor performance metrics
- **Customer Data**: Attendee demographics and purchase history
- **Market Analytics**: Event performance across different segments

### Supported Business Areas
- **Balay Kreative Events**: Filipino cultural events and dining experiences
- **Kapwa Gardens**: Community events and vendor markets
- **Multi-cultural Events**: Vietnamese, Filipino, and fusion experiences
- **Vendor Markets**: Close-out sales and vendor performance tracking

---

## Response Format Standards

All API responses follow this enhanced structure:

```json
{
  "success": true|false,
  "query": "Original user query",
  "response": "AI-generated natural language response",
  "data": [
    {
      "type": "table",
      "title": "Descriptive title for the data",
      "content": [
        {"column1": "value1", "column2": "value2"},
        {"column1": "value3", "column2": "value4"}
      ]
    }
  ],
  "route_used": "nlp|tables|sql",
  "rows_returned": 0,
  "timestamp": "2025-06-26T00:50:12.345Z",
  "error": null|"error message"
}
```

---

## Best Practices

### 1. Use Natural Language First
Start with business questions in plain English:
```bash
# Instead of complex SQL
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much revenue did we generate from Filipino events?"}'
```

### 2. Be Specific About Analysis
```bash
# Good: Specific business question
{"query": "Show me the top 5 events by attendance with revenue breakdown"}

# Better: Include context for deeper analysis
{"query": "Analyze the top 5 events by attendance, including revenue per attendee and profit margins"}
```

### 3. Leverage AI Context Understanding
```bash
# The AI understands business context
{"query": "Compare our Q1 performance to Q2 across all metrics"}
{"query": "Which vendor partnerships were most profitable?"}
{"query": "Show me customer retention rates for repeat attendees"}
```

### 4. Error Handling
```javascript
const result = await fetch('/api/v1/data/query', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ query: userQuestion })
});

const data = await result.json();

if (data.success) {
  // Process AI response and data
  console.log(data.response);
  data.data.forEach(display => {
    // Handle table displays
  });
} else {
  // Handle errors gracefully
  console.error(`Query failed: ${data.error}`);
  
  // Suggest alternatives based on route_used
  if (data.route_used === 'nlp') {
    console.log('Try rephrasing your question or use more specific terms');
  }
}
```

---

## Migration from Previous Version

### Old Way (Limited)
```bash
# Had to choose specific endpoints
curl -X POST .../api/v1/data/tables  # For table discovery
curl -X POST .../api/v1/data/sql     # For SQL queries
# Natural language returned 503 error
```

### New Way (Intelligent)
```bash
# One endpoint handles everything intelligently
curl -X POST https://kultivate-chat-ck.replit.app/api/v1/data/query \
  -H "Content-Type: application/json" \
  -d '{"query": "ANY TYPE OF QUESTION OR REQUEST"}'
```

---

## Security & Performance

### Server-Side Intelligence
- **AI Processing**: Gemini 2.0 Flash with business context understanding
- **Credential Management**: All BigQuery and Keboola credentials handled server-side
- **Query Optimization**: Automatic query optimization and result caching
- **Rate Limiting**: Intelligent throttling for production usage

### Data Privacy
- **No Client Credentials**: External applications never handle sensitive credentials
- **Secure Processing**: All AI processing happens in secure server environment
- **Audit Logging**: Complete query and response logging for security

---

**Last Updated:** June 26, 2025  
**API Version:** Enhanced Natural Language v1.0  
**AI Model:** Gemini 2.0 Flash with Business Intelligence Tools