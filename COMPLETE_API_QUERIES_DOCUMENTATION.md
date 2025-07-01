# Kultivate AI Complete API Documentation
**Natural Language Business Intelligence + Dashboard Data**

## Base URL
```
https://kultiva-chatv-2-mcp-conceptkitchen.replit.app
```

---

## 1. NATURAL LANGUAGE QUERIES

### Endpoint: POST /api/query
**Ask any business question in plain English and get intelligent analysis**

**Request Format:**
```bash
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "YOUR BUSINESS QUESTION HERE"}'
```

### Example Business Queries

#### Revenue Analysis
```bash
# Which event made the most money from 2021-2024?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which event made the most money from 2021-2024?"}'

# Show me vendors who made over $500
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors who made over $500"}'

# What was the total revenue for Kapwa Gardens events?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What was the total revenue for Kapwa Gardens events?"}'
```

#### Vendor Performance
```bash
# Who are the top 10 vendors by sales?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are the top 10 vendors by sales?"}'

# Show me vendors from UNDISCOVERED events
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors from UNDISCOVERED events"}'

# Which vendors participated in both Kapwa Gardens and UNDISCOVERED?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in both Kapwa Gardens and UNDISCOVERED?"}'
```

#### Contact Information
```bash
# What are the email addresses of food vendors?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of food vendors?"}'

# Give me phone numbers of UNDISCOVERED vendors
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Give me phone numbers of UNDISCOVERED vendors"}'

# Show me contact info for vendors in San Francisco
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me contact info for vendors in San Francisco"}'
```

#### Geographic Analysis
```bash
# How many attendees were from the Bay Area?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees were from the Bay Area?"}'

# Show me vendors located in Daly City
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors located in Daly City"}'

# What zip codes do our customers come from?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What zip codes do our customers come from?"}'
```

#### Demographic Analysis
```bash
# Show me vendors of Filipino descent
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors of Filipino descent"}'

# Which vendors identify as currently living in SF?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors identify as currently living in SF?"}'

# Show me demographic breakdown of vendors
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me demographic breakdown of vendors"}'
```

#### Event Comparison
```bash
# Compare attendance between Kapwa Gardens and UNDISCOVERED
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Compare attendance between Kapwa Gardens and UNDISCOVERED"}'

# Which 2023 event had the highest vendor participation?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which 2023 event had the highest vendor participation?"}'

# Show me revenue trends by month for 2024
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me revenue trends by month for 2024"}'
```

### Response Format for Queries
```json
{
  "status": "success",
  "response": "Detailed business intelligence analysis with specific numbers, vendor names, and insights",
  "query_type": "revenue_analysis",
  "data_source": "BigQuery Tables",
  "execution_time": "1.2s",
  "tables_analyzed": ["2023-10-19-UNDISCOVERED", "2024-02-11-Kapwa-Gardens"],
  "records_processed": 247
}
```

---

## 2. DASHBOARD ENDPOINT

### Endpoint: GET /dashboard
**Get structured dashboard data for frontend components**

```bash
curl -X GET https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard
```

**Response:**
```json
{
  "status": "success",
  "data": {
    "financial_summary": {
      "total_revenue": 458430.52,
      "total_vendors": 463,
      "undiscovered_revenue": 308416.44,
      "undiscovered_vendors": 232,
      "kapwa_revenue": 150014.08,
      "kapwa_vendors": 231
    },
    "vendor_performance": {
      "data": [
        {
          "vendor_name": "Victory Hall",
          "total_sales": 24401.15,
          "event_count": 3,
          "sources": "UNDISCOVERED,Kapwa Gardens",
          "rank": 1
        }
      ]
    }
  }
}
```

---

## 3. DATA DISCOVERY

### Endpoint: POST /api/query
**Discover what data is available**

```bash
# See all available tables
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What tables do I have?"}'

# Show me data from a specific event
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me data from Lovers Mart event"}'

# What kind of vendor information do we track?
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What kind of vendor information do we track?"}'
```

---

## 4. COMPLEX BUSINESS INTELLIGENCE

### Advanced Multi-Table Analysis
```bash
# Cross-event vendor analysis
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors who made over $1000 across multiple events"}'

# Revenue performance by event type
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Compare average vendor revenue between indoor and outdoor events"}'

# Customer retention analysis
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which customers attended multiple events in 2023?"}'
```

### Grant and Funding Analysis
```bash
# Vendor eligibility for programs
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me minority-owned vendors for grant applications"}'

# Geographic distribution for funding
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What percentage of vendors are from underserved areas?"}'
```

---

## 5. INTEGRATION EXAMPLES

### JavaScript Frontend
```javascript
// Natural Language Query
async function askBusinessQuestion(question) {
  const response = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: question })
  });
  return await response.json();
}

// Dashboard Data
async function getDashboardData() {
  const response = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard');
  return await response.json();
}

// Usage Examples
const topVendors = await askBusinessQuestion("Who are the top 5 vendors by revenue?");
const dashboard = await getDashboardData();
```

### Python Integration
```python
import requests

# Natural Language Query
def ask_business_question(question):
    response = requests.post(
        'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': question}
    )
    return response.json()

# Dashboard Data
def get_dashboard_data():
    response = requests.get('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard')
    return response.json()

# Examples
revenue_analysis = ask_business_question("What was our total revenue in 2024?")
dashboard_data = get_dashboard_data()
```

---

## 6. KEY CAPABILITIES

### What the API Can Answer:
- ✅ **Revenue Analysis**: "Which event made the most money?"
- ✅ **Vendor Performance**: "Show me top vendors by sales"
- ✅ **Contact Extraction**: "Get email addresses of food vendors"
- ✅ **Geographic Analysis**: "How many attendees from San Francisco?"
- ✅ **Demographic Filtering**: "Show me Filipino vendors"
- ✅ **Cross-Event Analysis**: "Vendors in both Kapwa Gardens and UNDISCOVERED"
- ✅ **Financial Thresholds**: "Vendors who made over $500"
- ✅ **Event Comparisons**: "Compare 2023 vs 2024 attendance"
- ✅ **Grant Eligibility**: "Minority-owned vendors for programs"
- ✅ **Customer Analysis**: "Repeat customers across events"

### Data Sources Available:
- **Close-out Sales Sheets**: 33 tables with vendor revenue data
- **Squarespace Registration**: 4 tables with vendor/attendee contact info
- **Typeform Responses**: 1 table with Balay Kreative responses
- **Total Data**: 38 tables covering 2020-2024 events

### Response Types:
- **Structured Data**: Tables, lists, and specific numbers
- **Business Intelligence**: Analysis with insights and recommendations
- **Contact Information**: Email addresses, phone numbers
- **Financial Metrics**: Revenue totals, averages, rankings
- **Geographic Data**: Zip codes, cities, regional analysis

This API provides complete access to your business intelligence through both natural language queries and structured dashboard data.