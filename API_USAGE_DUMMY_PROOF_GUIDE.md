# DUMMY-PROOF API USAGE GUIDE
**Kultivate AI Business Intelligence API**

## How It Works (Super Simple)

### 1. **One Single Endpoint** 
**URL**: `https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query`

### 2. **Just Ask Questions in Plain English**
Send any business question as JSON to the endpoint. That's it!

## Examples (Copy & Paste Ready)

### Basic Revenue Question
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "How much money did vendors make?"}'
```

**Result**: 
```json
{
  "business_intelligence": "FINANCIAL PERFORMANCE: $3,330.00 total revenue across 17 transactions | Revenue metrics: Average $195.88, Range $0.00 to $780.00 | High performers: 8 transactions above average, totaling $3,120.00"
}
```

### Vendor Information Question
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are our top vendors?"}'
```

**Result**:
```json
{
  "business_intelligence": "VENDOR ANALYSIS: 40 unique businesses identified | Vendors include: Augusto Gonzales, Native Sol, Oodaalolly, Bongagung Supermarket, Hatzumomo, Paulo Manaid, Honey My Heart, Jungle Dog"
}
```

### Contact Information Question
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "What email addresses do we have for vendors?"}'
```

## JavaScript Example (Website Integration)
```javascript
async function askBusinessQuestion(question) {
  const response = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: question })
  });
  
  const data = await response.json();
  return data.business_intelligence;
}

// Usage examples:
const revenue = await askBusinessQuestion("How much revenue did we make?");
const vendors = await askBusinessQuestion("Who are our best vendors?");
const contacts = await askBusinessQuestion("Show me vendor contact info");
```

## Python Example (Dashboard Integration)
```python
import requests

def get_business_intelligence(question):
    response = requests.post(
        'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': question}
    )
    return response.json()['business_intelligence']

# Usage examples:
revenue_analysis = get_business_intelligence("What's our total vendor revenue?")
top_vendors = get_business_intelligence("Who made the most money?")
vendor_contacts = get_business_intelligence("Give me vendor email addresses")
```

## What You Can Ask (In Plain English)

### Financial Questions
- "How much money did vendors make?"
- "What's our total revenue from events?"
- "Who are the highest earning vendors?"
- "Show me revenue by event"

### Vendor Questions  
- "Who are our top vendors?"
- "List all vendor names"
- "Which vendors participated in multiple events?"
- "Show me vendor performance rankings"

### Contact Questions
- "What email addresses do we have?"
- "Give me vendor phone numbers"
- "Show me contact information for food vendors"
- "What's the contact info for UNDISCOVERED vendors?"

### Geographic Questions
- "What zip codes are our vendors from?"
- "Show me vendor locations"
- "Which cities do our vendors come from?"

### Event Analysis
- "How did Kapwa Gardens events perform?"
- "Show me Yum Yams vendor data"
- "Compare revenue across different events"

## Key Features

### ✅ **Natural Language Processing**
- Ask questions exactly like you would ask a person
- No need to learn SQL or technical terms
- AI understands business context automatically

### ✅ **Comprehensive Analysis**
- Gets specific dollar amounts, not just record counts
- Identifies real vendor names and businesses
- Provides performance metrics and rankings
- Includes business recommendations

### ✅ **Ready-to-Use Results**
- Professional business intelligence summaries
- Executive-level insights for decision making
- Data quality assessments included
- Source attribution for transparency

## Response Format
Every response includes:
```json
{
  "business_intelligence": "Detailed analysis with specific metrics",
  "query_processed": "Your original question",
  "processing_time": "1.2 seconds",
  "data_source": "Event/table information"
}
```

## No Authentication Required
The API is ready to use immediately - no API keys, no setup, no configuration needed.

## Error Handling
If something goes wrong, you get clear error messages:
```json
{
  "error": "Clear explanation of what went wrong",
  "suggestion": "How to fix the issue"
}
```

## That's It!
One endpoint, plain English questions, comprehensive business intelligence results. Perfect for dashboards, websites, mobile apps, or any system that needs business data analysis.