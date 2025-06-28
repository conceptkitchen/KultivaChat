# Kultivate AI Business Intelligence API

## Overview

Ask any business question in plain English and get comprehensive analysis with specific financial metrics, vendor identification, and actionable insights.

**Base URL**: `https://kultiva-chatv-2-mcp-conceptkitchen.replit.app`

## Single Endpoint

### Natural Language Query
**POST** `/api/query`

Send any business question and receive professional business intelligence analysis.

**Request:**
```json
{
  "query": "Your business question in plain English"
}
```

**Response:**
```json
{
  "business_intelligence": "Comprehensive analysis with specific metrics",
  "query_processed": "Your original question",
  "processing_time": "Response time in seconds",
  "data_source": "Source table information"
}
```

## Example Usage

### Revenue Analysis
**Request:**
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "How much money did vendors make?"}'
```

**Response:**
```json
{
  "business_intelligence": "FINANCIAL PERFORMANCE: $3,330.00 total revenue across 17 transactions | Revenue metrics: Average $195.88, Range $0.00 to $780.00 | High performers: 8 transactions above average, totaling $3,120.00"
}
```

### Vendor Analysis
**Request:**
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are our top vendors?"}'
```

**Response:**
```json
{
  "business_intelligence": "VENDOR ANALYSIS: 40 unique businesses identified | Vendors include: Augusto Gonzales, Native Sol, Oodaalolly, Bongagung Supermarket, Hatzumomo, Paulo Manaid, Honey My Heart, Jungle Dog"
}
```

### Contact Information
**Request:**
```bash
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -H "Content-Type: application/json" \
  -d '{"query": "What email addresses do we have for vendors?"}'
```

## What You Can Ask

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

## Programming Language Examples

### JavaScript
```javascript
async function getBusinessIntelligence(question) {
  const response = await fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: question })
  });
  
  const data = await response.json();
  return data.business_intelligence;
}

// Usage
const revenue = await getBusinessIntelligence("How much revenue did we make?");
const vendors = await getBusinessIntelligence("Who are our best vendors?");
```

### Python
```python
import requests

def get_business_insight(question):
    response = requests.post(
        'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query',
        json={'query': question}
    )
    return response.json()['business_intelligence']

# Usage
revenue_analysis = get_business_insight("What's our total vendor revenue?")
top_vendors = get_business_insight("Who made the most money?")
```

### PHP
```php
function getBusinessIntelligence($question) {
    $data = json_encode(['query' => $question]);
    $options = [
        'http' => [
            'header' => "Content-Type: application/json\r\n",
            'method' => 'POST',
            'content' => $data
        ]
    ];
    
    $context = stream_context_create($options);
    $result = file_get_contents('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', false, $context);
    $response = json_decode($result, true);
    
    return $response['business_intelligence'];
}

// Usage
$revenue = getBusinessIntelligence("How much money did vendors make?");
```

## Key Features

### ✅ Natural Language Processing
- Ask questions exactly like you would ask a person
- No SQL or technical knowledge required
- AI understands business context automatically

### ✅ Comprehensive Analysis
- Specific dollar amounts and financial metrics
- Real vendor names and business identification
- Performance rankings and statistical analysis
- Executive-level insights for decision making

### ✅ No Authentication Required
- Ready to use immediately
- No API keys or setup needed
- No configuration required

### ✅ Cross-Platform Compatible
- Works with any programming language
- Perfect for websites, mobile apps, dashboards
- Simple REST API integration

## Response Types

### Revenue Analysis
Returns specific financial metrics:
- Total revenue amounts
- Average transaction values
- Performance ranges and distributions
- High performer identification

### Vendor Intelligence
Provides business identification:
- Real vendor and business names
- Performance rankings and comparisons
- Revenue per vendor calculations
- Market participation analysis

### Contact Information
Extracts communication data:
- Email addresses and domains
- Phone numbers and contact methods
- Business contact directories
- Communication channel analysis

### Geographic Analysis
Shows location insights:
- Zip code coverage areas
- City and region representation
- Geographic distribution patterns
- Service area mapping

## Error Handling

If something goes wrong, you receive clear error messages:
```json
{
  "error": "Description of what went wrong",
  "suggestion": "How to fix the issue",
  "status": "error"
}
```

## That's It!

One endpoint, plain English questions, comprehensive business intelligence results. Perfect for any application that needs business data analysis.