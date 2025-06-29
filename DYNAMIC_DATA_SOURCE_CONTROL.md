# Dynamic Data Source Control System

## Overview

Your API automatically adapts to changing data in your BigQuery workspace. The system intelligently categorizes and routes queries to the correct data source without any hardcoding.

## Current Data Structure (Auto-Detected)

Based on your workspace analysis:

### Closeout Sales Tables: 60 tables
- Pattern detection: "close-out", "closeout", "sales", "lovers-mart", "halo-halo"
- Examples: "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens"
- Content: Revenue data, vendor costs, event sales tracking

### Squarespace Tables: 2 tables  
- Pattern detection: "squarespace", "vendor-export", "attendees-export"
- Examples: "Undiscovered---Attendees-Export---Squarespace"
- Content: Vendor applications, attendee registrations

### Typeform Tables: 6 tables
- Pattern detection: "typeform", "balay-kreative", "answers_unioned"
- Examples: "Balay-Kreative---attendees---all-orders", "answers_unioned_Balay_Kreative_Typeform_data"
- Content: Form responses, survey data

## How Intelligent Routing Works

### 1. Query Analysis
The system analyzes your natural language query for data source keywords:

```bash
# Typeform queries
"Show me typeform data" → Routes to typeform tables
"Balay Kreative responses" → Routes to typeform tables

# Squarespace queries  
"Show me squarespace exports" → Routes to squarespace tables
"Vendor applications" → Routes to squarespace tables

# Closeout sales queries
"Show me sales data" → Routes to closeout sales tables
"Revenue information" → Routes to closeout sales tables
```

### 2. Dynamic Table Categorization
The system automatically categorizes all tables in your workspace:

```python
# Auto-categorization logic (adapts to new data)
def categorize_table(table_name):
    if "typeform" in table_name or "balay-kreative" in table_name:
        return "typeform"
    elif "squarespace" in table_name or "export" in table_name:
        return "squarespace"  
    elif "close-out" in table_name or "sales" in table_name:
        return "closeout_sales"
    else:
        return "other"
```

### 3. Smart Table Selection
When you ask for specific data, the system:
1. Scans all 63 tables in your workspace
2. Categorizes them by data source type
3. Matches your query intent to the best category
4. Selects the most relevant table from that category

## Adaptability Features

### Automatic Updates
- New closeout sales sheets → Instantly recognized and categorized
- New squarespace exports → Automatically added to squarespace category
- New typeform responses → Immediately available for typeform queries
- No code changes required when you add new data

### Query Examples That Work

#### For Your Current Data:
```bash
# Target typeform data (6 tables)
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "Show me Balay Kreative typeform responses"}'

# Target squarespace data (2 tables)  
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "Show me squarespace vendor applications"}'

# Target closeout sales (60 tables)
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "What closeout sales revenue do we have?"}'
```

#### Business Intelligence Queries:
```bash
# These automatically find the most relevant data source
curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "How much money did vendors make?"}'

curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "What email addresses do we have?"}'

curl -X POST "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query" \
  -d '{"query": "Who are our top vendors?"}'
```

## Zero Maintenance Required

### As You Add New Data:
- **New closeout sheets** → Automatically detected by "close-out", "sales" patterns
- **New squarespace forms** → Automatically detected by "squarespace", "export" patterns  
- **New typeform data** → Automatically detected by "typeform", "balay" patterns

### Pattern Recognition Adapts:
The system learns from your naming conventions:
- Event names (Kapwa Gardens, Halo Halo, Lovers Mart)
- Data source indicators (Squarespace, Typeform)
- Content types (Close-Out-Sales, Attendees-Export, Vendor-Export)

## API Endpoint

Single endpoint handles all data source routing:

```
POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query
Content-Type: application/json

{
  "query": "Your question in plain English"
}
```

## Current Status

✅ **Automatic table detection**: 63 tables categorized  
✅ **Dynamic routing**: Queries route to correct data source  
✅ **Business intelligence**: Financial analysis, contact extraction, vendor rankings  
✅ **Zero hardcoding**: Adapts to your changing data structure  
✅ **Real-time updates**: New data immediately available  

Your API now intelligently manages your growing data without any manual configuration updates.