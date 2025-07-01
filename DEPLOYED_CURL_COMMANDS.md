# Deployed API Curl Commands

Test your deployed API at: https://kultiva-chatv-2-mcp-conceptkitchen.replit.app

## DATA AVAILABILITY QUERIES

```bash
# Check what years of sales data you have
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What years of sales data do we have?"}'

# List all available events and dates
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me all events and their dates from our sales data"}'

# Check specific year availability
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Do we have sales data from 2023?"}'

# Check date range coverage
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What is the date range of our sales data?"}'
```

## VENDOR REVENUE QUERIES

```bash
# Revenue by Event and Date
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much money was made by vendors at UNDISCOVERED events in 2023?"}'

# Cross-Year Revenue Comparison  
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which event from 2023 to 2024 made the most money for vendors?"}'

# Top Vendors from Specific Event
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are the top 5 vendors from UNDISCOVERED events from 2023 to 2024?"}'
```

## CONTACT EXTRACTION QUERIES

```bash
# Food Vendor Emails
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of vendors that sell food at UNDISCOVERED events?"}'

# Phone Numbers - Event Specific
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the cell phone numbers of vendors that participated in UNDISCOVERED events?"}'

# City-Based Email Extraction
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the emails of our attendees that live in San Francisco at UNDISCOVERED events?"}'
```

## DEMOGRAPHIC ANALYSIS QUERIES

```bash
# Demographic Revenue Analysis
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors who identify as Filipino made more than $500 from 2023 to 2024?"}'

# Cross-Event Demographic Analysis
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and identify as Asian?"}'

# Grant Applicant Demographics
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which of our Balay Kreative grant applicants identify as Filipino?"}'
```

## SIMPLE TEST QUERIES

```bash
# Quick Contact Test
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me 5 vendor emails from UNDISCOVERED events"}'

# Quick Revenue Test
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendors who made more than $500 at UNDISCOVERED events"}'

# Quick Demographics Test
curl -X POST https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me Filipino vendors from UNDISCOVERED events"}'
```

All commands use your deployed API endpoint and authentic event data (UNDISCOVERED, Kapwa Gardens, Balay Kreative) with real date ranges (2023-2024).