# CURL Test Commands for Kultivate AI Business Intelligence API

## Vendor Queries

### 1. Event Revenue Query
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much money was made by vendors at UNDISCOVERED events?"}' \
  --max-time 30
```

### 2. Top Revenue Event (2023-2024)
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which event from 2023 to 2024 made the most money for vendors?"}' \
  --max-time 30
```

### 3. Top 5 Vendors
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are the top 5 vendors from UNDISCOVERED events?"}' \
  --max-time 30
```

### 4. Vendor Zip Codes
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What zip codes are our vendors from who participated from 2023 to 2024?"}' \
  --max-time 30
```

### 5. High Revenue Vendors
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors made more than $500 from 2023 to 2024?"}' \
  --max-time 45
```

### 6. Vendor Email Addresses
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of vendors that sell products?"}' \
  --max-time 30
```

### 7. Low Income Vendor Emails
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of vendors that make less than $300?"}' \
  --max-time 30
```

### 8. Vendor Phone Numbers (Yum Yams)
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the phone numbers of vendors that participated in Yum Yams events?"}' \
  --max-time 30
```

### 9. Kapwa Gardens Vendor Phones
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the phone numbers of vendors that participated at Kapwa Gardens?"}' \
  --max-time 30
```

### 10. Cross-Event High Revenue Vendors
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and made at least $500?"}' \
  --max-time 45
```

## Attendee/Donor Queries

### 11. Popular Donor Cities
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What is the most popular city that our donors live in?"}' \
  --max-time 30
```

### 12. Attendees by Zip Code
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees live in 94102 zip code?"}' \
  --max-time 30
```

### 13. High Donation Attendees
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees gave more than $1 from 2023 to 2024?"}' \
  --max-time 30
```

### 14. Cross-Event Attendees
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who has attended events at Balay Kreative and UNDISCOVERED in 2023?"}' \
  --max-time 30
```

### 15. Attendee Emails by City
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the emails of our attendees that live in San Francisco?"}' \
  --max-time 30
```

### 16. SF and Daly City Attendees
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees live in SF and Daly City?"}' \
  --max-time 30
```

### 17. 2023 Attendee Count
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees did we have in 2023?"}' \
  --max-time 30
```

### 18. Kapwa Gardens 2024 Donations
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much was given in 2024 at Kapwa Gardens?"}' \
  --max-time 30
```

### 19. Grant Applicant Event Attendance
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who applied to a Balay Kreative Grant and went to our events more than 2 times?"}' \
  --max-time 30
```

### 20. Daly City Grant Applicants
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which of our Balay Kreative applicants live in Daly City?"}' \
  --max-time 30
```

### 21. Cross-Event Middle Eastern Vendors
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and identify as Middle Eastern?"}' \
  --max-time 45
```

### 22. Grant Applicant Demographics
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which of our Balay Kreative applicants identify as Filipino?"}' \
  --max-time 30
```

## Quick Test Commands

### Basic Functionality Test
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me my tables"}' \
  --max-time 10
```

### Simple Vendor Query
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Show me vendor data"}' \
  --max-time 15
```

### Revenue Analysis Test
```bash
curl -s -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What was the total revenue from one event?"}' \
  --max-time 20
```

## Usage Instructions

1. **Copy any curl command above**
2. **Paste it in your terminal**
3. **Press Enter to test**
4. **Check the JSON response for data**

## Expected Response Format
```json
{
  "status": "success",
  "data": [
    {
      "vendor_name": "Example Vendor",
      "revenue": 750.00,
      "table_source": "event-table-name"
    }
  ],
  "query_executed": "SELECT ...",
  "analysis_type": "business_intelligence"
}
```

## Error Response Format
```json
{
  "status": "error",
  "error_message": "Description of error",
  "query_attempted": "Original query text"
}
```