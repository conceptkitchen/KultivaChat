# Example Queries - CRM - Kultivate Labs (CURL Tests)

Based on your actual data (2023-2024 events), here are working curl examples:

## VENDOR QUERIES

### 1. Revenue by Event and Date
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much money was made by vendors at UNDISCOVERED events in 2023?"}'
```

### 2. Cross-Year Revenue Comparison  
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which event from 2023 to 2024 made the most money for vendors?"}'
```

### 3. Top Vendors from Specific Event
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who are the top 5 vendors from UNDISCOVERED events from 2023 to 2024?"}'
```

### 4. Vendor Zip Code Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What zip codes are our vendors from who participated from 2023 to 2024?"}'
```

### 5. Demographic Revenue Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors who identify as Filipino made more than $500 from 2023 to 2024?"}'
```

### 6. Food Vendor Emails
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of vendors that sell food at UNDISCOVERED events?"}'
```

### 7. Income-Based Email Extraction
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the email addresses of vendors that make less than $97000 income at Kapwa Gardens events?"}'
```

### 8. Phone Numbers - Event Specific
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the cell phone numbers of vendors that participated in UNDISCOVERED events?"}'
```

### 9. Phone Numbers - Kapwa Gardens
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the cell phone numbers of vendors that participated at Kapwa Gardens events?"}'
```

### 10. Cross-Event Participation ($500+ threshold)
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and made at least $500?"}'
```

### 11. Cross-Event Demographic Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and identify as Asian?"}'
```

## ATTENDEE/DONOR QUERIES

### 12. Most Popular Donor City
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What is the most popular city that our attendees live in at Balay Kreative events?"}'
```

### 13. Attendees by Zip Code
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees live in zip code 94102 at UNDISCOVERED events?"}'
```

### 14. Revenue Threshold Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees paid more than $50 from 2023-2024 at UNDISCOVERED events?"}'
```

### 15. Cross-Event Attendance
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who has attended events at Balay Kreative and UNDISCOVERED in 2023?"}'
```

### 16. City-Based Email Extraction
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "What are the emails of our attendees that live in San Francisco at UNDISCOVERED events?"}'
```

### 17. Multi-City Geographic Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees live in SF and Daly City at Balay Kreative events?"}'
```

### 18. Annual Attendance Count
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How many attendees did we have in 2023 at UNDISCOVERED events?"}'
```

### 19. Event-Specific Revenue
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "How much was paid in 2024 at Kapwa Gardens events?"}'
```

### 20. Grant + Multi-Event Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Who applied to a Balay Kreative Grant and went to our events more than 2 times?"}'
```

### 21. Grant Applicant Geographic Analysis
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which of our Balay Kreative grant applicants live in Daly City?"}'
```

### 22. Grant Applicant Demographics
```bash
curl -X POST http://localhost:8081/api/query \
  -H "Content-Type: application/json" \
  -d '{"query": "Which of our Balay Kreative grant applicants identify as Filipino?"}'
```

## NOTES:
- All queries use your actual events: UNDISCOVERED, Kapwa Gardens, Balay Kreative, Lovers Mart
- Date ranges updated to 2023-2024 (your actual data range)
- Demographic terms use your authentic values: Filipino, Asian, Woman, LGBTQ+
- Income thresholds use your actual registration form values: $97000, $82900
- All queries should work with your current API without modifications