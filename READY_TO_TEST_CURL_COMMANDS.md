# Ready-to-Test CURL Commands

Copy and paste these commands to test your API:

## VENDOR QUERIES

```bash
# 1. Revenue by Event and Date
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How much money was made by vendors at UNDISCOVERED events in 2023?"}'

# 2. Cross-Year Revenue Comparison  
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which event from 2023 to 2024 made the most money for vendors?"}'

# 3. Top Vendors from Specific Event
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Who are the top 5 vendors from UNDISCOVERED events from 2023 to 2024?"}'

# 4. Vendor Zip Code Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What zip codes are our vendors from who participated from 2023 to 2024?"}'

# 5. Demographic Revenue Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which vendors who identify as Filipino made more than $500 from 2023 to 2024?"}'

# 6. Food Vendor Emails
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What are the email addresses of vendors that sell food at UNDISCOVERED events?"}'

# 7. Income-Based Email Extraction
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What are the email addresses of vendors that make less than $97000 income at Kapwa Gardens events?"}'

# 8. Phone Numbers - Event Specific
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What are the cell phone numbers of vendors that participated in UNDISCOVERED events?"}'

# 9. Phone Numbers - Kapwa Gardens
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What are the cell phone numbers of vendors that participated at Kapwa Gardens events?"}'

# 10. Cross-Event Participation ($500+ threshold)
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and made at least $500?"}'

# 11. Cross-Event Demographic Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which vendors participated in Kapwa Gardens events and UNDISCOVERED events from 2023-2024 and identify as Asian?"}'
```

## ATTENDEE/DONOR QUERIES

```bash
# 12. Most Popular Donor City
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What is the most popular city that our attendees live in at Balay Kreative events?"}'

# 13. Attendees by Zip Code
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How many attendees live in zip code 94102 at UNDISCOVERED events?"}'

# 14. Revenue Threshold Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How many attendees paid more than $50 from 2023-2024 at UNDISCOVERED events?"}'

# 15. Cross-Event Attendance
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Who has attended events at Balay Kreative and UNDISCOVERED in 2023?"}'

# 16. City-Based Email Extraction
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What are the emails of our attendees that live in San Francisco at UNDISCOVERED events?"}'

# 17. Multi-City Geographic Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How many attendees live in SF and Daly City at Balay Kreative events?"}'

# 18. Annual Attendance Count
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How many attendees did we have in 2023 at UNDISCOVERED events?"}'

# 19. Event-Specific Revenue
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "How much was paid in 2024 at Kapwa Gardens events?"}'

# 20. Grant + Multi-Event Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Who applied to a Balay Kreative Grant and went to our events more than 2 times?"}'

# 21. Grant Applicant Geographic Analysis
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which of our Balay Kreative grant applicants live in Daly City?"}'

# 22. Grant Applicant Demographics
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Which of our Balay Kreative grant applicants identify as Filipino?"}'
```

## DATA AVAILABILITY QUERIES

```bash
# Check what years of sales data you have
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What years of sales data do we have?"}'

# List all available events and dates
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Show me all events and their dates from our sales data"}'

# Check specific year availability
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Do we have sales data from 2023?"}'

# Check date range coverage
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "What is the date range of our sales data?"}'
```

## SIMPLE TEST QUERIES

```bash
# Quick Contact Test
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Show me 5 vendor emails from UNDISCOVERED events"}'

# Quick Revenue Test
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Show me vendors who made more than $500 at UNDISCOVERED events"}'

# Quick Demographics Test
curl -X POST http://localhost:8081/api/query -H "Content-Type: application/json" -d '{"query": "Show me Filipino vendors from UNDISCOVERED events"}'
```

All commands use your actual events (UNDISCOVERED, Kapwa Gardens, Balay Kreative) and real date ranges (2023-2024).