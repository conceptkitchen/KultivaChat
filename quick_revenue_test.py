#!/usr/bin/env python3
import requests
import json

response = requests.post('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/query', 
    json={'query': 'Which Kapwa Gardens vendors made over $500?'}, 
    headers={'Content-Type': 'application/json'})

print(f'Status: {response.status_code}')
print(f'Response: {json.dumps(response.json(), indent=2)}')