import requests
import json
import sys

def quick_test():
    """Quick validation of API documentation accuracy"""
    url = 'https://kultivate-chat-ck.replit.app'
    
    tests = [
        ('Health Check', 'GET', f'{url}/api/health', None),
        ('Tables Discovery', 'POST', f'{url}/api/v1/data/tables', {}),
        ('SQL Execution', 'POST', f'{url}/api/v1/data/sql', {'sql': 'SELECT 1 as test'}),
        ('Smart Router', 'POST', f'{url}/api/v1/data/query', {'query': 'show tables'})
    ]
    
    results = []
    
    for name, method, endpoint, payload in tests:
        try:
            if method == 'GET':
                r = requests.get(endpoint, timeout=5)
            else:
                r = requests.post(endpoint, json=payload, timeout=5)
            
            status = '✓' if r.status_code == 200 else '✗'
            results.append(f"{status} {name}: {r.status_code}")
            
            if r.status_code == 200:
                data = r.json()
                if name == 'Tables Discovery':
                    count = len(data.get('data', []))
                    results.append(f"    Found {count} tables")
                elif name == 'SQL Execution':
                    rows = data.get('rows_returned', 0)
                    results.append(f"    Returned {rows} rows")
                elif name == 'Smart Router':
                    route = data.get('route_used', 'unknown')
                    results.append(f"    Routed to: {route}")
                    
        except Exception as e:
            results.append(f"✗ {name}: Error - {str(e)[:50]}")
    
    for result in results:
        print(result)

if __name__ == '__main__':
    quick_test()