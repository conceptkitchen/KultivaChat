# Dashboard API Documentation

## Overview

The Dashboard API provides specialized endpoints for financial data visualization and business intelligence dashboards. This API uses a separate Keboola workspace (`KBC_WORKSPACE_SCHEMA_DASHBOARD`) for financial close-out sales data with CSV fallbacks for reliable data access.

## Base URL

**Production**: `https://kultiva-chatv-2-mcp-conceptkitchen.replit.app`

## Authentication

No authentication required. The API uses server-side credential management.

## Dashboard Endpoints

### 1. Financial Summary

**GET** `/api/dashboard/financial-summary`

Get aggregated financial metrics by event type (Kapwa Gardens vs UNDISCOVERED).

**Response Example:**
```json
{
  "status": "success",
  "data": [
    {
      "event_type": "Kapwa Gardens",
      "total_revenue": 8760.84,
      "vendor_count": 14
    },
    {
      "event_type": "UNDISCOVERED",
      "total_revenue": 31847.45,
      "vendor_count": 52
    }
  ],
  "source": "csv_fallback"
}
```

### 2. Vendor Performance

**GET** `/api/dashboard/vendor-performance`

Get top 20 performing vendors by revenue across all events.

**Response Example:**
```json
{
  "status": "success",
  "data": [
    {
      "vendor_name": "Street Stix",
      "event_name": "UNDISCOVERED SF",
      "revenue": 5593.0
    },
    {
      "vendor_name": "Fely's Siomai Asian Cuisines",
      "event_name": "UNDISCOVERED SF", 
      "revenue": 3907.0
    }
  ],
  "source": "csv_fallback"
}
```

### 3. Event Timeline

**GET** `/api/dashboard/event-timeline`

Get chronological event data with revenue and vendor counts by date.

**Response Example:**
```json
{
  "status": "success",
  "data": [
    {
      "date": "2023-02-11",
      "event_name": "02",
      "total_revenue": 4925.0,
      "vendor_count": 12
    },
    {
      "date": "2023-03-18", 
      "event_name": "Sari Sari Saturday",
      "total_revenue": 6491.81,
      "vendor_count": 15
    }
  ],
  "source": "csv_fallback"
}
```

### 4. Revenue Breakdown

**GET** `/api/dashboard/revenue-breakdown`

Get revenue distribution by individual events for pie charts and analysis.

**Response Example:**
```json
{
  "status": "success",
  "data": [
    {
      "event_name": "UNDISCOVERED SF",
      "revenue": 31847.45
    },
    {
      "event_name": "Sari Sari Saturday", 
      "revenue": 6491.81
    }
  ],
  "total_revenue": 40599.10,
  "source": "csv_fallback"
}
```

## Data Sources

### Primary Data Source
- **BigQuery Workspace**: Uses `KBC_WORKSPACE_SCHEMA_DASHBOARD` environment variable
- **Credentials**: `GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD` JSON file

### Fallback Data Source
- **CSV Files**: Reliable fallback with close-out sales transformation data
  - `kapwa_gardens_dashboard_data.csv`: Kapwa Gardens financial records
  - `undiscovered_dashboard_data.csv`: UNDISCOVERED event financial records

## Error Handling

All endpoints return consistent error format:

```json
{
  "error": "Error description"
}
```

HTTP Status Codes:
- `200`: Success
- `500`: Server error

## Integration Examples

### JavaScript/React
```javascript
// Fetch financial summary
const response = await fetch('/api/dashboard/financial-summary');
const data = await response.json();

// Use in dashboard component
const totalRevenue = data.data.reduce((sum, item) => sum + item.total_revenue, 0);
```

### Python
```python
import requests

# Get vendor performance data
response = requests.get('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/dashboard/vendor-performance')
vendors = response.json()['data']

# Top vendor
top_vendor = vendors[0] if vendors else None
```

### cURL
```bash
# Test financial summary endpoint
curl -X GET https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/dashboard/financial-summary

# Test vendor performance endpoint  
curl -X GET https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/api/dashboard/vendor-performance
```

## Dashboard Use Cases

1. **Executive Dashboard**: Use financial-summary for high-level KPIs
2. **Vendor Analysis**: Use vendor-performance for top performer identification
3. **Timeline Visualization**: Use event-timeline for chronological revenue tracking
4. **Revenue Distribution**: Use revenue-breakdown for pie charts and event comparison

## Technical Notes

- All currency calculations handle string conversion and remove non-numeric characters
- CSV fallback ensures 100% uptime even during BigQuery workspace issues
- Revenue calculations use authentic close-out sales data
- Vendor names and event details are preserved from source data
- Date formatting follows YYYY-MM-DD standard

## Environment Variables

Required for dashboard workspace connections:

**Close-out Sales Data:**
- `GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES`: Path to service account JSON
- `KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES`: Dashboard workspace schema name

**Squarespace Forms Data:**
- `GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS`: Path to service account JSON
- `KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS`: Squarespace forms workspace schema name

## Support

For dashboard integration support or additional endpoints, refer to the main API documentation or contact the development team.