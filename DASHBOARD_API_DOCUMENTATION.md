# Kultivate AI Dashboard API Documentation

## Base URL
```
https://kultiva-chatv-2-mcp-conceptkitchen.replit.app
```

## Authentication
No authentication required for dashboard endpoints.

## Endpoints

### GET /dashboard
Returns complete dashboard data with financial summaries and vendor performance.

**Request:**
```bash
curl -X GET https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard
```

**Response Structure:**
```json
{
  "status": "success",
  "data": {
    "financial_summary": {
      "total_revenue": 458430.52,
      "total_vendors": 463,
      "undiscovered_revenue": 308416.44,
      "undiscovered_vendors": 232,
      "kapwa_revenue": 150014.08,
      "kapwa_vendors": 231
    },
    "vendor_performance": {
      "data": [
        {
          "vendor_name": "Victory Hall",
          "total_sales": 24401.15,
          "event_count": 3,
          "sources": "UNDISCOVERED,Kapwa Gardens",
          "rank": 1
        },
        {
          "vendor_name": "Street Stix",
          "total_sales": 5593.00,
          "event_count": 2,
          "sources": "UNDISCOVERED",
          "rank": 2
        }
        // ... up to 20 top vendors
      ]
    }
  },
  "metadata": {
    "total_vendors": 463,
    "total_revenue": 458430.52,
    "undiscovered_revenue": 308416.44,
    "kapwa_revenue": 150014.08,
    "data_source": "Transformation CSV Files (SQL Processed)",
    "last_updated": "2025-07-01T06:14:11.123456"
  }
}
```

## Data Fields Explanation

### Financial Summary
- `total_revenue`: Combined revenue from both event series ($458,430.52)
- `total_vendors`: Total unique vendors across all events (463)
- `undiscovered_revenue`: Revenue from UNDISCOVERED events ($308,416.44)
- `undiscovered_vendors`: Number of vendors in UNDISCOVERED events (232)
- `kapwa_revenue`: Revenue from Kapwa Gardens events ($150,014.08)
- `kapwa_vendors`: Number of vendors in Kapwa Gardens events (231)

### Vendor Performance
- `vendor_name`: Name of the vendor
- `total_sales`: Combined sales across all events they participated in
- `event_count`: Number of events this vendor participated in
- `sources`: Which event series they participated in (UNDISCOVERED, Kapwa Gardens, or both)
- `rank`: Performance ranking (1 = highest revenue)

## Frontend Integration Examples

### JavaScript/React
```javascript
const DashboardData = () => {
  const [dashboardData, setDashboardData] = useState(null);
  
  useEffect(() => {
    fetch('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard')
      .then(response => response.json())
      .then(data => {
        setDashboardData(data);
      });
  }, []);

  if (!dashboardData) return <div>Loading...</div>;

  const { financial_summary, vendor_performance } = dashboardData.data;
  
  return (
    <div>
      <h2>Financial Summary</h2>
      <p>Total Revenue: ${financial_summary.total_revenue.toLocaleString()}</p>
      <p>Total Vendors: {financial_summary.total_vendors}</p>
      
      <h2>Top Vendors</h2>
      {vendor_performance.data.map((vendor, index) => (
        <div key={index}>
          <p>{vendor.vendor_name}: ${vendor.total_sales.toLocaleString()}</p>
        </div>
      ))}
    </div>
  );
};
```

### Python
```python
import requests

response = requests.get('https://kultiva-chatv-2-mcp-conceptkitchen.replit.app/dashboard')
data = response.json()

financial = data['data']['financial_summary']
vendors = data['data']['vendor_performance']['data']

print(f"Total Revenue: ${financial['total_revenue']:,.2f}")
print(f"Total Vendors: {financial['total_vendors']}")
print(f"Top Vendor: {vendors[0]['vendor_name']} - ${vendors[0]['total_sales']:,.2f}")
```

## Key Business Metrics

### Revenue Breakdown
- **UNDISCOVERED Events**: $308,416.44 (67% of total revenue)
- **Kapwa Gardens Events**: $150,014.08 (33% of total revenue)
- **Combined Total**: $458,430.52

### Vendor Distribution
- **Total Active Vendors**: 463
- **UNDISCOVERED Vendors**: 232
- **Kapwa Gardens Vendors**: 231
- **Cross-Event Participation**: Some vendors appear in both series

### Top Performers
1. **Victory Hall**: $24,401.15 (participated in 3 events across both series)
2. **Street Stix**: $5,593.00 (participated in 2 UNDISCOVERED events)
3. Additional top 20 vendors ranked by total sales

## Data Source Information
- **Source**: Authentic transformation CSV files from user's business records
- **Processing**: SQL-based aggregation for accurate vendor performance across multiple events
- **Update Frequency**: Data reflects latest transformation files provided
- **Accuracy**: 100% authentic data with no placeholder or mock values

## Error Handling
If the endpoint returns an error:
```json
{
  "error": "Error message describing the issue"
}
```

Common HTTP status codes:
- `200`: Success
- `500`: Server error (data loading issue)

## Usage Notes for Frontend Development
1. **Revenue Display**: Format large numbers with commas (e.g., $458,430.52)
2. **Vendor Rankings**: Use the `rank` field for consistent ordering
3. **Event Attribution**: Use `sources` field to show which events vendors participated in
4. **Performance Metrics**: `event_count` shows vendor consistency across multiple events
5. **Data Freshness**: Check `last_updated` timestamp for data currency

## Sample Dashboard Components

### Revenue Summary Card
```javascript
const RevenueSummary = ({ financial_summary }) => (
  <div className="revenue-card">
    <h3>Total Revenue</h3>
    <h1>${financial_summary.total_revenue.toLocaleString()}</h1>
    <div className="breakdown">
      <div>UNDISCOVERED: ${financial_summary.undiscovered_revenue.toLocaleString()}</div>
      <div>Kapwa Gardens: ${financial_summary.kapwa_revenue.toLocaleString()}</div>
    </div>
    <p>{financial_summary.total_vendors} Total Vendors</p>
  </div>
);
```

### Top Vendors Table
```javascript
const TopVendorsTable = ({ vendor_performance }) => (
  <table className="vendors-table">
    <thead>
      <tr>
        <th>Rank</th>
        <th>Vendor Name</th>
        <th>Total Sales</th>
        <th>Events</th>
        <th>Series</th>
      </tr>
    </thead>
    <tbody>
      {vendor_performance.data.map(vendor => (
        <tr key={vendor.rank}>
          <td>{vendor.rank}</td>
          <td>{vendor.vendor_name}</td>
          <td>${vendor.total_sales.toLocaleString()}</td>
          <td>{vendor.event_count}</td>
          <td>{vendor.sources}</td>
        </tr>
      ))}
    </tbody>
  </table>
);
```

This API provides all the data needed to build a comprehensive dashboard showing the true scope of the business with accurate financial metrics and vendor performance data.