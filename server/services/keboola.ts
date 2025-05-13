import axios from "axios";
import { DisplayContent } from "../../client/src/lib/utils";

// Mock data since we don't have actual API keys
const mockSalesData = [
  { date: "2023-01-15", region: "North America", product: "Product A", units: 423, revenue: 12690 },
  { date: "2023-01-18", region: "Europe", product: "Product B", units: 156, revenue: 8580 },
  { date: "2023-01-22", region: "Asia Pacific", product: "Product A", units: 385, revenue: 11550 },
  { date: "2023-01-28", region: "South America", product: "Product C", units: 214, revenue: 6420 },
  { date: "2023-02-03", region: "North America", product: "Product B", units: 287, revenue: 15785 },
  { date: "2023-02-10", region: "Europe", product: "Product A", units: 319, revenue: 9570 },
  { date: "2023-02-15", region: "Asia Pacific", product: "Product C", units: 175, revenue: 5250 },
  { date: "2023-02-22", region: "South America", product: "Product A", units: 231, revenue: 6930 }
];

// Function to fetch data from Keboola API
export async function getKeboolaData() {
  // In a real implementation, we would use the API key from environment variables
  const apiKey = process.env.KEBOOLA_API_KEY;
  
  try {
    if (apiKey) {
      // Real API call would go here
      // const response = await axios.get('https://connection.keboola.com/v2/storage/tables/your-table-id/export', {
      //   headers: {
      //     'X-StorageApi-Token': apiKey,
      //     'Content-Type': 'application/json'
      //   }
      // });
      // return response.data;
    }
    
    // For now, return mock data
    return mockSalesData;
  } catch (error) {
    console.error("Error fetching data from Keboola:", error);
    throw new Error("Failed to fetch data from Keboola API");
  }
}

// Function to generate visualization based on data and query
export async function generateVisualization(data: any[], query: string) {
  // Check what kind of visualization is requested
  const isBarChart = query.toLowerCase().includes("bar") || 
                     query.toLowerCase().includes("chart") ||
                     query.toLowerCase().includes("graph");
  
  // Format data for visualization
  const chartData = data.map(item => ({
    name: item.date,
    value: item.revenue
  }));
  
  // Generate appropriate visualization
  if (isBarChart) {
    return {
      message: "Here's a bar chart visualization of the sales data by date:",
      displays: [
        {
          type: "visualization" as const,
          title: "Sales Revenue by Date",
          content: chartData
        },
        {
          type: "code" as const,
          title: "React Chart Implementation",
          language: "javascript",
          content: `import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';

// Sample data
const data = ${JSON.stringify(chartData, null, 2)};

// Component implementation
function SalesChart() {
  return (
    <div className="w-full h-80">
      <ResponsiveContainer width="100%" height="100%">
        <BarChart data={data}
          margin={{ top: 20, right: 30, left: 20, bottom: 5 }}>
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis dataKey="name" />
          <YAxis />
          <Tooltip />
          <Bar dataKey="value" fill="#E9BE00" />
        </BarChart>
      </ResponsiveContainer>
    </div>
  );
}`
        }
      ]
    };
  } else {
    // Default visualization response
    return {
      message: "Here's the data visualization you requested:",
      displays: [
        {
          type: "visualization" as const,
          title: "Sales Revenue",
          content: chartData
        }
      ]
    };
  }
}
