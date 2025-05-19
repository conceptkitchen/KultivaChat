import axios from "axios";
import { DisplayContent } from "../../client/src/lib/utils";

// Simpler function to fetch data from Keboola API
export async function getKeboolaData() {
  const apiKey = process.env.KEBOOLA_API_KEY;
  
  if (!apiKey) {
    throw new Error("Keboola API key is not configured");
  }
  
  try {
    // Get tables from Keboola API
    const response = await axios.get('https://connection.keboola.com/v2/storage/tables', {
      headers: {
        'X-StorageApi-Token': apiKey
      }
    });
    
    // Get first table data
    if (response.data && response.data.length > 0) {
      const tableId = response.data[0].id;
      
      const tableData = await axios.get(`https://connection.keboola.com/v2/storage/tables/${tableId}/export`, {
        headers: {
          'X-StorageApi-Token': apiKey
        }
      });
      
      return tableData.data;
    } else {
      // No tables found
      return [];
    }
  } catch (error) {
    console.error("Error fetching data from Keboola:", error);
    // Return empty array if we can't get real data
    return [];
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
    name: item.date || item.DATE || Object.values(item)[0],
    value: item.revenue || item.REVENUE || item.AMOUNT || item.VALUE || parseInt(Object.values(item)[1] as string) || 0
  }));
  
  // Generate appropriate visualization
  if (isBarChart) {
    return {
      message: "Here's a bar chart visualization of the data:",
      displays: [
        {
          type: "visualization" as const,
          title: "Data Visualization",
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
function DataChart() {
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
          type: "table" as const,
          title: "Data Table",
          content: data.slice(0, 10) // Limit to 10 rows for display
        }
      ]
    };
  }
}