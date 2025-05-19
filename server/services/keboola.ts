import axios from "axios";
import { DisplayContent } from "../../client/src/lib/utils";
import { db } from "../db";
import { Pool } from "pg";

// PostgreSQL connection for Keboola
interface KeboolaDbConfig {
  host: string;
  port: number;
  database: string;
  user: string;
  password: string;
  schema: string;
}

// Function to connect to Keboola PostgreSQL destination
export async function connectToKeboolaPostgres() {
  // Using the values from your screenshot
  const keboolaDbConfig: KeboolaDbConfig = {
    port: 5432,          // Your Keboola PostgreSQL port
    database: "misty-lake-44017956",  // From your screenshot
    user: "userName",    // From your screenshot (replace with actual value)
    password: process.env.PGPASSWORD || "", // Using existing database password
    host: process.env.PGHOST || "",   // Using existing database host
    schema: "Schema"     // From your screenshot
  };
  
  try {
    // Create a new pool for the Keboola connection
    const pool = new Pool({
      host: keboolaDbConfig.host,
      port: keboolaDbConfig.port,
      database: keboolaDbConfig.database,
      user: keboolaDbConfig.user,
      password: keboolaDbConfig.password
    });
    
    // Test the connection
    const client = await pool.connect();
    const result = await client.query('SELECT NOW()');
    client.release();
    
    console.log("Successfully connected to Keboola PostgreSQL:", result.rows[0]);
    return pool;
  } catch (error) {
    console.error("Error connecting to Keboola PostgreSQL:", error);
    throw new Error("Failed to connect to Keboola PostgreSQL");
  }
}

// Function to fetch data from Keboola API
export async function getKeboolaData() {
  const apiKey = process.env.KEBOOLA_API_KEY;
  
  if (!apiKey) {
    throw new Error("Keboola API key is not configured");
  }
  
  try {
    // First try to get data from Keboola API
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
      }
    } catch (apiError) {
      console.log("Could not get data from Keboola API, trying database connection...");
    }
    
    // If API fails, try the direct PostgreSQL connection
    try {
      // Try to connect to the Keboola PostgreSQL destination
      const keboolaPool = new Pool({
        host: process.env.PGHOST,
        port: parseInt(process.env.PGPORT || '5432'),
        database: "misty-lake-44017956",  // From your screenshot
        user: process.env.PGUSER,
        password: process.env.PGPASSWORD,
        // No schema specified to get data from the public schema first
      });
      
      // List all tables in the database to find the ones from Keboola
      const client = await keboolaPool.connect();
      
      // First, try to see if there are any tables in the Schema that was specified
      let tablesQuery = `
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'Schema' 
        LIMIT 10
      `;
      
      let tablesResult = await client.query(tablesQuery);
      
      // If no tables in 'Schema', look in public schema
      if (tablesResult.rows.length === 0) {
        tablesQuery = `
          SELECT table_name 
          FROM information_schema.tables 
          WHERE table_schema = 'public' 
          LIMIT 10
        `;
        tablesResult = await client.query(tablesQuery);
      }
      
      // If we found tables, query the first one
      if (tablesResult.rows.length > 0) {
        const firstTable = tablesResult.rows[0].table_name;
        const schema = tablesResult.rows.length > 0 ? 'Schema' : 'public';
        
        // Query the first table
        const dataQuery = `SELECT * FROM ${schema}.${firstTable} LIMIT 100`;
        const dataResult = await client.query(dataQuery);
        
        client.release();
        return dataResult.rows;
      }
      
      client.release();
      throw new Error("No tables found in the database");
      
    } catch (dbError) {
      console.error("Error querying Keboola PostgreSQL:", dbError);
      
      // Last resort - try our local database to see if Keboola has synced data there
      try {
        const result = await db.execute(
          `SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'`
        );
        
        if (result.rows.length > 0) {
          // Get the first table and query it
          const tableName = result.rows[0].table_name;
          const dataResult = await db.execute(`SELECT * FROM ${tableName} LIMIT 100`);
          return dataResult.rows;
        }
        
        throw new Error("No tables found in local database");
      } catch (localDbError) {
        console.error("Error querying local database:", localDbError);
        throw new Error("Could not fetch data from Keboola API or any database");
      }
    }
  } catch (error) {
    console.error("All Keboola data access methods failed:", error);
    throw new Error("Failed to fetch data from Keboola using all available methods");
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
