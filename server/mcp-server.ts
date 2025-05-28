import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  ToolSchema,
} from '@modelcontextprotocol/sdk/types.js';
import { KeboolaMCP } from './services/keboola-mcp.js';

// Initialize MCP Server
const server = new Server(
  {
    name: 'keboola-mcp-server',
    version: '1.0.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Initialize Keboola MCP client
let keboolaMCP: KeboolaMCP;
try {
  keboolaMCP = new KeboolaMCP();
} catch (error) {
  console.error('Failed to initialize Keboola MCP:', error);
}

// Define MCP tools following the Keboola MCP Server specification
const tools: ToolSchema[] = [
  {
    name: 'retrieve_buckets',
    description: 'Retrieves information about all buckets in the project.',
    inputSchema: {
      type: 'object',
      properties: {},
    },
  },
  {
    name: 'get_bucket_detail',
    description: 'Gets detailed information about a specific bucket.',
    inputSchema: {
      type: 'object',
      properties: {
        bucket_id: {
          type: 'string',
          description: 'Unique ID of the bucket.',
        },
      },
      required: ['bucket_id'],
    },
  },
  {
    name: 'retrieve_bucket_tables',
    description: 'Retrieves all tables in a specific bucket with their basic information.',
    inputSchema: {
      type: 'object',
      properties: {
        bucket_id: {
          type: 'string',
          description: 'Unique ID of the bucket.',
        },
      },
      required: ['bucket_id'],
    },
  },
  {
    name: 'get_table_detail',
    description: 'Gets detailed information about a specific table including its DB identifier and column information.',
    inputSchema: {
      type: 'object',
      properties: {
        table_id: {
          type: 'string',
          description: 'Unique ID of the table.',
        },
      },
      required: ['table_id'],
    },
  },
  {
    name: 'query_table',
    description: 'Executes an SQL SELECT query to get the data from the underlying database.',
    inputSchema: {
      type: 'object',
      properties: {
        sql_query: {
          type: 'string',
          description: 'SQL SELECT query to run.',
        },
      },
      required: ['sql_query'],
    },
  },
  {
    name: 'get_sql_dialect',
    description: 'Gets the name of the SQL dialect used by Keboola project\'s underlying database.',
    inputSchema: {
      type: 'object',
      properties: {},
    },
  },
  {
    name: 'retrieve_jobs',
    description: 'Retrieves all jobs in the project, or filter jobs by a specific component_id or config_id, with optional status filtering.',
    inputSchema: {
      type: 'object',
      properties: {
        component_id: {
          type: 'string',
          description: 'Optional component ID to filter jobs.',
        },
        config_id: {
          type: 'string',
          description: 'Optional config ID to filter jobs.',
        },
        status: {
          type: 'string',
          description: 'Optional status to filter jobs.',
        },
      },
    },
  },
  {
    name: 'get_job_detail',
    description: 'Retrieves detailed information about a specific job.',
    inputSchema: {
      type: 'object',
      properties: {
        job_id: {
          type: 'string',
          description: 'Unique ID of the job.',
        },
      },
      required: ['job_id'],
    },
  },
  {
    name: 'find_component_id',
    description: 'Returns list of component IDs that match the given query.',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Search query for component IDs.',
        },
      },
      required: ['query'],
    },
  },
];

// List tools handler
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return {
    tools,
  };
});

// Call tool handler
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  if (!keboolaMCP) {
    throw new Error('Keboola MCP client not initialized');
  }

  try {
    switch (name) {
      case 'retrieve_buckets':
        const buckets = await keboolaMCP.retrieveBuckets();
        return {
          content: [
            {
              type: 'text',
              text: `Found ${buckets.length} buckets in your Keboola project:\n\n${buckets.map(bucket => 
                `• ${bucket.name} (${bucket.stage}) - ${bucket.description || 'No description'}`
              ).join('\n')}`,
            },
          ],
        };

      case 'get_bucket_detail':
        const bucket = await keboolaMCP.getBucketDetail(args.bucket_id);
        return {
          content: [
            {
              type: 'text',
              text: `Bucket Details:\n\nName: ${bucket.name}\nID: ${bucket.id}\nStage: ${bucket.stage}\nDescription: ${bucket.description || 'No description'}\nCreated: ${bucket.created}\nLast Modified: ${bucket.lastChangeDate}`,
            },
          ],
        };

      case 'retrieve_bucket_tables':
        const tables = await keboolaMCP.retrieveBucketTables(args.bucket_id);
        return {
          content: [
            {
              type: 'text',
              text: `Found ${tables.length} tables in bucket:\n\n${tables.map(table => 
                `• ${table.name} (${table.rowsCount} rows, ${table.columns.length} columns)`
              ).join('\n')}`,
            },
          ],
        };

      case 'get_table_detail':
        const tableDetail = await keboolaMCP.getTableDetail(args.table_id);
        return {
          content: [
            {
              type: 'text',
              text: `Table: ${tableDetail.name}\nRows: ${tableDetail.rowsCount}\nColumns: ${tableDetail.columns.join(', ')}\nSize: ${Math.round(tableDetail.dataSizeBytes / 1024 / 1024 * 100) / 100} MB\nLast Updated: ${tableDetail.lastChangeDate}`,
            },
          ],
        };

      case 'query_table':
        const queryResults = await keboolaMCP.queryTable(args.sql_query);
        return {
          content: [
            {
              type: 'text',
              text: `Query Results (${queryResults.length} rows):\n\n${JSON.stringify(queryResults, null, 2)}`,
            },
          ],
        };

      case 'get_sql_dialect':
        const dialect = await keboolaMCP.getSqlDialect();
        return {
          content: [
            {
              type: 'text',
              text: `SQL Dialect: ${dialect}`,
            },
          ],
        };

      case 'retrieve_jobs':
        const jobs = await keboolaMCP.retrieveJobs(args.component_id, args.config_id, args.status);
        return {
          content: [
            {
              type: 'text',
              text: `Found ${jobs.length} jobs:\n\n${jobs.map(job => 
                `• Job ${job.id}: ${job.status} (${job.component})`
              ).join('\n')}`,
            },
          ],
        };

      case 'get_job_detail':
        const jobDetail = await keboolaMCP.getJobDetail(args.job_id);
        return {
          content: [
            {
              type: 'text',
              text: `Job ${jobDetail.id}\nStatus: ${jobDetail.status}\nComponent: ${jobDetail.component}\nDuration: ${jobDetail.durationSeconds}s\nCreated: ${jobDetail.createdTime}`,
            },
          ],
        };

      case 'find_component_id':
        const componentIds = await keboolaMCP.findComponentId(args.query);
        return {
          content: [
            {
              type: 'text',
              text: `Found component IDs: ${componentIds.join(', ')}`,
            },
          ],
        };

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    throw new Error(`Tool execution failed: ${error.message}`);
  }
});

// Function to get MCP server response for chat integration
export async function getMCPResponse(userMessage: string): Promise<{ content: string; displays?: any[] }> {
  // Analyze user intent and call appropriate tools
  const message = userMessage.toLowerCase();
  
  try {
    if (message.includes('bucket') && !message.includes('table')) {
      // List all buckets
      const buckets = await keboolaMCP.retrieveBuckets();
      const response = keboolaMCP.generateDataResponse(buckets, userMessage, 'buckets');
      return {
        content: response.message,
        displays: response.displays
      };
    } 
    
    if (message.includes('table') || message.includes('data')) {
      // Get tables from relevant buckets
      const buckets = await keboolaMCP.retrieveBuckets();
      
      // Find specific bucket if mentioned
      let targetBucket = buckets[0];
      for (const bucket of buckets) {
        if (message.includes(bucket.name.toLowerCase()) || 
            message.includes(bucket.id.toLowerCase())) {
          targetBucket = bucket;
          break;
        }
      }
      
      const tables = await keboolaMCP.retrieveBucketTables(targetBucket.id);
      
      if (tables.length > 0) {
        // Try to get actual data
        try {
          const firstTable = tables[0];
          const sampleQuery = `SELECT * FROM "${firstTable.id}" LIMIT 10`;
          const actualData = await keboolaMCP.queryTable(sampleQuery);
          
          return {
            content: `Here's data from "${firstTable.name}" in bucket "${targetBucket.name}":`,
            displays: [{
              type: "table",
              title: `${firstTable.name} - Sample Data`,
              content: actualData
            }]
          };
        } catch (queryError) {
          // Fallback to table structure
          const tablesList = tables.map(table => ({
            'Table Name': table.name || 'Unknown',
            'Records': table.rowsCount || 0,
            'Columns': (table.columns && table.columns.length) || 0,
            'Last Updated': table.lastChangeDate ? new Date(table.lastChangeDate).toLocaleDateString() : 'Unknown'
          }));
          
          return {
            content: `Found ${tables.length} tables in "${targetBucket.name}":`,
            displays: [{
              type: "table",
              title: `Tables in ${targetBucket.name}`,
              content: tablesList
            }]
          };
        }
      }
    }
    
    if (message.includes('job')) {
      const jobs = await keboolaMCP.retrieveJobs();
      const response = keboolaMCP.generateDataResponse(jobs, userMessage, 'jobs');
      return {
        content: response.message,
        displays: response.displays
      };
    }
    
    // Default: show project overview
    const buckets = await keboolaMCP.retrieveBuckets();
    const response = keboolaMCP.generateDataResponse(buckets, userMessage, 'buckets');
    return {
      content: `Here's your Keboola project overview with ${buckets.length} buckets:`,
      displays: response.displays
    };
    
  } catch (error) {
    throw new Error(`MCP Server error: ${error.message}`);
  }
}

// Start the server if running directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const transport = new StdioServerTransport();
  server.connect(transport);
}

export { server };