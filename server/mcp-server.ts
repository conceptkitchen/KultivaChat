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

// Define MCP tools following the complete Keboola MCP Server specification
const tools: ToolSchema[] = [
  // Storage Tools
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
    name: 'update_bucket_description',
    description: 'Update the description for a given Keboola bucket.',
    inputSchema: {
      type: 'object',
      properties: {
        bucket_id: {
          type: 'string',
          description: 'The ID of the bucket to update.',
        },
        description: {
          type: 'string',
          description: 'The new description for the bucket.',
        },
      },
      required: ['bucket_id', 'description'],
    },
  },
  {
    name: 'update_table_description',
    description: 'Update the description for a given Keboola table.',
    inputSchema: {
      type: 'object',
      properties: {
        table_id: {
          type: 'string',
          description: 'The ID of the table to update.',
        },
        description: {
          type: 'string',
          description: 'The new description for the table.',
        },
      },
      required: ['table_id', 'description'],
    },
  },
  {
    name: 'update_column_description',
    description: 'Update the description for a given column in a Keboola table.',
    inputSchema: {
      type: 'object',
      properties: {
        table_id: {
          type: 'string',
          description: 'The ID of the table that contains the column.',
        },
        column_name: {
          type: 'string',
          description: 'The name of the column to update.',
        },
        description: {
          type: 'string',
          description: 'The new description for the column.',
        },
      },
      required: ['table_id', 'column_name', 'description'],
    },
  },
  
  // SQL Tools
  {
    name: 'get_sql_dialect',
    description: 'Gets the name of the SQL dialect used by Keboola project\'s underlying database.',
    inputSchema: {
      type: 'object',
      properties: {},
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
  
  // Component Tools
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
  {
    name: 'get_component',
    description: 'Gets information about a specific component given its ID.',
    inputSchema: {
      type: 'object',
      properties: {
        component_id: {
          type: 'string',
          description: 'The ID of the component.',
        },
      },
      required: ['component_id'],
    },
  },
  {
    name: 'retrieve_component_configurations',
    description: 'Retrieves configurations of components present in the project.',
    inputSchema: {
      type: 'object',
      properties: {
        component_id: {
          type: 'string',
          description: 'Optional component ID to filter configurations.',
        },
      },
    },
  },
  {
    name: 'get_component_configuration',
    description: 'Gets information about a specific component/transformation configuration.',
    inputSchema: {
      type: 'object',
      properties: {
        component_id: {
          type: 'string',
          description: 'The ID of the component.',
        },
        configuration_id: {
          type: 'string',
          description: 'The ID of the configuration.',
        },
      },
      required: ['component_id', 'configuration_id'],
    },
  },
  {
    name: 'retrieve_transformations',
    description: 'Retrieves transformations configurations in the project.',
    inputSchema: {
      type: 'object',
      properties: {
        transformation_id: {
          type: 'string',
          description: 'Optional transformation ID to filter.',
        },
      },
    },
  },
  
  // Jobs Tools
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
    name: 'start_job',
    description: 'Starts a new job for a given component or transformation.',
    inputSchema: {
      type: 'object',
      properties: {
        component_id: {
          type: 'string',
          description: 'The ID of the component.',
        },
        configuration_id: {
          type: 'string',
          description: 'The ID of the configuration.',
        },
      },
      required: ['component_id', 'configuration_id'],
    },
  },
  
  // Documentation Tools
  {
    name: 'docs_query',
    description: 'Answers a question using the Keboola documentation as a source.',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Question to answer using Keboola documentation.',
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
        const componentIds = await keboolaMCP.findComponentId(args?.query as string);
        return {
          content: [
            {
              type: 'text',
              text: `Found component IDs: ${componentIds.join(', ')}`,
            },
          ],
        };

      case 'get_component':
        const component = await keboolaMCP.getComponent(args?.component_id as string);
        return {
          content: [
            {
              type: 'text',
              text: `Component: ${component.name}\nID: ${component.id}\nType: ${component.type}\nDescription: ${component.description || 'No description'}`,
            },
          ],
        };

      case 'retrieve_component_configurations':
        const configs = await keboolaMCP.retrieveComponentConfigurations(args?.component_id as string);
        return {
          content: [
            {
              type: 'text',
              text: `Found ${configs.length} configurations:\n\n${configs.map(config => 
                `• ${config.name} (${config.id})`
              ).join('\n')}`,
            },
          ],
        };

      case 'get_component_configuration':
        const config = await keboolaMCP.getComponentConfiguration(args?.component_id as string, args?.configuration_id as string);
        return {
          content: [
            {
              type: 'text',
              text: `Configuration: ${config.name}\nID: ${config.id}\nComponent: ${config.componentId}\nDescription: ${config.description || 'No description'}`,
            },
          ],
        };

      case 'retrieve_transformations':
        const transformations = await keboolaMCP.retrieveTransformations(args?.transformation_id as string);
        return {
          content: [
            {
              type: 'text',
              text: `Found ${transformations.length} transformations:\n\n${transformations.map(transform => 
                `• ${transform.name} (${transform.id})`
              ).join('\n')}`,
            },
          ],
        };

      case 'start_job':
        const jobResult = await keboolaMCP.startJob(args?.component_id as string, args?.configuration_id as string);
        return {
          content: [
            {
              type: 'text',
              text: `Job started successfully!\nJob ID: ${jobResult.id}\nStatus: ${jobResult.status}`,
            },
          ],
        };

      case 'update_bucket_description':
        await keboolaMCP.updateBucketDescription(args?.bucket_id as string, args?.description as string);
        return {
          content: [
            {
              type: 'text',
              text: `Bucket description updated successfully for: ${args?.bucket_id}`,
            },
          ],
        };

      case 'update_table_description':
        await keboolaMCP.updateTableDescription(args?.table_id as string, args?.description as string);
        return {
          content: [
            {
              type: 'text',
              text: `Table description updated successfully for: ${args?.table_id}`,
            },
          ],
        };

      case 'update_column_description':
        await keboolaMCP.updateColumnDescription(args?.table_id as string, args?.column_name as string, args?.description as string);
        return {
          content: [
            {
              type: 'text',
              text: `Column description updated successfully for: ${args?.column_name} in table ${args?.table_id}`,
            },
          ],
        };

      case 'docs_query':
        const docsResponse = await keboolaMCP.docsQuery(args?.query as string);
        return {
          content: [
            {
              type: 'text',
              text: docsResponse,
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
  const message = userMessage.toLowerCase();
  
  try {
    // Smart query for specific business data
    if (message.includes('kapwa') && (message.includes('order') || message.includes('data'))) {
      // First get the actual tables in the Kapwa Gardens bucket
      try {
        const kapwaTables = await keboolaMCP.retrieveBucketTables('out.c-squarespace-kapwa-gardens');
        
        if (kapwaTables && kapwaTables.length > 0) {
          // Find orders table
          const ordersTable = kapwaTables.find(table => 
            table.name.toLowerCase().includes('order') || 
            table.id.toLowerCase().includes('order')
          ) || kapwaTables[0];
          
          // Query the actual table with proper ID
          const kapwaQuery = `SELECT * FROM \`${ordersTable.id}\` LIMIT 20`;
          const kapwaData = await keboolaMCP.queryTable(kapwaQuery);
          
          return {
            content: `Here are the recent Kapwa Gardens orders from table "${ordersTable.name}":`,
            displays: [{
              type: "table",
              title: `Kapwa Gardens - ${ordersTable.name}`,
              content: kapwaData
            }]
          };
        } else {
          return {
            content: `No tables found in Kapwa Gardens bucket. Available buckets: c-squarespace-kapwa-gardens`,
            displays: []
          };
        }
      } catch (queryError) {
        console.error('Kapwa query error:', queryError);
        // Show what buckets are available
        const buckets = await keboolaMCP.retrieveBuckets();
        const kapwaBucket = buckets.find(b => b.name.includes('kapwa'));
        return {
          content: `Unable to access Kapwa Gardens data. Found bucket: ${kapwaBucket ? kapwaBucket.name : 'Not found'}`,
          displays: []
        };
      }
    }

    if (message.includes('undiscovered') && (message.includes('order') || message.includes('data'))) {
      const undiscoveredQuery = `SELECT * FROM \`OUT_FACT_ORDERS_3_UNDISCOVERED\` LIMIT 20`;
      const undiscoveredData = await keboolaMCP.queryTable(undiscoveredQuery);
      return {
        content: `Here are the recent Undiscovered orders:`,
        displays: [{
          type: "table",
          title: "Undiscovered Orders",
          content: undiscoveredData
        }]
      };
    }

    if (message.includes('balay') && (message.includes('order') || message.includes('data'))) {
      const balayQuery = `SELECT * FROM \`OUT_FACT_ORDERS_4_BALAY_KREATIVE\` LIMIT 20`;
      const balayData = await keboolaMCP.queryTable(balayQuery);
      return {
        content: `Here are the recent Balay Kreative orders:`,
        displays: [{
          type: "table",
          title: "Balay Kreative Orders", 
          content: balayData
        }]
      };
    }

    if (message.includes('form') && message.includes('data')) {
      const formQuery = `SELECT * FROM \`OUT_FORMS_TYPEFORM\` LIMIT 20`;
      const formData = await keboolaMCP.queryTable(formQuery);
      return {
        content: `Here's your Typeform data:`,
        displays: [{
          type: "table",
          title: "Form Data",
          content: formData
        }]
      };
    }

    if (message.includes('customer')) {
      // Try Kapwa customers first
      const customerQuery = `SELECT * FROM \`OUT_DIM_CUSTOMERS_2_KAPWA_GARDENS\` LIMIT 20`;
      const customerData = await keboolaMCP.queryTable(customerQuery);
      return {
        content: `Here are your Kapwa Gardens customers:`,
        displays: [{
          type: "table",
          title: "Kapwa Gardens Customers",
          content: customerData
        }]
      };
    }

    if (message.includes('bucket')) {
      const buckets = await keboolaMCP.retrieveBuckets();
      const response = keboolaMCP.generateDataResponse(buckets, userMessage, 'buckets');
      return {
        content: response.message,
        displays: response.displays
      };
    }
    
    // Default: show recent orders across all brands
    const recentOrdersQuery = `SELECT * FROM \`OUT_FACT_ORDERS_KAPWA_GARDENS\` LIMIT 10`;
    try {
      const recentData = await keboolaMCP.queryTable(recentOrdersQuery);
      return {
        content: `Here's a sample of your recent business data:`,
        displays: [{
          type: "table",
          title: "Recent Orders",
          content: recentData
        }]
      };
    } catch (error) {
      // Fallback to buckets if no data access
      const buckets = await keboolaMCP.retrieveBuckets();
      const response = keboolaMCP.generateDataResponse(buckets, userMessage, 'buckets');
      return {
        content: `Here's your Keboola project overview with ${buckets.length} buckets:`,
        displays: response.displays
      };
    }
    
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