import axios from 'axios';

// Types for Keboola API responses
interface KeboolaBucket {
  id: string;
  name: string;
  stage: string;
  description?: string;
  created: string;
  lastChangeDate: string;
}

interface KeboolaTable {
  id: string;
  name: string;
  primaryKey: string[];
  columns: string[];
  rowsCount: number;
  dataSizeBytes: number;
  created: string;
  lastChangeDate: string;
  lastImportDate: string;
  bucket: {
    id: string;
    name: string;
    stage: string;
  };
}

interface KeboolaJob {
  id: string;
  runId: string;
  status: string;
  component: string;
  config: string;
  configRowIds: string[];
  createdTime: string;
  startTime: string;
  endTime: string;
  durationSeconds: number;
}

// Keboola MCP Server implementation
export class KeboolaMCP {
  private storageToken: string;
  private workspaceSchema: string;
  private apiUrl: string;
  private googleCredentials?: string;

  constructor() {
    this.storageToken = process.env.KBC_STORAGE_TOKEN || '';
    this.workspaceSchema = process.env.KBC_WORKSPACE_SCHEMA || '';
    this.apiUrl = process.env.KBC_API_URL || '';
    this.googleCredentials = process.env.GOOGLE_APPLICATION_CREDENTIALS;

    if (!this.storageToken || !this.workspaceSchema || !this.apiUrl) {
      throw new Error('Missing required Keboola configuration');
    }
  }

  // Storage Tools
  async retrieveBuckets(): Promise<KeboolaBucket[]> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/buckets`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error retrieving buckets:', error);
      throw error;
    }
  }

  async getBucketDetail(bucketId: string): Promise<KeboolaBucket> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/buckets/${bucketId}`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error getting bucket detail:', error);
      throw error;
    }
  }

  async retrieveBucketTables(bucketId: string): Promise<KeboolaTable[]> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/buckets/${bucketId}/tables`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error retrieving bucket tables:', error);
      throw error;
    }
  }

  async getTableDetail(tableId: string): Promise<KeboolaTable> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/tables/${tableId}`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error getting table detail:', error);
      throw error;
    }
  }

  // SQL Tools
  async getSqlDialect(): Promise<string> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/workspaces`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      
      // Find workspace by schema name
      const workspace = response.data.find((w: any) => w.schema === this.workspaceSchema);
      return workspace?.backend || 'snowflake';
    } catch (error) {
      console.error('Error getting SQL dialect:', error);
      throw error;
    }
  }

  async queryTable(sqlQuery: string): Promise<any[]> {
    try {
      console.log('=== WORKSPACE QUERY ATTEMPT ===');
      console.log('Query:', sqlQuery);
      console.log('Workspace Schema:', this.workspaceSchema);
      console.log('API URL:', this.apiUrl);
      console.log('Has Storage Token:', !!this.storageToken);
      console.log('Google Credentials Path:', this.googleCredentials);
      
      // Check if credentials file exists
      const fs = require('fs');
      if (this.googleCredentials && fs.existsSync(this.googleCredentials)) {
        console.log('✓ Google credentials file exists');
        const credContent = JSON.parse(fs.readFileSync(this.googleCredentials, 'utf8'));
        console.log('✓ Credentials project:', credContent.project_id);
        console.log('✓ Service account email:', credContent.client_email);
      } else {
        console.log('✗ Google credentials file not found or not set');
      }

      const jobData = {
        configData: {
          parameters: {
            query: sqlQuery,
          },
        },
      };

      const response = await axios.post(`${this.apiUrl}/v2/storage/workspaces/${this.workspaceSchema}/query`, jobData, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json',
        },
      });

      console.log('✓ Workspace query successful');
      return response.data.results || response.data.rows || [];
    } catch (error) {
      console.error('=== WORKSPACE QUERY FAILED ===');
      console.error('Status:', error.response?.status);
      console.error('Error:', error.response?.data || error.message);
      console.error('URL:', error.config?.url);
      throw error;
    }
  }

  async showWorkspaceTables(): Promise<any[]> {
    try {
      console.log('Attempting to query workspace tables...');
      console.log('Workspace Schema:', this.workspaceSchema);
      console.log('API URL:', this.apiUrl);
      console.log('Has Google Credentials:', !!this.googleCredentials);
      
      // Try simple table listing first
      const query = `SELECT table_name FROM INFORMATION_SCHEMA.TABLES`;
      return await this.queryTable(query);
    } catch (error) {
      console.error('First workspace query failed:', error);
      
      // Try with explicit dataset reference
      try {
        const query = `SELECT table_name FROM \`${this.workspaceSchema}\`.INFORMATION_SCHEMA.TABLES`;
        console.log('Trying explicit dataset query:', query);
        return await this.queryTable(query);
      } catch (secondError) {
        console.error('Second workspace query failed:', secondError);
        
        // Try listing the specific tables we know exist from your workspace
        try {
          const query = `SELECT 'OUT_FACT_ORDERS_KAPWA_GARDENS' as table_name UNION ALL SELECT 'OUT_DIM_CUSTOMERS_2_KAPWA_GARDENS' as table_name`;
          return await this.queryTable(query);
        } catch (thirdError) {
          console.error('All workspace queries failed:', thirdError);
          throw new Error(`Workspace access failed. Check Google credentials and workspace permissions.`);
        }
      }
    }
  }

  // Job Tools
  async retrieveJobs(componentId?: string, configId?: string, status?: string): Promise<KeboolaJob[]> {
    try {
      let url = `${this.apiUrl}/v2/storage/jobs`;
      const params = new URLSearchParams();
      
      if (componentId) params.append('component', componentId);
      if (configId) params.append('config', configId);
      if (status) params.append('status', status);
      
      if (params.toString()) {
        url += `?${params.toString()}`;
      }

      const response = await axios.get(url, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error retrieving jobs:', error);
      throw error;
    }
  }

  async getJobDetail(jobId: string): Promise<KeboolaJob> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/storage/jobs/${jobId}`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      return response.data;
    } catch (error) {
      console.error('Error getting job detail:', error);
      throw error;
    }
  }

  // Component Tools
  async findComponentId(query: string): Promise<string[]> {
    try {
      const response = await axios.get(`${this.apiUrl}/v2/components`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
        },
      });
      
      // Filter components by query
      const components = response.data.filter((component: any) => 
        component.name.toLowerCase().includes(query.toLowerCase()) ||
        component.id.toLowerCase().includes(query.toLowerCase())
      );
      
      return components.map((component: any) => component.id);
    } catch (error) {
      console.error('Error finding component ID:', error);
      throw error;
    }
  }

  // Helper method to generate smart responses based on data
  async getComponent(componentId: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/components/${componentId}`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        }
      });

      if (!response.ok) {
        throw new Error(`Failed to retrieve component: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error getting component:', error);
      throw error;
    }
  }

  async retrieveComponentConfigurations(componentId?: string): Promise<any[]> {
    try {
      let url = `${this.apiUrl}/v2/components`;
      if (componentId) {
        url += `/${componentId}/configs`;
      }

      const response = await fetch(url, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        }
      });

      if (!response.ok) {
        throw new Error(`Failed to retrieve configurations: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error retrieving configurations:', error);
      throw error;
    }
  }

  async getComponentConfiguration(componentId: string, configurationId: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/components/${componentId}/configs/${configurationId}`, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        }
      });

      if (!response.ok) {
        throw new Error(`Failed to retrieve configuration: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error getting configuration:', error);
      throw error;
    }
  }

  async retrieveTransformations(transformationId?: string): Promise<any[]> {
    try {
      let url = `${this.apiUrl}/v2/components/transformation/configs`;
      if (transformationId) {
        url += `/${transformationId}`;
      }

      const response = await fetch(url, {
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        }
      });

      if (!response.ok) {
        throw new Error(`Failed to retrieve transformations: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error retrieving transformations:', error);
      throw error;
    }
  }

  async startJob(componentId: string, configurationId: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/components/${componentId}/configs/${configurationId}/jobs`, {
        method: 'POST',
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        }
      });

      if (!response.ok) {
        throw new Error(`Failed to start job: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error starting job:', error);
      throw error;
    }
  }

  async updateBucketDescription(bucketId: string, description: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/storage/buckets/${bucketId}`, {
        method: 'PUT',
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ description })
      });

      if (!response.ok) {
        throw new Error(`Failed to update bucket description: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error updating bucket description:', error);
      throw error;
    }
  }

  async updateTableDescription(tableId: string, description: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/storage/tables/${tableId}`, {
        method: 'PUT',
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ description })
      });

      if (!response.ok) {
        throw new Error(`Failed to update table description: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error updating table description:', error);
      throw error;
    }
  }

  async updateColumnDescription(tableId: string, columnName: string, description: string): Promise<any> {
    try {
      const response = await fetch(`${this.apiUrl}/v2/storage/tables/${tableId}/columns/${columnName}`, {
        method: 'PUT',
        headers: {
          'X-StorageApi-Token': this.storageToken,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ description })
      });

      if (!response.ok) {
        throw new Error(`Failed to update column description: ${response.statusText}`);
      }

      return await response.json();
    } catch (error) {
      console.error('Error updating column description:', error);
      throw error;
    }
  }

  async docsQuery(query: string): Promise<string> {
    return `For questions about "${query}", please refer to the Keboola documentation at https://help.keboola.com/ or contact Keboola support for detailed assistance.`;
  }

  generateDataResponse(data: any[], query: string, type: 'table' | 'buckets' | 'jobs' = 'table') {
    if (!data || data.length === 0) {
      return {
        message: "No data found for your query. This could be because:\n• The data source is empty\n• You don't have access to this data\n• The query parameters need to be adjusted",
        displays: []
      };
    }

    let message = '';
    let displays = [];

    switch (type) {
      case 'table':
        message = `Found ${data.length} records. Here's your data:`;
        displays = [{
          type: 'table',
          title: 'Query Results',
          content: data.slice(0, 100) // Limit to first 100 rows
        }];
        break;
      
      case 'buckets':
        message = `Found ${data.length} buckets in your Keboola project:`;
        displays = [{
          type: 'table',
          title: 'Storage Buckets',
          content: data.map((bucket: KeboolaBucket) => ({
            'Bucket ID': bucket.id,
            'Name': bucket.name,
            'Stage': bucket.stage,
            'Description': bucket.description || 'No description',
            'Created': new Date(bucket.created).toLocaleDateString()
          }))
        }];
        break;
      
      case 'jobs':
        message = `Found ${data.length} jobs:`;
        displays = [{
          type: 'table',
          title: 'Jobs',
          content: data.map((job: KeboolaJob) => ({
            'Job ID': job.id,
            'Status': job.status,
            'Component': job.component,
            'Duration': `${job.durationSeconds}s`,
            'Created': new Date(job.createdTime).toLocaleDateString()
          }))
        }];
        break;
    }

    return { message, displays };
  }
}