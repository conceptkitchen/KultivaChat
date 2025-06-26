#!/usr/bin/env node

// API Documentation Testing Script
// Tests all endpoints documented in SIMPLIFIED_API_DOCUMENTATION_1750898536216.md

const API_BASE = 'https://kultivate-chat-ck.replit.app';

class APIDocumentationTester {
  constructor() {
    this.results = [];
    this.passed = 0;
    this.total = 0;
  }

  log(message) {
    console.log(`${new Date().toISOString()} - ${message}`);
  }

  addResult(testName, passed, details) {
    this.results.push({ testName, passed, details });
    this.total++;
    if (passed) this.passed++;
    
    const status = passed ? '✅ PASS' : '❌ FAIL';
    this.log(`${status}: ${testName}`);
    if (details) this.log(`   Details: ${details}`);
  }

  async makeRequest(url, options = {}) {
    try {
      const response = await fetch(url, {
        timeout: 30000,
        ...options
      });
      
      const contentType = response.headers.get('content-type');
      let data = null;
      
      if (contentType && contentType.includes('application/json')) {
        data = await response.json();
      } else {
        data = await response.text();
      }
      
      return {
        status: response.status,
        statusText: response.statusText,
        data,
        headers: Object.fromEntries(response.headers.entries())
      };
    } catch (error) {
      return {
        status: 0,
        statusText: 'Network Error',
        data: null,
        error: error.message
      };
    }
  }

  async testHealthEndpoint() {
    this.log('Testing Health Check Endpoint...');
    
    const response = await this.makeRequest(`${API_BASE}/api/health`);
    
    if (response.status === 200) {
      const expectedFields = ['backend', 'status'];
      const hasRequiredFields = expectedFields.every(field => 
        response.data && typeof response.data === 'object' && field in response.data
      );
      
      if (hasRequiredFields) {
        this.addResult('Health Check Endpoint', true, 
          `Status: ${response.status}, Backend: ${response.data.backend}, Status: ${response.data.status}`);
      } else {
        this.addResult('Health Check Endpoint', false, 
          `Missing required fields. Got: ${JSON.stringify(response.data)}`);
      }
    } else {
      this.addResult('Health Check Endpoint', false, 
        `Expected 200, got ${response.status}: ${response.statusText}`);
    }
  }

  async testTablesEndpoint() {
    this.log('Testing Tables Discovery Endpoint...');
    
    const response = await this.makeRequest(`${API_BASE}/api/v1/data/tables`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({})
    });
    
    if (response.status === 200) {
      const data = response.data;
      const hasRequiredFields = data && 
        typeof data.success === 'boolean' && 
        Array.isArray(data.data) &&
        typeof data.timestamp === 'string';
      
      if (hasRequiredFields && data.success) {
        this.addResult('Tables Discovery Endpoint', true, 
          `Found ${data.data.length} tables, Success: ${data.success}`);
      } else {
        this.addResult('Tables Discovery Endpoint', false, 
          `Invalid response structure: ${JSON.stringify(data)}`);
      }
    } else {
      this.addResult('Tables Discovery Endpoint', false, 
        `Expected 200, got ${response.status}: ${response.statusText || response.error}`);
    }
  }

  async testSQLEndpoint() {
    this.log('Testing Direct SQL Endpoint...');
    
    const testSQL = "SELECT 1 as test_column, 'hello' as test_string";
    
    const response = await this.makeRequest(`${API_BASE}/api/v1/data/sql`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ sql: testSQL })
    });
    
    if (response.status === 200) {
      const data = response.data;
      const hasRequiredFields = data && 
        typeof data.success === 'boolean' && 
        Array.isArray(data.data) &&
        typeof data.rows_returned === 'number' &&
        'error' in data &&
        typeof data.timestamp === 'string';
      
      if (hasRequiredFields && data.success && data.rows_returned > 0) {
        this.addResult('Direct SQL Endpoint', true, 
          `Query executed successfully, returned ${data.rows_returned} rows`);
      } else {
        this.addResult('Direct SQL Endpoint', false, 
          `Invalid response or query failed: ${JSON.stringify(data)}`);
      }
    } else {
      this.addResult('Direct SQL Endpoint', false, 
        `Expected 200, got ${response.status}: ${response.statusText || response.error}`);
    }
  }

  async testIntelligentQueryRouter() {
    this.log('Testing Intelligent Query Router...');
    
    // Test table discovery routing
    const tableResponse = await this.makeRequest(`${API_BASE}/api/v1/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: "show me tables" })
    });
    
    if (tableResponse.status === 200) {
      const data = tableResponse.data;
      const isValidTableResponse = data && 
        typeof data.success === 'boolean' && 
        typeof data.route_used === 'string' &&
        data.route_used === 'tables';
      
      if (isValidTableResponse) {
        this.addResult('Intelligent Router - Table Discovery', true, 
          `Correctly routed to: ${data.route_used}`);
      } else {
        this.addResult('Intelligent Router - Table Discovery', false, 
          `Incorrect routing or response: ${JSON.stringify(data)}`);
      }
    } else {
      this.addResult('Intelligent Router - Table Discovery', false, 
        `Expected 200, got ${tableResponse.status}: ${tableResponse.statusText || tableResponse.error}`);
    }

    // Test SQL routing
    const sqlResponse = await this.makeRequest(`${API_BASE}/api/v1/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: "SELECT 1 as test" })
    });
    
    if (sqlResponse.status === 200) {
      const data = sqlResponse.data;
      const isValidSQLResponse = data && 
        typeof data.success === 'boolean' && 
        typeof data.route_used === 'string' &&
        data.route_used === 'sql';
      
      if (isValidSQLResponse) {
        this.addResult('Intelligent Router - SQL Query', true, 
          `Correctly routed to: ${data.route_used}`);
      } else {
        this.addResult('Intelligent Router - SQL Query', false, 
          `Incorrect routing or response: ${JSON.stringify(data)}`);
      }
    } else {
      this.addResult('Intelligent Router - SQL Query', false, 
        `Expected 200, got ${sqlResponse.status}: ${sqlResponse.statusText || sqlResponse.error}`);
    }
  }

  async testErrorHandling() {
    this.log('Testing Error Handling...');
    
    // Test invalid SQL
    const response = await this.makeRequest(`${API_BASE}/api/v1/data/sql`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ sql: "INVALID SQL QUERY" })
    });
    
    // Should return error with proper structure
    if (response.status >= 400 || (response.status === 200 && response.data && !response.data.success)) {
      this.addResult('Error Handling', true, 
        `Properly handled invalid SQL with status ${response.status}`);
    } else {
      this.addResult('Error Handling', false, 
        `Did not properly handle invalid SQL: ${JSON.stringify(response.data)}`);
    }
  }

  async testResponseFormats() {
    this.log('Testing Response Format Consistency...');
    
    const response = await this.makeRequest(`${API_BASE}/api/v1/data/tables`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({})
    });
    
    if (response.status === 200) {
      const data = response.data;
      
      // Check timestamp format (ISO 8601)
      const timestampValid = data.timestamp && 
        /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/.test(data.timestamp);
      
      // Check required fields
      const requiredFields = ['success', 'data', 'timestamp'];
      const hasAllFields = requiredFields.every(field => field in data);
      
      if (timestampValid && hasAllFields) {
        this.addResult('Response Format Consistency', true, 
          `All required fields present with correct formats`);
      } else {
        this.addResult('Response Format Consistency', false, 
          `Missing fields or invalid formats. Timestamp valid: ${timestampValid}, Has all fields: ${hasAllFields}`);
      }
    } else {
      this.addResult('Response Format Consistency', false, 
        `Could not test response format due to API error: ${response.status}`);
    }
  }

  printSummary() {
    console.log('\n' + '='.repeat(50));
    console.log('API DOCUMENTATION TEST SUMMARY');
    console.log('='.repeat(50));
    console.log(`Total Tests: ${this.total}`);
    console.log(`Passed: ${this.passed}`);
    console.log(`Failed: ${this.total - this.passed}`);
    console.log(`Success Rate: ${((this.passed / this.total) * 100).toFixed(1)}%`);
    
    if (this.passed === this.total) {
      console.log('\n🎉 ALL TESTS PASSED');
      console.log('Documentation is accurate and API is fully operational');
    } else if (this.passed >= this.total * 0.8) {
      console.log('\n✅ MOSTLY OPERATIONAL');
      console.log('Minor issues found but core functionality works');
    } else {
      console.log('\n⚠️ SIGNIFICANT ISSUES FOUND');
      console.log('Documentation may need updates or API has problems');
    }
    
    console.log('\nFailed Tests:');
    this.results.filter(r => !r.passed).forEach(result => {
      console.log(`❌ ${result.testName}: ${result.details}`);
    });
  }

  async runAllTests() {
    this.log('Starting API Documentation Validation...');
    
    await this.testHealthEndpoint();
    await this.testTablesEndpoint();
    await this.testSQLEndpoint();
    await this.testIntelligentQueryRouter();
    await this.testErrorHandling();
    await this.testResponseFormats();
    
    this.printSummary();
  }
}

// Run the tests
const tester = new APIDocumentationTester();
tester.runAllTests().catch(error => {
  console.error('Test execution failed:', error);
  process.exit(1);
});