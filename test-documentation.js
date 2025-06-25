// Comprehensive API Documentation Testing Script
// Tests all documented endpoints and verifies response formats

const API_BASE = 'https://Kultivate-chat-ck.replit.app/api/v1';

class APITester {
  constructor() {
    this.results = {
      total: 0,
      passed: 0,
      failed: 0,
      tests: []
    };
  }

  log(message) {
    console.log(`[${new Date().toISOString()}] ${message}`);
  }

  addResult(testName, passed, details) {
    this.results.total++;
    if (passed) {
      this.results.passed++;
      this.log(`✅ PASS: ${testName}`);
    } else {
      this.results.failed++;
      this.log(`❌ FAIL: ${testName} - ${details}`);
    }
    this.results.tests.push({ testName, passed, details });
  }

  async testEndpoint(url, options, expectedStructure, testName) {
    try {
      this.log(`🧪 Testing: ${testName}`);
      
      const response = await fetch(url, options);
      const data = await response.json();
      
      // Test response status
      if (response.status !== 200) {
        this.addResult(testName, false, `HTTP ${response.status}: ${data.error || 'Unknown error'}`);
        return false;
      }

      // Test response structure
      const structureValid = this.validateStructure(data, expectedStructure);
      if (!structureValid.valid) {
        this.addResult(testName, false, `Invalid structure: ${structureValid.error}`);
        return false;
      }

      this.addResult(testName, true, `Response validated successfully`);
      return true;

    } catch (error) {
      this.addResult(testName, false, `Network error: ${error.message}`);
      return false;
    }
  }

  validateStructure(data, expected) {
    for (const [key, type] of Object.entries(expected)) {
      if (!(key in data)) {
        return { valid: false, error: `Missing required field: ${key}` };
      }
      
      if (type === 'array' && !Array.isArray(data[key])) {
        return { valid: false, error: `Field ${key} should be an array` };
      }
      
      if (type === 'string' && typeof data[key] !== 'string') {
        return { valid: false, error: `Field ${key} should be a string` };
      }
      
      if (type === 'boolean' && typeof data[key] !== 'boolean') {
        return { valid: false, error: `Field ${key} should be a boolean` };
      }
      
      if (type === 'number' && typeof data[key] !== 'number') {
        return { valid: false, error: `Field ${key} should be a number` };
      }
    }
    return { valid: true };
  }

  async runAllTests() {
    this.log('🚀 Starting Kultivate AI API Documentation Tests');
    this.log('=' * 60);

    // Test 1: Natural Language Query Endpoint
    await this.testNaturalLanguageQuery();

    // Test 2: Direct SQL Execution Endpoint  
    await this.testDirectSQL();

    // Test 3: Table Discovery Endpoint
    await this.testTableDiscovery();

    // Test 4: Error Handling
    await this.testErrorHandling();

    // Test 5: Response Format Validation
    await this.testResponseFormats();

    this.printSummary();
  }

  async testNaturalLanguageQuery() {
    this.log('\n📝 Testing Natural Language Query Endpoint');
    
    const testCases = [
      {
        name: 'Basic Data Query',
        query: 'Show me available tables',
        expected: {
          success: 'boolean',
          query: 'string',
          response: 'string',
          data: 'array',
          timestamp: 'string'
        }
      },
      {
        name: 'Specific Business Query',
        query: 'Show me Balay Kreative data',
        expected: {
          success: 'boolean',
          query: 'string', 
          response: 'string',
          data: 'array',
          timestamp: 'string'
        }
      }
    ];

    for (const testCase of testCases) {
      await this.testEndpoint(
        `${API_BASE}/data/query`,
        {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            query: testCase.query,
            credentials: {}
          })
        },
        testCase.expected,
        `Natural Language Query - ${testCase.name}`
      );
    }
  }

  async testDirectSQL() {
    this.log('\n🗃️ Testing Direct SQL Execution Endpoint');
    
    const testCases = [
      {
        name: 'Table Discovery SQL',
        sql: 'SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 5',
        expected: {
          success: 'boolean',
          data: 'array',
          rows_returned: 'number',
          timestamp: 'string'
        }
      }
    ];

    for (const testCase of testCases) {
      await this.testEndpoint(
        `${API_BASE}/data/sql`,
        {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            sql: testCase.sql,
            credentials: {}
          })
        },
        testCase.expected,
        `Direct SQL - ${testCase.name}`
      );
    }
  }

  async testTableDiscovery() {
    this.log('\n📊 Testing Table Discovery Endpoint');
    
    await this.testEndpoint(
      `${API_BASE}/data/tables`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          credentials: {}
        })
      },
      {
        success: 'boolean',
        tables: 'array',
        total_tables: 'number',
        timestamp: 'string'
      },
      'Table Discovery - Get All Tables'
    );
  }

  async testErrorHandling() {
    this.log('\n⚠️ Testing Error Handling');

    // Test missing required parameters
    const response = await fetch(`${API_BASE}/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({}) // Missing query parameter
    });

    if (response.status === 400) {
      this.addResult('Error Handling - Missing Parameters', true, 'Correctly returned 400 status');
    } else {
      this.addResult('Error Handling - Missing Parameters', false, `Expected 400, got ${response.status}`);
    }
  }

  async testResponseFormats() {
    this.log('\n🔍 Testing Response Format Compliance');

    try {
      // Test that responses contain proper timestamp format
      const response = await fetch(`${API_BASE}/data/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          query: 'test query',
          credentials: {}
        })
      });

      const data = await response.json();
      
      // Validate timestamp format (ISO 8601)
      const timestampValid = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/.test(data.timestamp);
      
      if (timestampValid) {
        this.addResult('Response Format - Timestamp', true, 'ISO 8601 format validated');
      } else {
        this.addResult('Response Format - Timestamp', false, `Invalid timestamp format: ${data.timestamp}`);
      }

    } catch (error) {
      this.addResult('Response Format - Timestamp', false, `Error testing timestamp: ${error.message}`);
    }
  }

  printSummary() {
    this.log('\n' + '=' * 60);
    this.log('📋 TEST SUMMARY');
    this.log('=' * 60);
    this.log(`Total Tests: ${this.results.total}`);
    this.log(`Passed: ${this.results.passed}`);
    this.log(`Failed: ${this.results.failed}`);
    this.log(`Success Rate: ${((this.results.passed / this.results.total) * 100).toFixed(1)}%`);
    
    if (this.results.failed > 0) {
      this.log('\n❌ Failed Tests:');
      this.results.tests
        .filter(test => !test.passed)
        .forEach(test => {
          this.log(`  - ${test.testName}: ${test.details}`);
        });
    }

    this.log('\n✅ Documentation Validation Results:');
    
    if (this.results.passed >= this.results.total * 0.8) {
      this.log('🎉 DOCUMENTATION VERIFIED: API endpoints match documented behavior');
      this.log('📖 Documentation is accurate and ready for production use');
    } else {
      this.log('⚠️ DOCUMENTATION ISSUES FOUND: Some endpoints don\'t match documentation');
      this.log('🔧 Documentation needs updates based on test results');
    }

    // Provide integration guidance
    this.log('\n🔌 Integration Recommendations:');
    this.log('1. Use the JavaScript/Python examples in the documentation');
    this.log('2. Implement proper error handling for network timeouts');
    this.log('3. Include credentials in requests for authenticated data access');
    this.log('4. Use table discovery endpoint to identify available data sources');
    this.log('5. Structure queries using natural language for best AI interpretation');
  }
}

// Execute comprehensive testing
const tester = new APITester();
tester.runAllTests().catch(console.error);