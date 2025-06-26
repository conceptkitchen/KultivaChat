import fetch from 'node-fetch';

async function testAPI() {
  const baseUrl = 'https://kultivate-chat-ck.replit.app';
  
  console.log('Testing API Documentation...\n');
  
  try {
    // Test 1: Health Check
    console.log('1. Testing Health Check...');
    const healthResponse = await fetch(`${baseUrl}/api/health`, { timeout: 10000 });
    const healthData = await healthResponse.json();
    console.log(`   Status: ${healthResponse.status}`);
    console.log(`   Response: ${JSON.stringify(healthData)}\n`);
    
    // Test 2: Tables Endpoint
    console.log('2. Testing Tables Discovery...');
    const tablesResponse = await fetch(`${baseUrl}/api/v1/data/tables`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({}),
      timeout: 10000
    });
    const tablesData = await tablesResponse.json();
    console.log(`   Status: ${tablesResponse.status}`);
    console.log(`   Tables found: ${tablesData.data ? tablesData.data.length : 0}`);
    console.log(`   Success: ${tablesData.success}\n`);
    
    // Test 3: SQL Endpoint
    console.log('3. Testing SQL Execution...');
    const sqlResponse = await fetch(`${baseUrl}/api/v1/data/sql`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ sql: "SELECT 1 as test_value" }),
      timeout: 10000
    });
    const sqlData = await sqlResponse.json();
    console.log(`   Status: ${sqlResponse.status}`);
    console.log(`   Rows returned: ${sqlData.rows_returned || 0}`);
    console.log(`   Success: ${sqlData.success}\n`);
    
    // Test 4: Intelligent Router
    console.log('4. Testing Intelligent Router...');
    const routerResponse = await fetch(`${baseUrl}/api/v1/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: "show me tables" }),
      timeout: 10000
    });
    const routerData = await routerResponse.json();
    console.log(`   Status: ${routerResponse.status}`);
    console.log(`   Route used: ${routerData.route_used}`);
    console.log(`   Success: ${routerData.success}\n`);
    
    console.log('Documentation Test Complete!');
    
  } catch (error) {
    console.error('Test failed:', error.message);
  }
}

testAPI();