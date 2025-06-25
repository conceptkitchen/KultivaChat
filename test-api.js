// Test script for Kultivate AI API endpoints
// Usage: node test-api.js

const API_BASE = 'https://Kultivate-chat-ck.replit.app/api/v1';

async function testAPI() {
  console.log('🧪 Testing Kultivate AI API endpoints...\n');

  // Test 1: Natural Language Query
  console.log('1️⃣ Testing Natural Language Query API');
  try {
    const response = await fetch(`${API_BASE}/data/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query: "Show me Balay Kreative customer data",
        credentials: {
          // Add your credentials here if needed
        }
      })
    });
    
    const result = await response.json();
    console.log('✅ Query API Status:', response.status);
    console.log('📊 Response Preview:', JSON.stringify(result, null, 2).substring(0, 300) + '...');
    
    if (result.data && result.data.length > 0) {
      console.log(`📈 Data returned: ${result.data.length} display objects`);
    }
  } catch (error) {
    console.log('❌ Query API Error:', error.message);
  }

  console.log('\n' + '='.repeat(50) + '\n');

  // Test 2: Direct SQL Execution
  console.log('2️⃣ Testing Direct SQL API');
  try {
    const response = await fetch(`${API_BASE}/data/sql`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sql: "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` LIMIT 5",
        credentials: {
          // Add your credentials here if needed
        }
      })
    });
    
    const result = await response.json();
    console.log('✅ SQL API Status:', response.status);
    console.log('📊 Response Preview:', JSON.stringify(result, null, 2).substring(0, 300) + '...');
    
    if (result.data && result.data.length > 0) {
      console.log(`📈 Tables found: ${result.data.length} rows`);
    }
  } catch (error) {
    console.log('❌ SQL API Error:', error.message);
  }

  console.log('\n' + '='.repeat(50) + '\n');

  // Test 3: Table Discovery
  console.log('3️⃣ Testing Table Discovery API');
  try {
    const response = await fetch(`${API_BASE}/data/tables`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        credentials: {
          // Add your credentials here if needed
        }
      })
    });
    
    const result = await response.json();
    console.log('✅ Tables API Status:', response.status);
    console.log('📊 Response Preview:', JSON.stringify(result, null, 2).substring(0, 300) + '...');
    
    if (result.tables && result.tables.length > 0) {
      console.log(`📈 Table displays: ${result.tables.length} found`);
    }
  } catch (error) {
    console.log('❌ Tables API Error:', error.message);
  }

  console.log('\n🎉 API testing complete!');
  console.log('\n📝 Usage Examples for Your Other Product:');
  console.log(`
// Natural Language Query
const result = await fetch('${API_BASE}/data/query', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    query: "Show me recent sales data",
    credentials: { KBC_STORAGE_TOKEN: 'your-token' }
  })
});

// Direct SQL Execution  
const sqlResult = await fetch('${API_BASE}/data/sql', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    sql: "SELECT * FROM your_table LIMIT 10",
    credentials: { GOOGLE_APPLICATION_CREDENTIALS: 'path-to-service-account.json' }
  })
});
  `);
}

// Run the test
testAPI().catch(console.error);