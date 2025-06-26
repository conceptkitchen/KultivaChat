import https from 'https';

function makeAPIRequest(query, testName) {
    return new Promise((resolve, reject) => {
        const postData = JSON.stringify({ query });
        
        const options = {
            hostname: 'kultivate-chat-ck.replit.app',
            port: 443,
            path: '/api/v1/data/query',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': Buffer.byteLength(postData)
            },
            timeout: 10000
        };
        
        const req = https.request(options, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => {
                try {
                    const response = JSON.parse(data);
                    resolve({ status: res.statusCode, response, testName, query });
                } catch (e) {
                    resolve({ status: res.statusCode, error: 'Invalid JSON', data, testName, query });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject({ error: 'TIMEOUT', testName, query });
        });
        
        req.on('error', (e) => reject({ error: e.message, testName, query }));
        
        req.write(postData);
        req.end();
    });
}

async function testAllQueries() {
    console.log('🧪 Testing All API Query Types\n');
    
    const testQueries = [
        // Table discovery
        { query: 'show me tables', name: 'Table Discovery' },
        { query: 'list all tables', name: 'Table List' },
        { query: 'what tables are available', name: 'Available Tables' },
        
        // Business entity queries
        { query: 'show me data from Balay Kreative', name: 'Balay Kreative Query' },
        { query: 'Kapwa Gardens events', name: 'Kapwa Gardens Query' },
        { query: 'revenue from Balay Kreative events', name: 'Revenue Query' },
        
        // SQL queries
        { query: 'SELECT * FROM table_name LIMIT 10', name: 'Direct SQL' },
        { query: 'SELECT table_name FROM INFORMATION_SCHEMA.TABLES', name: 'Schema Query' },
        
        // General queries
        { query: 'hello', name: 'General Greeting' },
        { query: 'help me analyze my data', name: 'Analysis Request' }
    ];
    
    for (const test of testQueries) {
        try {
            console.log(`📋 Testing: ${test.name}`);
            console.log(`   Query: "${test.query}"`);
            
            const result = await makeAPIRequest(test.query, test.name);
            
            console.log(`   Status: ${result.status}`);
            console.log(`   Success: ${result.response?.success}`);
            
            if (result.response?.data && result.response.data.length > 0) {
                console.log(`   Data: ${result.response.data.length} items returned`);
            } else {
                console.log(`   Response: ${result.response?.response?.substring(0, 100)}...`);
            }
            
            if (result.response?.suggestion) {
                console.log(`   Suggestion: ${result.response.suggestion}`);
            }
            
            console.log('   ✅ SUCCESS\n');
            
        } catch (error) {
            console.log(`   ❌ FAILED: ${error.error}`);
            console.log(`   Query: "${error.query}"\n`);
        }
    }
    
    console.log('🎯 API Testing Complete');
}

testAllQueries().catch(console.error);