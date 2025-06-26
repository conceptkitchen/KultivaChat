import http from 'http';

function testDirectBackend(query, testName) {
    return new Promise((resolve, reject) => {
        const postData = JSON.stringify({ query });
        
        const options = {
            hostname: 'localhost',
            port: 8081,
            path: '/api/v1/data/query',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': Buffer.byteLength(postData)
            },
            timeout: 5000
        };
        
        const req = http.request(options, (res) => {
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

async function testDirectAPI() {
    console.log('🔗 Testing Direct Backend API (Port 8081)');
    
    const tests = [
        { query: 'show me tables', name: 'Table Discovery' },
        { query: 'Balay Kreative events', name: 'Business Query' },
        { query: 'SELECT * FROM table LIMIT 5', name: 'SQL Query' }
    ];
    
    for (const test of tests) {
        try {
            console.log(`\n📋 ${test.name}: "${test.query}"`);
            
            const result = await testDirectBackend(test.query, test.name);
            
            console.log(`   Status: ${result.status}`);
            console.log(`   Success: ${result.response?.success}`);
            
            if (result.response?.data && result.response.data.length > 0) {
                console.log(`   Tables Found: ${result.response.data.length}`);
                console.log(`   First Table: ${result.response.data[0]?.table_name}`);
            } else {
                console.log(`   Response: ${result.response?.response?.substring(0, 80)}...`);
            }
            
            console.log('   ✅ SUCCESS');
            
        } catch (error) {
            console.log(`   ❌ FAILED: ${error.error}`);
        }
    }
}

testDirectAPI().catch(console.error);