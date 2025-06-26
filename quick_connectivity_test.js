import https from 'https';

async function quickConnectivityTest() {
    console.log('QUICK CONNECTIVITY TEST\n');
    
    // Test 1: Health check
    console.log('1. Testing health endpoint...');
    try {
        const healthResult = await makeRequest('/api/health', 'GET');
        console.log(`   Health: ${healthResult.success ? 'OK' : 'FAILED'}`);
    } catch (e) {
        console.log(`   Health: ERROR - ${e.message}`);
    }
    
    // Test 2: Simple table query
    console.log('2. Testing table discovery...');
    try {
        const tableResult = await makeRequest('/api/v1/data/query', 'POST', { query: 'show me tables' });
        console.log(`   Tables: ${tableResult.success ? 'OK' : 'FAILED'}`);
        if (tableResult.success && tableResult.response.data) {
            console.log(`   Found: ${tableResult.response.data.length} tables`);
        }
    } catch (e) {
        console.log(`   Tables: ERROR - ${e.message}`);
    }
    
    // Test 3: Direct SQL endpoint
    console.log('3. Testing SQL endpoint...');
    const sql = `SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\` LIMIT 3`;
    try {
        const sqlResult = await makeRequest('/api/v1/data/sql', 'POST', { sql });
        console.log(`   SQL: ${sqlResult.success ? 'OK' : 'FAILED'}`);
        if (sqlResult.success && sqlResult.response.data) {
            console.log(`   Returned: ${sqlResult.response.data.length} rows`);
        }
    } catch (e) {
        console.log(`   SQL: ERROR - ${e.message}`);
    }
    
    console.log('\nConnectivity test complete.');
}

function makeRequest(path, method = 'GET', body = null) {
    return new Promise((resolve, reject) => {
        const postData = body ? JSON.stringify(body) : null;
        
        const options = {
            hostname: 'kultivate-chat-ck.replit.app',
            port: 443,
            path: path,
            method: method,
            headers: {
                'Content-Type': 'application/json',
                ...(postData && { 'Content-Length': Buffer.byteLength(postData) })
            },
            timeout: 10000
        };
        
        const req = https.request(options, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => {
                try {
                    const response = res.headers['content-type']?.includes('application/json') 
                        ? JSON.parse(data) 
                        : { message: data };
                    resolve({ status: res.statusCode, success: res.statusCode === 200, response });
                } catch (e) {
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error', raw: data } });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject(new Error('Timeout'));
        });
        
        req.on('error', (e) => reject(e));
        
        if (postData) {
            req.write(postData);
        }
        req.end();
    });
}

quickConnectivityTest().catch(console.error);