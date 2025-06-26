import https from 'https';

async function testSimpleQueries() {
    console.log('SIMPLE API TESTS - Quick Response Validation\n');
    
    const simpleQueries = [
        "show me tables",
        "show me 5 records from Balay Kreative attendees",
        "get customer data"
    ];
    
    for (let i = 0; i < simpleQueries.length; i++) {
        const query = simpleQueries[i];
        console.log(`[${i + 1}] ${query}`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            if (result.success) {
                if (result.response.data && result.response.data.length > 0) {
                    console.log(`✅ SUCCESS: ${result.response.data.length} records (${duration}ms)`);
                    const fields = Object.keys(result.response.data[0]);
                    console.log(`   Fields: ${fields.slice(0, 4).join(', ')}`);
                } else {
                    console.log(`✅ PROCESSED: ${duration}ms`);
                }
            } else {
                console.log(`❌ FAILED: ${result.status}`);
            }
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
        }
        
        await new Promise(resolve => setTimeout(resolve, 1000));
    }
}

function makeAPIRequest(query) {
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
                    resolve({ status: res.statusCode, success: res.statusCode === 200, response });
                } catch (e) {
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error' } });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject(new Error('Timeout'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

testSimpleQueries().catch(console.error);