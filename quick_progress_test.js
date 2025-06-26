import https from 'https';

async function quickProgressTest() {
    console.log('QUICK PROGRESS INDICATOR TEST\n');
    
    const testQueries = [
        {
            query: "show me tables",
            type: "simple"
        },
        {
            query: "How much money was made by vendors at Kapwa Gardens events in 2023?",
            type: "complex"
        }
    ];
    
    for (const test of testQueries) {
        console.log(`Testing: ${test.query}`);
        console.log(`Expected: ${test.type} query`);
        
        try {
            const result = await makeAPIRequest(test.query);
            
            if (result.success) {
                const data = result.response;
                console.log(`✅ Success`);
                
                // Check for progress indicators
                if (data.is_complex_query !== undefined) {
                    console.log(`   Complex: ${data.is_complex_query}`);
                    console.log(`   Type: ${data.query_type}`);
                    console.log(`   Time: ${data.estimated_processing_time}`);
                    console.log(`   Status: ${data.processing_status}`);
                } else {
                    console.log(`   ⚠️ No progress indicators found`);
                }
                
                if (data.data && data.data.length > 0) {
                    console.log(`   Data: ${data.data.length} records`);
                }
            } else {
                console.log(`❌ Failed: ${result.status}`);
            }
        } catch (error) {
            console.log(`💥 Error: ${error.message}`);
        }
        
        console.log('');
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

quickProgressTest().catch(console.error);