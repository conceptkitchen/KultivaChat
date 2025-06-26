import https from 'https';

async function testCurlFix() {
    console.log('TESTING CURL FIX - Reproducing your exact command\n');
    
    const sql_query = `SELECT * FROM \`kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders\` LIMIT 5`;
    
    console.log('Testing your exact curl command multiple times...\n');
    
    for (let i = 1; i <= 3; i++) {
        console.log(`=== Attempt ${i} ===`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(sql_query);
            const duration = Date.now() - startTime;
            
            if (result.success) {
                const data = result.response;
                console.log(`✅ SUCCESS (${duration}ms)`);
                console.log(`   Rows returned: ${data.rows_returned || 0}`);
                
                if (data.data && data.data.length > 0) {
                    const fields = Object.keys(data.data[0]);
                    console.log(`   Sample fields: ${fields.slice(0, 4).join(', ')}`);
                }
            } else {
                console.log(`❌ FAILED: Status ${result.status}`);
                console.log(`   Error: ${result.response.error || 'Unknown error'}`);
            }
            
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
        }
        
        console.log('');
        
        // Wait 2 seconds between requests
        if (i < 3) {
            await new Promise(resolve => setTimeout(resolve, 2000));
        }
    }
    
    console.log('CURL FIX TEST COMPLETE');
}

function makeAPIRequest(sql) {
    return new Promise((resolve, reject) => {
        const postData = JSON.stringify({ sql });
        
        const options = {
            hostname: 'kultivate-chat-ck.replit.app',
            port: 443,
            path: '/api/v1/data/sql',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': Buffer.byteLength(postData)
            },
            timeout: 30000
        };
        
        const req = https.request(options, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => {
                try {
                    const response = JSON.parse(data);
                    resolve({ status: res.statusCode, success: res.statusCode === 200, response });
                } catch (e) {
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error', raw: data } });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject(new Error('Request timeout'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

testCurlFix().catch(console.error);