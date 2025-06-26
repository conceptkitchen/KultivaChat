import https from 'https';

async function testRepeatedSQLCalls() {
    console.log('TESTING REPEATED SQL API CALLS - Reproducing curl failure pattern\n');
    
    const sql_query = `SELECT * FROM \`kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders\` LIMIT 5`;
    
    for (let i = 1; i <= 4; i++) {
        console.log(`=== Attempt ${i} ===`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(sql_query);
            const duration = Date.now() - startTime;
            
            console.log(`Status: ${result.status}`);
            console.log(`Duration: ${duration}ms`);
            
            if (result.success) {
                const data = result.response;
                console.log(`✅ SUCCESS - ${data.rows_returned || 0} rows returned`);
                
                if (data.data && data.data.length > 0) {
                    const fields = Object.keys(data.data[0]);
                    console.log(`Fields: ${fields.slice(0, 3).join(', ')}`);
                }
            } else {
                console.log(`❌ FAILED - ${result.response.error || 'Unknown error'}`);
                console.log(`Response body: ${JSON.stringify(result.response).substring(0, 200)}`);
            }
            
        } catch (error) {
            console.log(`💥 ERROR - ${error.message}`);
        }
        
        console.log('');
        
        // Wait 2 seconds between requests
        if (i < 4) {
            await new Promise(resolve => setTimeout(resolve, 2000));
        }
    }
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

testRepeatedSQLCalls().catch(console.error);