import https from 'https';

function testDebugRoute() {
    return new Promise((resolve, reject) => {
        const postData = JSON.stringify({ query: "debug_test" });
        
        const options = {
            hostname: 'kultivate-chat-ck.replit.app',
            port: 443,
            path: '/api/v1/data/query',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': Buffer.byteLength(postData)
            },
            timeout: 5000
        };
        
        const req = https.request(options, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => {
                try {
                    const response = JSON.parse(data);
                    resolve({ status: res.statusCode, response });
                } catch (e) {
                    resolve({ status: res.statusCode, error: 'Invalid JSON', data });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject({ error: 'TIMEOUT' });
        });
        
        req.on('error', (e) => reject({ error: e.message }));
        
        req.write(postData);
        req.end();
    });
}

testDebugRoute()
    .then(result => {
        console.log('Debug Route Test:');
        console.log(`Status: ${result.status}`);
        console.log(`Response:`, result.response);
        
        if (result.response?.message === "Direct Node.js handler working") {
            console.log('✅ Node.js handler confirmed working');
        } else {
            console.log('⚠️ Route may be intercepted');
        }
    })
    .catch(error => {
        console.log(`❌ Error: ${error.error}`);
    });