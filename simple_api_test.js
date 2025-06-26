const https = require('https');

function testAPI() {
    console.log('Testing API fix...');
    
    const postData = JSON.stringify({
        query: 'show me tables'
    });
    
    const options = {
        hostname: 'kultivate-chat-ck.replit.app',
        port: 443,
        path: '/api/v1/data/query',
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Content-Length': Buffer.byteLength(postData)
        },
        timeout: 5000  // 5 second timeout
    };
    
    const req = https.request(options, (res) => {
        console.log(`Status: ${res.statusCode}`);
        
        let data = '';
        res.on('data', (chunk) => {
            data += chunk;
        });
        
        res.on('end', () => {
            try {
                const response = JSON.parse(data);
                console.log('API Response:', JSON.stringify(response, null, 2));
            } catch (e) {
                console.log('Raw response:', data);
            }
        });
    });
    
    req.on('timeout', () => {
        console.log('REQUEST TIMED OUT - API still spinning');
        req.destroy();
    });
    
    req.on('error', (e) => {
        console.error('Request error:', e.message);
    });
    
    req.write(postData);
    req.end();
}

testAPI();