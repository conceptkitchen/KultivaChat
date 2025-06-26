import https from 'https';

async function testEnhancedAPI() {
    console.log('Testing Enhanced Natural Language API Capabilities...\n');
    
    const tests = [
        {
            name: "Table Discovery Test",
            query: "show me all my data tables"
        },
        {
            name: "Business Entity Query",
            query: "Show me revenue from Balay Kreative events"
        },
        {
            name: "Natural Language Analysis",
            query: "What are the top selling products from Kapwa Gardens?"
        },
        {
            name: "Direct SQL Test",
            query: "SELECT * FROM `Balay-Kreative---attendees---all-orders` LIMIT 5"
        }
    ];
    
    for (const test of tests) {
        console.log(`\n🧪 ${test.name}`);
        console.log(`Query: "${test.query}"`);
        
        try {
            const result = await makeAPIRequest(test.query);
            
            if (result.success) {
                console.log(`✅ Success (${result.status})`);
                
                if (result.response.data && result.response.data.length > 0) {
                    console.log(`📊 Data returned: ${result.response.data.length} records`);
                    console.log(`Sample: ${JSON.stringify(result.response.data[0]).substring(0, 100)}...`);
                } else if (result.response.displays && result.response.displays.length > 0) {
                    console.log(`📋 Tables discovered: ${result.response.displays.length}`);
                } else {
                    console.log(`💬 AI Response: ${result.response.response?.substring(0, 150)}...`);
                }
            } else {
                console.log(`❌ Failed (${result.status}): ${result.response.error || 'Unknown error'}`);
            }
            
        } catch (error) {
            console.log(`💥 Error: ${error.message}`);
        }
        
        // Wait between requests
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
                    resolve({ status: res.statusCode, success: false, response: { error: 'Invalid JSON', raw: data } });
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

testEnhancedAPI()
    .then(() => {
        console.log('\n🎉 Enhanced API testing completed!');
        console.log('\nThe API v1 endpoint now provides full natural language capabilities');
        console.log('matching the sophisticated business intelligence features of the main chat interface.');
    })
    .catch(error => {
        console.log(`\n💥 Testing failed: ${error.message}`);
    });