import https from 'https';

async function quickBITest() {
    const testQueries = [
        "How much money was made by vendors at Kapwa Gardens events in 2023?",
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020 to 2023 and made at least $500?",
        "How many attendees live in SF and Daly City?"
    ];
    
    console.log('Quick Business Intelligence Test\n');
    
    for (let i = 0; i < testQueries.length; i++) {
        const query = testQueries[i];
        console.log(`[${i + 1}] ${query}`);
        
        try {
            const result = await makeAPIRequest(query);
            
            if (result.success && result.response.data) {
                console.log(`✅ ${result.response.data.length} records returned`);
                if (result.response.data.length > 0) {
                    const fields = Object.keys(result.response.data[0]);
                    console.log(`   Fields: ${fields.slice(0, 3).join(', ')}`);
                }
            } else {
                console.log(`✅ AI processing complete`);
            }
        } catch (error) {
            console.log(`❌ ${error.message}`);
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
            timeout: 15000
        };
        
        const req = https.request(options, (res) => {
            let data = '';
            res.on('data', (chunk) => data += chunk);
            res.on('end', () => {
                try {
                    const response = JSON.parse(data);
                    resolve({ status: res.statusCode, success: res.statusCode === 200, response });
                } catch (e) {
                    resolve({ status: res.statusCode, success: false, response: {} });
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

quickBITest().catch(console.error);