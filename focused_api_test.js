import https from 'https';

async function focusedAPITest() {
    console.log('FOCUSED API TEST - Essential Functionality Check\n');
    
    const tests = [
        {
            query: "show me tables",
            type: "simple",
            expectData: true
        },
        {
            query: "show me 3 records from Balay Kreative",
            type: "simple", 
            expectData: true
        },
        {
            query: "How much money was made by vendors at Kapwa Gardens events in 2023?",
            type: "complex",
            expectData: true
        }
    ];
    
    let passed = 0;
    let total = tests.length;
    
    for (let i = 0; i < tests.length; i++) {
        const test = tests[i];
        console.log(`[${i + 1}/${total}] ${test.query}`);
        console.log(`Expected: ${test.type} query`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(test.query, test.type === 'complex' ? 30000 : 15000);
            const duration = Date.now() - startTime;
            
            if (result.success) {
                const data = result.response;
                console.log(`✅ SUCCESS (${duration}ms)`);
                
                // Check data
                let hasData = false;
                let dataCount = 0;
                
                if (data.data && Array.isArray(data.data) && data.data.length > 0) {
                    hasData = true;
                    dataCount = data.data.length;
                } else if (data.rows_returned > 0) {
                    hasData = true;
                    dataCount = data.rows_returned;
                }
                
                console.log(`   Data: ${dataCount} records`);
                
                if (hasData && test.expectData) {
                    console.log(`   ✅ Data validation: PASSED`);
                    passed++;
                    
                    // Show field names for verification
                    if (data.data && data.data[0]) {
                        const fields = Object.keys(data.data[0]);
                        console.log(`   Fields: ${fields.slice(0, 3).join(', ')}`);
                    }
                } else if (!test.expectData) {
                    console.log(`   ✅ No data expected: PASSED`);
                    passed++;
                } else {
                    console.log(`   ❌ Data validation: FAILED (no data returned)`);
                }
                
                // Check progress indicators
                if (data.is_complex_query !== undefined) {
                    console.log(`   Progress indicators: Present`);
                    console.log(`   Complexity: ${data.is_complex_query ? 'Complex' : 'Simple'}`);
                } else {
                    console.log(`   Progress indicators: Missing`);
                }
                
            } else {
                console.log(`❌ FAILED: Status ${result.status}`);
                console.log(`   Error: ${result.response.error || 'Unknown'}`);
            }
            
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
        }
        
        console.log('');
        
        // Wait between tests
        if (i < tests.length - 1) {
            await new Promise(resolve => setTimeout(resolve, 3000));
        }
    }
    
    console.log('='.repeat(50));
    console.log(`FINAL RESULT: ${passed}/${total} tests passed`);
    
    if (passed === total) {
        console.log('🎯 API STATUS: FULLY OPERATIONAL');
        console.log('✅ Simple queries working');
        console.log('✅ Complex business intelligence working');
        console.log('✅ Data extraction working');
        console.log('✅ Ready for production use');
    } else if (passed >= total * 0.7) {
        console.log('⚠️ API STATUS: MOSTLY WORKING');
        console.log('Some functionality may need optimization');
    } else {
        console.log('❌ API STATUS: NEEDS ATTENTION');
        console.log('Multiple issues detected');
    }
}

function makeAPIRequest(query, timeout = 15000) {
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
            timeout: timeout
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
            reject(new Error(`Timeout after ${timeout/1000}s`));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

focusedAPITest().catch(console.error);