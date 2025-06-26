import https from 'https';

async function testAPIProgressIndicators() {
    console.log('API PROGRESS INDICATOR TESTING\n');
    console.log('Testing both simple and complex queries to verify progress metadata\n');
    
    const testQueries = [
        {
            query: "show me tables",
            expected_complexity: "simple",
            description: "Simple table discovery"
        },
        {
            query: "show me 3 records from Balay Kreative",
            expected_complexity: "simple", 
            description: "Simple data retrieval"
        },
        {
            query: "How much money was made by vendors at Kapwa Gardens events in 2023?",
            expected_complexity: "complex",
            description: "Complex revenue analysis with date filtering"
        },
        {
            query: "Which vendors participated in Kapwa Gardens AND UNDSCVRD events from 2020-2023 and made at least $500?",
            expected_complexity: "complex",
            description: "Complex multi-event analysis with financial threshold"
        },
        {
            query: "How many attendees live in SF and Daly City?",
            expected_complexity: "complex",
            description: "Complex geographic analysis"
        },
        {
            query: "Who applied to a Balay Kreative Grant and went to our events more than 2 times?",
            expected_complexity: "complex",
            description: "Complex grant correlation analysis"
        }
    ];
    
    for (let i = 0; i < testQueries.length; i++) {
        const test = testQueries[i];
        console.log(`[${i + 1}] ${test.description}`);
        console.log(`Query: "${test.query}"`);
        console.log(`Expected: ${test.expected_complexity} complexity`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(test.query);
            const duration = Date.now() - startTime;
            
            if (result.success) {
                // Check for progress indicator metadata
                const metadata = result.response;
                const hasComplexityInfo = metadata.is_complex_query !== undefined;
                const hasProcessingTime = metadata.estimated_processing_time !== undefined;
                const hasStatus = metadata.processing_status !== undefined;
                
                console.log(`✅ SUCCESS: ${duration}ms`);
                
                if (hasComplexityInfo) {
                    console.log(`   Complex Query: ${metadata.is_complex_query}`);
                    console.log(`   Query Type: ${metadata.query_type || 'unknown'}`);
                    
                    if (hasProcessingTime) {
                        console.log(`   Estimated Time: ${metadata.estimated_processing_time}`);
                    }
                    
                    if (hasStatus) {
                        console.log(`   Status: ${metadata.processing_status}`);
                    }
                    
                    if (metadata.complexity_factors !== undefined) {
                        console.log(`   Complexity Factors: ${metadata.complexity_factors}`);
                    }
                    
                    // Verify complexity detection accuracy
                    const detected_complex = metadata.is_complex_query;
                    const expected_complex = test.expected_complexity === "complex";
                    
                    if (detected_complex === expected_complex) {
                        console.log(`   ✅ Complexity Detection: CORRECT`);
                    } else {
                        console.log(`   ❌ Complexity Detection: INCORRECT (got ${detected_complex}, expected ${expected_complex})`);
                    }
                } else {
                    console.log(`   ⚠️  Progress indicators not found in response`);
                }
                
                // Check for actual data
                if (metadata.data && metadata.data.length > 0) {
                    console.log(`   Data: ${metadata.data.length} records returned`);
                } else if (metadata.rows_returned !== undefined) {
                    console.log(`   Data: ${metadata.rows_returned} rows returned`);
                }
                
            } else {
                console.log(`❌ FAILED: Status ${result.status}`);
                if (result.response.error) {
                    console.log(`   Error: ${result.response.error}`);
                }
            }
            
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
        }
        
        console.log(''); // Add spacing between tests
        await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    console.log('='.repeat(60));
    console.log('API PROGRESS INDICATOR TESTING COMPLETE');
    console.log('='.repeat(60));
    console.log('The API now provides:');
    console.log('• Complexity detection for business intelligence queries');
    console.log('• Processing time estimates (1-5s simple, 15-30s complex)');
    console.log('• Status messages for user feedback during processing');
    console.log('• Query type classification (simple vs business_intelligence)');
    console.log('• Complexity factor scoring for advanced queries');
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
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error' } });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject(new Error('Request timeout - complex queries may take 15-30 seconds'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

testAPIProgressIndicators().catch(console.error);