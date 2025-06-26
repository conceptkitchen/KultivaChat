import https from 'https';

async function comprehensiveAPIRetest() {
    console.log('COMPREHENSIVE API RETEST - Simple + Your Business Questions\n');
    
    const testSuite = [
        {
            category: "Simple API Tests",
            queries: [
                "show me tables",
                "show me 5 records from Balay Kreative attendees",
                "get customer data"
            ]
        },
        {
            category: "Your Business Intelligence Questions",
            queries: [
                "How much money was made by vendors at Kapwa Gardens events in 2023?",
                "Which vendors participated in Kapwa Gardens AND UNDSCVRD events from 2020-2023 and made at least $500?",
                "How many attendees live in SF and Daly City?",
                "What are the email addresses of vendors that participated at Kapwa Gardens?",
                "How many attendees gave more than $1 from 2021 to 2024?",
                "Who applied to a Balay Kreative Grant and went to our events more than 2 times?"
            ]
        }
    ];
    
    const results = {
        simple_passed: 0,
        simple_total: 0,
        business_passed: 0,
        business_total: 0,
        errors: []
    };
    
    for (const category of testSuite) {
        console.log(`\n=== ${category.category} ===`);
        
        for (let i = 0; i < category.queries.length; i++) {
            const query = category.queries[i];
            const isSimple = category.category.includes("Simple");
            
            if (isSimple) {
                results.simple_total++;
            } else {
                results.business_total++;
            }
            
            console.log(`\n[${i + 1}] Testing: ${query}`);
            
            try {
                const startTime = Date.now();
                const result = await makeAPIRequest(query);
                const duration = Date.now() - startTime;
                
                if (result.success) {
                    const data = result.response;
                    console.log(`✅ SUCCESS (${duration}ms)`);
                    
                    // Check progress indicators
                    if (data.is_complex_query !== undefined) {
                        console.log(`   Complexity: ${data.is_complex_query ? 'Complex' : 'Simple'}`);
                        console.log(`   Type: ${data.query_type}`);
                        console.log(`   Est. Time: ${data.estimated_processing_time}`);
                        console.log(`   Status: ${data.processing_status}`);
                    }
                    
                    // Check data
                    let dataCount = 0;
                    if (data.data && Array.isArray(data.data)) {
                        dataCount = data.data.length;
                    } else if (data.rows_returned !== undefined) {
                        dataCount = data.rows_returned;
                    }
                    
                    console.log(`   Data: ${dataCount} records`);
                    
                    // Show sample fields for verification
                    if (data.data && data.data.length > 0) {
                        const fields = Object.keys(data.data[0]);
                        console.log(`   Fields: ${fields.slice(0, 4).join(', ')}`);
                        
                        // Show sample data for business verification
                        if (!isSimple && fields.length > 0) {
                            const sample = {};
                            fields.slice(0, 2).forEach(field => {
                                const value = data.data[0][field];
                                if (value !== null && value !== '') {
                                    sample[field] = value;
                                }
                            });
                            console.log(`   Sample: ${JSON.stringify(sample)}`);
                        }
                    }
                    
                    // Mark as passed
                    if (isSimple) {
                        results.simple_passed++;
                    } else {
                        results.business_passed++;
                    }
                    
                } else {
                    console.log(`❌ FAILED: Status ${result.status}`);
                    const error = `${query}: ${result.response.error || 'Unknown error'}`;
                    results.errors.push(error);
                    console.log(`   Error: ${result.response.error || 'Unknown error'}`);
                }
                
            } catch (error) {
                console.log(`💥 ERROR: ${error.message}`);
                const errorMsg = `${query}: ${error.message}`;
                results.errors.push(errorMsg);
            }
            
            // Wait between requests
            await new Promise(resolve => setTimeout(resolve, 2000));
        }
    }
    
    // Final Results
    console.log('\n' + '='.repeat(60));
    console.log('COMPREHENSIVE API TEST RESULTS');
    console.log('='.repeat(60));
    console.log(`Simple API Tests: ${results.simple_passed}/${results.simple_total} passed`);
    console.log(`Business Intelligence: ${results.business_passed}/${results.business_total} passed`);
    console.log(`Total Success Rate: ${results.simple_passed + results.business_passed}/${results.simple_total + results.business_total}`);
    
    if (results.errors.length > 0) {
        console.log('\nErrors encountered:');
        results.errors.forEach((error, i) => {
            console.log(`${i + 1}. ${error}`);
        });
    }
    
    // API Status Summary
    console.log('\nAPI STATUS SUMMARY:');
    if (results.simple_passed === results.simple_total) {
        console.log('✅ Simple queries: FULLY OPERATIONAL');
    } else {
        console.log('⚠️ Simple queries: PARTIAL FUNCTIONALITY');
    }
    
    if (results.business_passed === results.business_total) {
        console.log('✅ Business intelligence: FULLY OPERATIONAL');
    } else if (results.business_passed > 0) {
        console.log('⚠️ Business intelligence: PARTIAL FUNCTIONALITY');
    } else {
        console.log('❌ Business intelligence: NOT OPERATIONAL');
    }
    
    const overallPass = (results.simple_passed + results.business_passed) / (results.simple_total + results.business_total);
    if (overallPass >= 0.8) {
        console.log('🎯 OVERALL: API READY FOR PRODUCTION USE');
    } else if (overallPass >= 0.5) {
        console.log('⚠️ OVERALL: API NEEDS OPTIMIZATION');
    } else {
        console.log('❌ OVERALL: API REQUIRES FIXES');
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
            timeout: 25000  // Extended timeout for complex queries
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
            reject(new Error('Timeout - complex business queries may take 15-30 seconds'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

comprehensiveAPIRetest().catch(console.error);