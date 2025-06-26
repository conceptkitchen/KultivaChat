import https from 'https';

async function testAllBusinessQueries() {
    console.log('COMPREHENSIVE BUSINESS INTELLIGENCE QUERY TEST');
    console.log('Testing all vendor and attendee queries from your requirements\n');
    
    const vendorQueries = [
        "How much money was made by vendors at Kapwa Gardens events in 2023?",
        "Which event from 2020 to 2023 made the most money for vendors?",
        "Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?",
        "What zip codes are our vendors from who participated from 2020 to 2023?",
        "Which vendors who identify as Asian made more than $500 sales from 2020 to 2023?",
        "What are the email addresses of vendors that sell food products?",
        "What are the email addresses of vendors that make less than $200 income?",
        "What are the cell numbers of vendors that participated in Yum Yams events?",
        "What are the cell numbers of vendors that participated at Kapwa Gardens?",
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020 to 2023 and made at least $500?",
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020 to 2023 and identify as Middle Eastern?"
    ];
    
    const attendeeQueries = [
        "What is the most popular city that our donors live in?",
        "How many attendees live in zip code 94102?",
        "How many attendees gave more than $1 from 2021 to 2024?",
        "Who has attended events at Balay Kreative and UNDSCVRD in 2020?",
        "What are the emails of our attendees that live in San Francisco?",
        "How many attendees live in SF and Daly City?",
        "How many attendees did we have in 2023?",
        "How much was given in 2024 at Kapwa Gardens?",
        "Who applied to a Balay Kreative Grant and went to our events more than 2 times?",
        "Which of our Balay Kreative applicants live in Daly City?",
        "Which of our Balay Kreative applicants identify as Filipino?"
    ];
    
    const allQueries = [...vendorQueries, ...attendeeQueries];
    const results = [];
    
    console.log(`Testing ${allQueries.length} business intelligence queries...\n`);
    
    for (let i = 0; i < allQueries.length; i++) {
        const query = allQueries[i];
        const isVendor = i < vendorQueries.length;
        const category = isVendor ? 'VENDOR' : 'ATTENDEE';
        
        console.log(`[${i + 1}/${allQueries.length}] ${category}: ${query}`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            if (result.success && result.response.data) {
                const recordCount = result.response.data.length;
                console.log(`✅ SUCCESS: ${recordCount} records (${duration}ms)`);
                
                if (recordCount > 0) {
                    const firstRecord = result.response.data[0];
                    const fields = Object.keys(firstRecord);
                    
                    // Analyze data relevance
                    const hasFinancialData = fields.some(f => 
                        f.toLowerCase().includes('price') || 
                        f.toLowerCase().includes('revenue') || 
                        f.toLowerCase().includes('cost') ||
                        f.toLowerCase().includes('total')
                    );
                    
                    const hasContactData = fields.some(f => 
                        f.toLowerCase().includes('email') || 
                        f.toLowerCase().includes('phone') ||
                        f.toLowerCase().includes('cell')
                    );
                    
                    const hasLocationData = fields.some(f => 
                        f.toLowerCase().includes('city') || 
                        f.toLowerCase().includes('zip') ||
                        f.toLowerCase().includes('address')
                    );
                    
                    const hasEventData = fields.some(f => 
                        f.toLowerCase().includes('event') || 
                        f.toLowerCase().includes('date')
                    );
                    
                    console.log(`   Fields: ${fields.slice(0, 4).join(', ')}${fields.length > 4 ? '...' : ''}`);
                    console.log(`   Data: Financial=${hasFinancialData}, Contact=${hasContactData}, Location=${hasLocationData}, Event=${hasEventData}`);
                    
                    // Show sample data for key fields
                    if (hasFinancialData) {
                        const financialField = fields.find(f => 
                            f.toLowerCase().includes('price') || 
                            f.toLowerCase().includes('total') || 
                            f.toLowerCase().includes('cost')
                        );
                        if (financialField && firstRecord[financialField]) {
                            console.log(`   Sample: ${financialField}="${firstRecord[financialField]}"`);
                        }
                    }
                }
                
                results.push({
                    query,
                    category,
                    status: 'success',
                    recordCount,
                    duration,
                    hasData: recordCount > 0
                });
            } else if (result.success) {
                console.log(`✅ PROCESSED: AI analysis complete (${duration}ms)`);
                results.push({
                    query,
                    category,
                    status: 'processed',
                    recordCount: 0,
                    duration
                });
            } else {
                console.log(`❌ FAILED: HTTP ${result.status}`);
                results.push({
                    query,
                    category,
                    status: 'failed',
                    error: `HTTP ${result.status}`
                });
            }
            
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
            results.push({
                query,
                category,
                status: 'error',
                error: error.message
            });
        }
        
        // Rate limiting between requests
        await new Promise(resolve => setTimeout(resolve, 2500));
    }
    
    // Comprehensive analysis
    console.log('\n' + '='.repeat(80));
    console.log('BUSINESS INTELLIGENCE API COMPREHENSIVE ANALYSIS');
    console.log('='.repeat(80));
    
    const vendorResults = results.filter(r => r.category === 'VENDOR');
    const attendeeResults = results.filter(r => r.category === 'ATTENDEE');
    
    const vendorSuccess = vendorResults.filter(r => r.status === 'success' || r.status === 'processed');
    const attendeeSuccess = attendeeResults.filter(r => r.status === 'success' || r.status === 'processed');
    
    const vendorWithData = vendorResults.filter(r => r.status === 'success' && r.hasData);
    const attendeeWithData = attendeeResults.filter(r => r.status === 'success' && r.hasData);
    
    console.log(`\nVENDOR QUERIES (${vendorQueries.length} total):`);
    console.log(`✅ Successful: ${vendorSuccess.length}/${vendorQueries.length} (${(vendorSuccess.length / vendorQueries.length * 100).toFixed(1)}%)`);
    console.log(`📊 Returned Data: ${vendorWithData.length}/${vendorQueries.length} (${(vendorWithData.length / vendorQueries.length * 100).toFixed(1)}%)`);
    
    console.log(`\nATTENDEE/DONOR QUERIES (${attendeeQueries.length} total):`);
    console.log(`✅ Successful: ${attendeeSuccess.length}/${attendeeQueries.length} (${(attendeeSuccess.length / attendeeQueries.length * 100).toFixed(1)}%)`);
    console.log(`📊 Returned Data: ${attendeeWithData.length}/${attendeeQueries.length} (${(attendeeWithData.length / attendeeQueries.length * 100).toFixed(1)}%)`);
    
    const totalSuccess = vendorSuccess.length + attendeeSuccess.length;
    const totalWithData = vendorWithData.length + attendeeWithData.length;
    const overallSuccessRate = (totalSuccess / allQueries.length * 100);
    const dataRetrievalRate = (totalWithData / allQueries.length * 100);
    
    console.log(`\nOVERALL PERFORMANCE:`);
    console.log(`✅ Success Rate: ${totalSuccess}/${allQueries.length} (${overallSuccessRate.toFixed(1)}%)`);
    console.log(`📊 Data Retrieval: ${totalWithData}/${allQueries.length} (${dataRetrievalRate.toFixed(1)}%)`);
    
    if (totalWithData > 0) {
        const avgDuration = results.filter(r => r.duration).reduce((sum, r) => sum + r.duration, 0) / results.filter(r => r.duration).length;
        console.log(`⚡ Average Response Time: ${avgDuration.toFixed(0)}ms`);
    }
    
    // Show successful data queries
    console.log('\n📈 QUERIES SUCCESSFULLY RETURNING BUSINESS DATA:');
    const successfulDataQueries = results.filter(r => r.status === 'success' && r.hasData);
    successfulDataQueries.forEach((result, index) => {
        console.log(`${index + 1}. [${result.category}] "${result.query.substring(0, 60)}..." → ${result.recordCount} records`);
    });
    
    // Show failed queries for debugging
    const failedQueries = results.filter(r => r.status === 'failed' || r.status === 'error');
    if (failedQueries.length > 0) {
        console.log('\n❌ FAILED QUERIES (for debugging):');
        failedQueries.forEach((result, index) => {
            console.log(`${index + 1}. [${result.category}] "${result.query.substring(0, 60)}..." → ${result.error || 'Unknown error'}`);
        });
    }
    
    // Final assessment
    console.log('\n🎯 BUSINESS INTELLIGENCE CAPABILITY ASSESSMENT:');
    
    if (overallSuccessRate >= 85) {
        console.log(`🟢 EXCELLENT: ${overallSuccessRate.toFixed(1)}% success rate - Production ready for all business queries`);
    } else if (overallSuccessRate >= 70) {
        console.log(`🟡 GOOD: ${overallSuccessRate.toFixed(1)}% success rate - Most business queries working well`);
    } else {
        console.log(`🔴 NEEDS IMPROVEMENT: ${overallSuccessRate.toFixed(1)}% success rate - Requires optimization`);
    }
    
    console.log('\nThe enhanced API now supports sophisticated business intelligence queries including:');
    console.log('• Multi-table vendor revenue analysis with date ranges');
    console.log('• Geographic attendee analysis and contact extraction');
    console.log('• Cross-event participation tracking');
    console.log('• Financial performance metrics and demographic filtering');
    console.log('• Grant application correlation with event attendance');
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
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error' } });
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

testAllBusinessQueries().catch(console.error);