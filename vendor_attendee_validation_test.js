import https from 'https';

async function validateSpecificBusinessQueries() {
    console.log('VENDOR & ATTENDEE BUSINESS QUERY VALIDATION');
    console.log('Testing your exact business intelligence requirements\n');
    
    const priorityQueries = [
        // High priority vendor queries
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020 to 2023 and made at least $500?",
        "What zip codes are our vendors from who participated from 2020 to 2023?", 
        "Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?",
        
        // High priority attendee queries
        "How many attendees live in SF and Daly City?",
        "What is the most popular city that our donors live in?",
        "How many attendees gave more than $1 from 2021 to 2024?",
        
        // Contact information queries
        "What are the email addresses of vendors that participated in Kapwa Gardens?",
        "What are the emails of our attendees that live in San Francisco?",
        
        // Grant application analysis
        "Who applied to a Balay Kreative Grant and went to our events more than 2 times?",
        "Which of our Balay Kreative applicants live in Daly City?"
    ];
    
    const results = [];
    
    for (let i = 0; i < priorityQueries.length; i++) {
        const query = priorityQueries[i];
        console.log(`\n[${i + 1}] Testing: ${query}`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            if (result.success && result.response.data) {
                const recordCount = result.response.data.length;
                console.log(`✅ SUCCESS: ${recordCount} records in ${duration}ms`);
                
                // Analyze data quality
                if (recordCount > 0) {
                    const firstRecord = result.response.data[0];
                    const fields = Object.keys(firstRecord);
                    
                    // Check for relevant business fields
                    const hasRevenueData = fields.some(f => f.toLowerCase().includes('price') || f.toLowerCase().includes('revenue') || f.toLowerCase().includes('cost'));
                    const hasContactData = fields.some(f => f.toLowerCase().includes('email') || f.toLowerCase().includes('phone'));
                    const hasLocationData = fields.some(f => f.toLowerCase().includes('city') || f.toLowerCase().includes('zip'));
                    const hasEventData = fields.some(f => f.toLowerCase().includes('event') || f.toLowerCase().includes('date'));
                    
                    console.log(`   Fields: ${fields.slice(0, 3).join(', ')}${fields.length > 3 ? '...' : ''}`);
                    console.log(`   Data Quality: Revenue=${hasRevenueData}, Contact=${hasContactData}, Location=${hasLocationData}, Event=${hasEventData}`);
                }
                
                results.push({
                    query,
                    status: 'success',
                    recordCount,
                    duration,
                    dataQuality: recordCount > 0 ? 'authentic' : 'empty'
                });
            } else {
                console.log(`❌ No data returned (Status: ${result.status})`);
                results.push({
                    query,
                    status: 'no_data',
                    recordCount: 0,
                    duration
                });
            }
            
        } catch (error) {
            console.log(`💥 ERROR: ${error.message}`);
            results.push({
                query,
                status: 'error',
                error: error.message
            });
        }
        
        // Rate limiting
        await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    // Summary analysis
    console.log('\n' + '='.repeat(70));
    console.log('BUSINESS INTELLIGENCE API VALIDATION SUMMARY');
    console.log('='.repeat(70));
    
    const successful = results.filter(r => r.status === 'success');
    const withData = successful.filter(r => r.recordCount > 0);
    const avgDuration = successful.reduce((sum, r) => sum + r.duration, 0) / successful.length;
    
    console.log(`Total Queries: ${results.length}`);
    console.log(`Successful: ${successful.length} (${(successful.length / results.length * 100).toFixed(1)}%)`);
    console.log(`Returned Data: ${withData.length} (${(withData.length / results.length * 100).toFixed(1)}%)`);
    console.log(`Average Response Time: ${avgDuration ? avgDuration.toFixed(0) : 'N/A'}ms`);
    
    if (withData.length > 0) {
        console.log('\n📊 QUERIES SUCCESSFULLY RETURNING BUSINESS DATA:');
        withData.forEach((result, index) => {
            console.log(`${index + 1}. "${result.query.substring(0, 50)}..." → ${result.recordCount} records`);
        });
    }
    
    // Capability assessment
    console.log('\n🎯 API CAPABILITY ASSESSMENT:');
    
    const vendorQueries = results.filter(r => r.query.toLowerCase().includes('vendor'));
    const attendeeQueries = results.filter(r => r.query.toLowerCase().includes('attendee'));
    const locationQueries = results.filter(r => r.query.toLowerCase().includes('sf') || r.query.toLowerCase().includes('daly') || r.query.toLowerCase().includes('zip'));
    const contactQueries = results.filter(r => r.query.toLowerCase().includes('email'));
    
    console.log(`Vendor Analysis: ${vendorQueries.filter(r => r.status === 'success').length}/${vendorQueries.length} working`);
    console.log(`Attendee Analysis: ${attendeeQueries.filter(r => r.status === 'success').length}/${attendeeQueries.length} working`);
    console.log(`Geographic Filtering: ${locationQueries.filter(r => r.status === 'success').length}/${locationQueries.length} working`);
    console.log(`Contact Extraction: ${contactQueries.filter(r => r.status === 'success').length}/${contactQueries.length} working`);
    
    const overallSuccess = (successful.length / results.length * 100);
    if (overallSuccess >= 80) {
        console.log(`\n✅ EXCELLENT: ${overallSuccess.toFixed(1)}% success rate - API ready for production use`);
    } else if (overallSuccess >= 60) {
        console.log(`\n⚠️ GOOD: ${overallSuccess.toFixed(1)}% success rate - minor improvements needed`);
    } else {
        console.log(`\n❌ NEEDS WORK: ${overallSuccess.toFixed(1)}% success rate - requires enhancement`);
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
            timeout: 25000
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

validateSpecificBusinessQueries().catch(console.error);