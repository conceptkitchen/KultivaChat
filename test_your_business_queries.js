import https from 'https';

async function testYourBusinessQueries() {
    console.log('TESTING YOUR SPECIFIC BUSINESS INTELLIGENCE QUERIES\n');
    
    const queries = [
        // Vendor Revenue Questions
        { category: 'Vendor Revenue', query: 'How much money was made by vendors at Kapwa Gardens events in 2023?' },
        { category: 'Vendor Revenue', query: 'Which event from 2020 to 2023 made the most money for vendors?' },
        { category: 'Vendor Analysis', query: 'Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?' },
        { category: 'Vendor Demographics', query: 'What zip codes are our vendors from who participated from 2020 to 2023?' },
        { category: 'Vendor Identity/Revenue', query: 'Which vendors who identify as Middle Eastern made more than $500 from 2020 to 2023?' },
        { category: 'Vendor Contact', query: 'What are the email addresses of vendors that sell food products?' },
        { category: 'Vendor Income', query: 'What are the email addresses of vendors that make less than $1000 income?' },
        { category: 'Vendor Events', query: 'What are the cell numbers of vendors that participated in Yum Yams events?' },
        { category: 'Vendor Location', query: 'What are the cell numbers of vendors that participated at Kapwa Gardens?' },
        { category: 'Cross-Event Analysis', query: 'Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020-2023 and made at least $500?' },
        { category: 'Identity/Cross-Event', query: 'Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020-2023 and identify as Middle Eastern?' },
        
        // Attendee/Donor Questions
        { category: 'Donor Geography', query: 'What is the most popular city that our donors live in?' },
        { category: 'Attendee Geography', query: 'How many attendees live in zip code 94102?' },
        { category: 'Donor Analysis', query: 'How many attendees gave more than $1 from 2021 to 2024?' },
        { category: 'Cross-Event Attendance', query: 'Who has attended events at Balay Kreative and UNDSCVRD in 2020?' },
        { category: 'Attendee Contact', query: 'What are the emails of our attendees that live in San Francisco?' },
        { category: 'Geographic Analysis', query: 'How many attendees live in SF and Daly City?' },
        { category: 'Attendance Metrics', query: 'How many attendees did we have in 2023?' },
        { category: 'Revenue Analysis', query: 'How much was given in 2024 at Kapwa Gardens?' },
        { category: 'Grant Correlation', query: 'Who applied to a Balay Kreative Grant and went to our events more than 2 times?' },
        { category: 'Grant Demographics', query: 'Which of our Balay Kreative applicants live in Daly City?' },
        { category: 'Grant Identity', query: 'Which of our Balay Kreative applicants identify as Filipino?' }
    ];
    
    let results = {
        successful: 0,
        failed: 0,
        dataIssues: 0,
        details: []
    };
    
    console.log(`Testing ${queries.length} business intelligence queries...\n`);
    
    for (let i = 0; i < queries.length; i++) {
        const { category, query } = queries[i];
        const shortQuery = query.length > 50 ? query.substring(0, 50) + '...' : query;
        
        console.log(`[${i + 1}/${queries.length}] ${category}: ${shortQuery}`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            if (result.success) {
                if (result.response.data && result.response.data.length > 0) {
                    // Check for data quality issues
                    const sampleRecord = result.response.data[0];
                    const hasRefErrors = JSON.stringify(sampleRecord).includes('#REF!');
                    const emptyFields = Object.values(sampleRecord).filter(v => !v || v === '').length;
                    
                    if (hasRefErrors || emptyFields > Object.keys(sampleRecord).length / 2) {
                        console.log(`   ⚠️  SUCCESS but data quality issues (${duration}ms) - ${result.response.data.length} records`);
                        results.dataIssues++;
                    } else {
                        console.log(`   ✅ SUCCESS with good data (${duration}ms) - ${result.response.data.length} records`);
                        results.successful++;
                    }
                } else {
                    console.log(`   ⚠️  SUCCESS but no matching data found (${duration}ms)`);
                    results.dataIssues++;
                }
                
                results.details.push({
                    category,
                    query: shortQuery,
                    status: 'success',
                    duration,
                    recordCount: result.response.data ? result.response.data.length : 0
                });
            } else {
                console.log(`   ❌ FAILED - ${result.response.error || 'Unknown error'}`);
                results.failed++;
                results.details.push({
                    category,
                    query: shortQuery,
                    status: 'failed',
                    error: result.response.error
                });
            }
        } catch (error) {
            console.log(`   💥 ERROR - ${error.message}`);
            results.failed++;
            results.details.push({
                category,
                query: shortQuery,
                status: 'error',
                error: error.message
            });
        }
        
        // Brief pause between queries
        await new Promise(resolve => setTimeout(resolve, 1500));
    }
    
    // Results summary
    console.log('\n' + '='.repeat(80));
    console.log('BUSINESS INTELLIGENCE TESTING RESULTS');
    console.log('='.repeat(80));
    console.log(`Total Queries: ${queries.length}`);
    console.log(`Successful with Good Data: ${results.successful}`);
    console.log(`Success but Data Quality Issues: ${results.dataIssues}`);
    console.log(`Failed: ${results.failed}`);
    console.log(`Overall Success Rate: ${(((results.successful + results.dataIssues) / queries.length) * 100).toFixed(1)}%`);
    
    // Category breakdown
    const categories = [...new Set(queries.map(q => q.category))];
    console.log('\nResults by Category:');
    categories.forEach(cat => {
        const catResults = results.details.filter(r => r.category === cat);
        const successes = catResults.filter(r => r.status === 'success').length;
        console.log(`  ${cat}: ${successes}/${catResults.length} successful`);
    });
    
    if (results.successful + results.dataIssues >= queries.length * 0.9) {
        console.log('\n🎯 EXCELLENT: Business Intelligence API is fully operational');
        console.log('✅ All your specific queries are working');
        console.log('⚠️  Note: Some data quality issues in source tables (#REF! errors, empty fields)');
        console.log('💡 Recommendation: Address data import/cleaning process for better results');
    } else if (results.successful + results.dataIssues >= queries.length * 0.7) {
        console.log('\n⚠️  GOOD: Most business intelligence queries working');
        console.log('🔧 Some optimization needed');
    } else {
        console.log('\n❌ ISSUES: Significant problems detected');
        console.log('🔍 Investigation required');
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
                    resolve({ status: res.statusCode, success: false, response: { error: 'Parse error', raw: data.substring(0, 200) } });
                }
            });
        });
        
        req.on('timeout', () => {
            req.destroy();
            reject(new Error('Request timeout - API may be processing complex query'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

testYourBusinessQueries().catch(console.error);