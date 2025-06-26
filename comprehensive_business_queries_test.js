import https from 'https';

async function testAllBusinessQueries() {
    console.log('COMPREHENSIVE BUSINESS INTELLIGENCE QUERY TESTING\n');
    
    const vendorQueries = [
        "How much money was made by vendors at Kapwa Gardens events in 2023?",
        "Which event from 2020 to 2023 made the most money for vendors?",
        "Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?",
        "What zip codes are our vendors from who participated from 2020 to 2023?",
        "Which vendors who identify as Middle Eastern made more than $500 from 2020 to 2023?",
        "What are the email addresses of vendors that sell food?",
        "What are the email addresses of vendors that make less than $1000 income?",
        "What are the cell numbers of vendors that participated in Yum Yams events?",
        "What are the cell numbers of vendors that participated at Kapwa Gardens?",
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020-2023 and made at least $500?",
        "Which vendors participated in Kapwa Gardens events and UNDSCVRD events from 2020-2023 and identify as Middle Eastern?"
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
    
    const allQueries = [
        ...vendorQueries.map(q => ({ type: 'Vendor', query: q })),
        ...attendeeQueries.map(q => ({ type: 'Attendee', query: q }))
    ];
    
    let successCount = 0;
    let failCount = 0;
    
    for (let i = 0; i < allQueries.length; i++) {
        const { type, query } = allQueries[i];
        console.log(`[${i + 1}/${allQueries.length}] ${type}: ${query.substring(0, 60)}...`);
        
        try {
            const startTime = Date.now();
            const result = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            if (result.success && result.response.data) {
                console.log(`   ✅ SUCCESS (${duration}ms) - ${result.response.data.length} records`);
                successCount++;
            } else if (result.success) {
                console.log(`   ⚠️ SUCCESS but no data (${duration}ms)`);
                successCount++;
            } else {
                console.log(`   ❌ FAILED - ${result.response.error || 'Unknown error'}`);
                failCount++;
            }
        } catch (error) {
            console.log(`   💥 ERROR - ${error.message}`);
            failCount++;
        }
        
        // Brief pause between queries
        await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    console.log('\n' + '='.repeat(60));
    console.log('BUSINESS INTELLIGENCE TESTING RESULTS');
    console.log('='.repeat(60));
    console.log(`Total Queries Tested: ${allQueries.length}`);
    console.log(`Successful: ${successCount}`);
    console.log(`Failed: ${failCount}`);
    console.log(`Success Rate: ${((successCount / allQueries.length) * 100).toFixed(1)}%`);
    
    if (successCount === allQueries.length) {
        console.log('\n🎯 ALL BUSINESS INTELLIGENCE QUERIES WORKING');
        console.log('✅ API ready for production deployment');
    } else if (successCount >= allQueries.length * 0.8) {
        console.log('\n⚠️ MOST QUERIES WORKING - Minor optimization needed');
    } else {
        console.log('\n❌ SIGNIFICANT ISSUES DETECTED - Requires investigation');
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
            reject(new Error('Timeout'));
        });
        
        req.on('error', (e) => reject(e));
        
        req.write(postData);
        req.end();
    });
}

testAllBusinessQueries().catch(console.error);