import fetch from 'node-fetch';

async function diagnoseAPIQueries() {
    console.log('DIRECT API DIAGNOSIS - Testing Your Business Intelligence Queries\n');
    
    const testQueries = [
        // Simple test first
        { category: 'Basic', query: 'show me tables', expected: 'table list' },
        
        // Your specific vendor questions
        { category: 'Vendor Revenue', query: 'How much money was made by vendors at Kapwa Gardens events in 2023?', expected: 'revenue data' },
        { category: 'Vendor Events', query: 'Which event from 2020 to 2023 made the most money for vendors?', expected: 'event comparison' },
        { category: 'Vendor Top 5', query: 'Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?', expected: 'vendor rankings' },
        { category: 'Vendor Geography', query: 'What zip codes are our vendors from who participated from 2020 to 2023?', expected: 'zip code list' },
        { category: 'Vendor Identity', query: 'Which vendors who identify as Middle Eastern made more than $500 from 2020 to 2023?', expected: 'filtered vendor list' },
        
        // Your attendee questions  
        { category: 'Attendee Geography', query: 'What is the most popular city that our donors live in?', expected: 'city analysis' },
        { category: 'Attendee Count', query: 'How many attendees live in zip code 94102?', expected: 'count result' },
        { category: 'Donor Behavior', query: 'How many attendees gave more than $1 from 2021 to 2024?', expected: 'donor count' },
        { category: 'Cross Events', query: 'Who has attended events at Balay Kreative and UNDSCVRD in 2020?', expected: 'attendee list' },
    ];
    
    let results = [];
    
    for (let i = 0; i < testQueries.length; i++) {
        const { category, query, expected } = testQueries[i];
        console.log(`\n[${i + 1}/${testQueries.length}] ${category}: ${query.substring(0, 60)}...`);
        
        try {
            const startTime = Date.now();
            const response = await makeAPIRequest(query);
            const duration = Date.now() - startTime;
            
            // Detailed analysis of response
            if (response.ok) {
                const data = await response.json();
                console.log(`   ✅ HTTP ${response.status} (${duration}ms)`);
                
                // Analyze response structure
                if (data.success) {
                    if (data.data && Array.isArray(data.data)) {
                        console.log(`   📊 Data: ${data.data.length} records returned`);
                        
                        if (data.data.length > 0) {
                            const sample = data.data[0];
                            const fields = Object.keys(sample);
                            console.log(`   🔍 Fields: ${fields.slice(0, 5).join(', ')}${fields.length > 5 ? '...' : ''}`);
                            
                            // Check for data quality issues
                            const hasRefErrors = JSON.stringify(sample).includes('#REF!');
                            const emptyValues = Object.values(sample).filter(v => !v || v === '' || v === '0' || v === '$0.00').length;
                            
                            if (hasRefErrors) {
                                console.log(`   ⚠️  Contains #REF! errors - source data issue`);
                            }
                            if (emptyValues > fields.length / 2) {
                                console.log(`   ⚠️  Many empty fields (${emptyValues}/${fields.length}) - data quality issue`);
                            }
                            if (!hasRefErrors && emptyValues <= fields.length / 3) {
                                console.log(`   ✨ Good data quality detected`);
                            }
                        } else {
                            console.log(`   ⚠️  No matching records found - may need query refinement`);
                        }
                    } else if (data.tables) {
                        console.log(`   📋 Tables: ${data.total_tables || data.tables.length} found`);
                    } else {
                        console.log(`   ❓ Response structure: ${JSON.stringify(data).substring(0, 100)}...`);
                    }
                } else {
                    console.log(`   ❌ API Error: ${data.error || 'Unknown error'}`);
                    if (data.details) {
                        console.log(`   🔍 Details: ${data.details}`);
                    }
                }
                
                results.push({
                    category,
                    query: query.substring(0, 50),
                    status: 'success',
                    httpStatus: response.status,
                    duration,
                    dataReturned: data.data ? data.data.length : 0,
                    hasDataQualityIssues: data.data && data.data.length > 0 ? JSON.stringify(data.data[0]).includes('#REF!') : false
                });
                
            } else {
                console.log(`   ❌ HTTP ${response.status} - ${response.statusText}`);
                const errorText = await response.text();
                console.log(`   🔍 Error: ${errorText.substring(0, 200)}`);
                
                results.push({
                    category,
                    query: query.substring(0, 50),
                    status: 'http_error',
                    httpStatus: response.status,
                    duration,
                    error: errorText.substring(0, 100)
                });
            }
            
        } catch (error) {
            console.log(`   💥 Request Error: ${error.message}`);
            results.push({
                category,
                query: query.substring(0, 50),
                status: 'request_error',
                duration: Date.now() - startTime,
                error: error.message
            });
        }
        
        // Small delay between requests
        await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    // Analysis summary
    console.log('\n' + '='.repeat(80));
    console.log('DIAGNOSIS SUMMARY');
    console.log('='.repeat(80));
    
    const successful = results.filter(r => r.status === 'success' && r.dataReturned > 0);
    const dataQualityIssues = results.filter(r => r.hasDataQualityIssues);
    const noData = results.filter(r => r.status === 'success' && r.dataReturned === 0);
    const errors = results.filter(r => r.status !== 'success');
    
    console.log(`Total Queries: ${results.length}`);
    console.log(`Successful with Data: ${successful.length}`);
    console.log(`Data Quality Issues: ${dataQualityIssues.length}`);
    console.log(`No Data Returned: ${noData.length}`);
    console.log(`Errors: ${errors.length}`);
    
    if (errors.length > 0) {
        console.log('\nERRORS DETECTED:');
        errors.forEach(e => {
            console.log(`  • ${e.category}: ${e.error}`);
        });
    }
    
    if (dataQualityIssues.length > 0) {
        console.log('\nDATA QUALITY ISSUES:');
        console.log('  • #REF! errors in source data (spreadsheet import issues)');
        console.log('  • Empty fields and zero values common');
        console.log('  • This is a data source problem, not API failure');
    }
    
    if (noData.length > 0) {
        console.log('\nQUERIES WITH NO DATA:');
        noData.forEach(n => {
            console.log(`  • ${n.category}: May need table name or query refinement`);
        });
    }
    
    // Recommendations
    console.log('\nRECOMMENDATIONS:');
    if (successful.length >= results.length * 0.8) {
        console.log('✅ API is working well - focus on data quality improvements');
        console.log('  • Clean up #REF! errors in BigQuery source tables');
        console.log('  • Validate data import processes');
        console.log('  • Consider data validation rules');
    } else if (errors.length > results.length * 0.5) {
        console.log('❌ API has technical issues - needs debugging');
        console.log('  • Check backend server status');
        console.log('  • Verify BigQuery credentials and permissions');
        console.log('  • Review error logs for specific issues');
    } else {
        console.log('⚠️  Mixed results - API works but needs optimization');
        console.log('  • Some queries need refinement');
        console.log('  • Data source improvements recommended');
    }
}

async function makeAPIRequest(query) {
    // Create conversation first
    const convResponse = await fetch('http://localhost:5000/api/conversations', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            title: 'API Test Conversation'
        })
    });
    
    if (!convResponse.ok) {
        throw new Error(`Failed to create conversation: ${convResponse.status}`);
    }
    
    const conversation = await convResponse.json();
    
    // Send message with correct format
    const response = await fetch(`http://localhost:5000/api/conversations/${conversation.id}/messages`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            message: query
        }),
        timeout: 30000
    });
    
    return response;
}

diagnoseAPIQueries().catch(console.error);