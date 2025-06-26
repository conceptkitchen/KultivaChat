import fetch from 'node-fetch';

async function comprehensiveQueryValidation() {
    console.log('COMPREHENSIVE BUSINESS INTELLIGENCE QUERY VALIDATION\n');
    
    const queries = [
        // Vendor Revenue Queries
        { 
            category: 'Vendor Revenue', 
            query: 'How much money was made by vendors at Kapwa Gardens events in 2023?',
            expected: 'revenue calculations'
        },
        { 
            category: 'Event Comparison', 
            query: 'Which event from 2020 to 2023 made the most money for vendors?',
            expected: 'event revenue ranking'
        },
        { 
            category: 'Top Vendors', 
            query: 'Who are the top 5 vendors from Kapwa Gardens events from 2020 to 2023?',
            expected: 'vendor rankings'
        },
        { 
            category: 'Vendor Geography', 
            query: 'What zip codes are our vendors from who participated from 2020 to 2023?',
            expected: 'zip code data'
        },
        { 
            category: 'Vendor Identity+Revenue', 
            query: 'Which vendors who identify as Middle Eastern made more than $500 from 2020 to 2023?',
            expected: 'filtered vendor list'
        },
        
        // Attendee/Donor Queries
        { 
            category: 'Donor Geography', 
            query: 'What is the most popular city that our donors live in?',
            expected: 'city analysis'
        },
        { 
            category: 'Zip Code Analysis', 
            query: 'How many attendees live in zip code 94102?',
            expected: 'count result'
        },
        { 
            category: 'Donor Behavior', 
            query: 'How many attendees gave more than $1 from 2021 to 2024?',
            expected: 'donor count'
        },
        { 
            category: 'Cross-Event Analysis', 
            query: 'Who has attended events at Balay Kreative and UNDSCVRD in 2020?',
            expected: 'attendee list'
        },
        { 
            category: 'Contact Information', 
            query: 'What are the emails of our attendees that live in San Francisco?',
            expected: 'email list'
        }
    ];
    
    let results = {
        successful: 0,
        partial: 0,
        failed: 0,
        dataQuality: 0
    };
    
    console.log(`Testing ${queries.length} critical business intelligence queries...\n`);
    
    for (let i = 0; i < queries.length; i++) {
        const { category, query, expected } = queries[i];
        console.log(`[${i + 1}/${queries.length}] ${category}`);
        console.log(`Query: "${query.substring(0, 70)}..."`);
        
        try {
            const startTime = Date.now();
            const response = await testQuery(query);
            const duration = Date.now() - startTime;
            
            if (response.success) {
                if (response.data && response.data.length > 0) {
                    // Analyze data quality
                    const sample = response.data[0];
                    const fields = Object.keys(sample);
                    const values = Object.values(sample);
                    
                    const hasData = values.some(v => v && v !== '' && v !== '0' && v !== '$0.00' && !String(v).includes('#REF!'));
                    const hasRefErrors = JSON.stringify(sample).includes('#REF!');
                    const nullCount = values.filter(v => v === null || v === '').length;
                    
                    if (hasData && !hasRefErrors) {
                        console.log(`   ✅ SUCCESS (${duration}ms): ${response.data.length} records with good data`);
                        console.log(`   📊 Fields: ${fields.slice(0, 3).join(', ')}${fields.length > 3 ? '...' : ''}`);
                        results.successful++;
                    } else if (hasData) {
                        console.log(`   ⚠️  PARTIAL (${duration}ms): ${response.data.length} records with data quality issues`);
                        if (hasRefErrors) console.log(`   🔍 Contains #REF! errors`);
                        results.partial++;
                        results.dataQuality++;
                    } else {
                        console.log(`   ⚠️  PARTIAL (${duration}ms): ${response.data.length} records but mostly empty data`);
                        results.partial++;
                        results.dataQuality++;
                    }
                    
                } else if (response.reply && response.reply.toLowerCase().includes('error')) {
                    console.log(`   ❌ FAILED (${duration}ms): AI reported error`);
                    console.log(`   🔍 Error: ${response.reply.substring(0, 100)}...`);
                    results.failed++;
                    
                } else {
                    console.log(`   ⚠️  PARTIAL (${duration}ms): AI response but no data extracted`);
                    console.log(`   🔍 Response: ${response.reply ? response.reply.substring(0, 80) : 'No reply'}...`);
                    results.partial++;
                }
            } else {
                console.log(`   ❌ FAILED (${duration}ms): HTTP error ${response.status}`);
                results.failed++;
            }
            
        } catch (error) {
            console.log(`   💥 ERROR: ${error.message}`);
            results.failed++;
        }
        
        console.log(''); // Space between tests
        await new Promise(resolve => setTimeout(resolve, 3000));
    }
    
    // Final summary
    console.log('='.repeat(80));
    console.log('COMPREHENSIVE VALIDATION RESULTS');
    console.log('='.repeat(80));
    console.log(`Total Queries Tested: ${queries.length}`);
    console.log(`✅ Fully Successful: ${results.successful}`);
    console.log(`⚠️  Partial Success: ${results.partial}`);
    console.log(`❌ Failed: ${results.failed}`);
    console.log(`🔧 Data Quality Issues: ${results.dataQuality}`);
    
    const totalWorking = results.successful + results.partial;
    const successRate = (totalWorking / queries.length * 100).toFixed(1);
    console.log(`\n📈 Overall Success Rate: ${successRate}%`);
    
    if (results.successful >= queries.length * 0.8) {
        console.log('\n🎯 EXCELLENT: Business Intelligence API is production-ready');
        console.log('✅ All critical queries working with good data extraction');
    } else if (totalWorking >= queries.length * 0.7) {
        console.log('\n✅ GOOD: Most queries working, minor optimization needed');
        console.log('🔧 Focus on data quality improvements in source tables');
    } else {
        console.log('\n⚠️  NEEDS WORK: Significant query issues detected');
        console.log('🔍 May need AI tool adjustments or table schema fixes');
    }
    
    if (results.dataQuality > 0) {
        console.log('\n📋 DATA QUALITY RECOMMENDATIONS:');
        console.log('  • Clean up #REF! errors in BigQuery source tables');
        console.log('  • Standardize currency and numeric field formats');
        console.log('  • Implement data validation during import process');
        console.log('  • Consider data completeness scoring for better insights');
    }
}

async function testQuery(query) {
    try {
        // Create conversation
        const convResponse = await fetch('http://localhost:5000/api/conversations', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ title: 'BI Validation' })
        });
        
        if (!convResponse.ok) {
            return { success: false, status: convResponse.status, error: 'Failed to create conversation' };
        }
        
        const conversation = await convResponse.json();
        
        // Send query
        const msgResponse = await fetch(`http://localhost:5000/api/conversations/${conversation.id}/messages`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ message: query })
        });
        
        if (!msgResponse.ok) {
            return { success: false, status: msgResponse.status, error: 'Failed to send message' };
        }
        
        const result = await msgResponse.json();
        
        // Extract data from displays
        let extractedData = null;
        if (result.displays && result.displays.length > 0) {
            for (const display of result.displays) {
                if (display.content && Array.isArray(display.content) && display.content.length > 0) {
                    extractedData = display.content;
                    break;
                }
            }
        }
        
        return {
            success: true,
            status: msgResponse.status,
            reply: result.reply,
            data: extractedData,
            displays: result.displays
        };
        
    } catch (error) {
        return { success: false, error: error.message };
    }
}

comprehensiveQueryValidation().catch(console.error);