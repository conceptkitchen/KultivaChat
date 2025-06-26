import fetch from 'node-fetch';

async function testBusinessIntelligenceQueries() {
    console.log('BUSINESS INTELLIGENCE QUERY TESTING\n');
    
    // Start with simpler queries that should work
    const queries = [
        'show me my data tables',
        'What vendors do we have in our database?',
        'Show me some attendee data',
        'How many events do we have data for?'
    ];
    
    for (let i = 0; i < queries.length; i++) {
        const query = queries[i];
        console.log(`\n[${i + 1}] Testing: "${query}"`);
        
        try {
            const response = await testSingleQuery(query);
            console.log(`   Status: ${response.status}`);
            
            if (response.success) {
                if (response.data && response.data.length > 0) {
                    console.log(`   ✅ Success: ${response.data.length} records`);
                    console.log(`   Sample: ${JSON.stringify(response.data[0]).substring(0, 100)}...`);
                } else if (response.reply) {
                    console.log(`   ✅ Success: ${response.reply.substring(0, 100)}...`);
                } else {
                    console.log(`   ⚠️  Success but no data`);
                }
            } else {
                console.log(`   ❌ Failed: ${response.error || 'Unknown error'}`);
            }
            
        } catch (error) {
            console.log(`   💥 Error: ${error.message}`);
        }
        
        // Small delay
        await new Promise(resolve => setTimeout(resolve, 3000));
    }
}

async function testSingleQuery(query) {
    // Create conversation
    const convResponse = await fetch('http://localhost:5000/api/conversations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ title: 'Test Query' })
    });
    
    if (!convResponse.ok) {
        throw new Error(`Failed to create conversation: ${convResponse.status}`);
    }
    
    const conversation = await convResponse.json();
    
    // Send message
    const msgResponse = await fetch(`http://localhost:5000/api/conversations/${conversation.id}/messages`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message: query })
    });
    
    if (!msgResponse.ok) {
        throw new Error(`Message failed: ${msgResponse.status}`);
    }
    
    const result = await msgResponse.json();
    
    // Extract data from displays if available
    let extractedData = null;
    if (result.displays && result.displays.length > 0) {
        for (const display of result.displays) {
            if (display.data && Array.isArray(display.data)) {
                extractedData = display.data;
                break;
            }
        }
    }
    
    return {
        status: msgResponse.status,
        success: msgResponse.status === 200,
        reply: result.reply,
        displays: result.displays,
        data: extractedData,
        error: result.error
    };
}

testBusinessIntelligenceQueries().catch(console.error);