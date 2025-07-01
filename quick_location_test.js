#!/usr/bin/env node

async function testLocationCapabilities() {
    const baseUrl = 'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app';
    
    const testQueries = [
        "What venues or locations hosted our events?",
        "Show me vendor zip codes and addresses",
        "What cities are our vendors from?", 
        "Show me attendee locations by zip code",
        "Do we have any venue addresses for events?"
    ];
    
    console.log("🗺️  TESTING LOCATION DATA EXTRACTION\n");
    
    for (const query of testQueries) {
        try {
            console.log(`Testing: "${query}"`);
            
            const response = await fetch(`${baseUrl}/api/query`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ query })
            });
            
            const data = await response.json();
            
            if (data.status === 'success') {
                const text = data.response || '';
                
                // Look for location indicators
                const hasZips = /\b\d{5}\b/.test(text);
                const hasAddresses = /\d+\s+[A-Za-z\s]+(Street|St|Avenue|Ave|Boulevard|Blvd)/.test(text);
                const hasCities = /(San Francisco|Oakland|Berkeley|Daly City|SF)/i.test(text);
                const hasVenues = /(venue|hall|center|location|address)/i.test(text);
                
                console.log(`  ✅ Success`);
                if (hasZips) console.log(`  📮 Contains zip codes`);
                if (hasAddresses) console.log(`  🏠 Contains addresses`);
                if (hasCities) console.log(`  🏙️  Contains cities`);
                if (hasVenues) console.log(`  🏢 Contains venue info`);
                console.log(`  📄 Sample: ${text.substring(0, 150)}...\n`);
                
            } else {
                console.log(`  ❌ Error: ${data.error}\n`);
            }
            
        } catch (error) {
            console.log(`  ❌ Failed: ${error.message}\n`);
        }
        
        await new Promise(resolve => setTimeout(resolve, 2000));
    }
}

testLocationCapabilities().catch(console.error);