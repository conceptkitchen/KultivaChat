#!/usr/bin/env node
/**
 * Test location and event data extraction capabilities
 */

async function testLocationQueries() {
    const baseUrl = 'https://kultiva-chatv-2-mcp-conceptkitchen.replit.app';
    
    const queries = [
        // Event date queries
        "What events happened in 2023 vs 2024?",
        "Show me all UNDISCOVERED SF events by date",
        "Which event date had the highest total sales?",
        "What events happened in August 2023?",
        
        // Location queries
        "What venues or locations hosted our events?",
        "Show me event locations and addresses",
        "What cities have we held events in?",
        "Do we have venue information for events?",
        
        // Geographic vendor data
        "What zip codes are our vendors from?",
        "Show me vendor locations by city",
        "Which vendors are located in San Francisco?",
        "What areas do our attendees come from?",
        
        // Attendee geographic data
        "Show me attendee locations and zip codes",
        "How many attendees from each city?",
        "What's the geographic spread of our customers?",
        "Which zip codes have the most attendees?",
        
        // Event venue details
        "What venue details do we have for events?",
        "Show me event addresses if available",
        "Do we track event venue names?",
        "What location data is available for events?"
    ];
    
    console.log("🗺️  TESTING LOCATION & EVENT DATA EXTRACTION");
    console.log("=" * 60);
    
    for (let i = 0; i < queries.length; i++) {
        const query = queries[i];
        console.log(`\n${i + 1}. "${query}"`);
        
        try {
            const response = await fetch(`${baseUrl}/api/query`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ query })
            });
            
            const data = await response.json();
            
            if (data.status === 'success') {
                // Look for location/venue data in response
                const responseText = data.response || '';
                
                // Check for venue names
                const venueMatches = responseText.match(/venue|location|address|hall|center|park|building/gi);
                
                // Check for zip codes
                const zipMatches = responseText.match(/\b\d{5}\b/g);
                
                // Check for city names
                const cityMatches = responseText.match/San Francisco|SF|Daly City|Oakland|Berkeley|San Jose/gi);
                
                // Check for specific addresses
                const addressMatches = responseText.match(/\d+\s+[A-Za-z\s]+(?:Street|St|Avenue|Ave|Boulevard|Blvd|Road|Rd|Drive|Dr)/gi);
                
                console.log(`   Status: ✅ Success`);
                if (venueMatches) console.log(`   🏢 Venue data found: ${venueMatches.length} mentions`);
                if (zipMatches) console.log(`   📮 Zip codes: ${zipMatches.join(', ')}`);
                if (cityMatches) console.log(`   🏙️  Cities: ${cityMatches.join(', ')}`);
                if (addressMatches) console.log(`   🗺️  Addresses: ${addressMatches.join(', ')}`);
                
                // Show first 200 chars of response
                console.log(`   📄 Response: ${responseText.substring(0, 200)}...`);
                
            } else {
                console.log(`   Status: ❌ Error - ${data.error || 'Unknown error'}`);
            }
            
        } catch (error) {
            console.log(`   Status: ❌ Network error - ${error.message}`);
        }
        
        // Brief delay between requests
        await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    console.log("\n" + "=" * 60);
    console.log("🎯 LOCATION DATA ANALYSIS COMPLETE");
}

testLocationQueries().catch(console.error);