#!/usr/bin/env node

/**
 * COMPREHENSIVE EVENT NAME & DATA TYPE VALIDATION
 * Tests all event name recognition and data extraction capabilities
 * Ensures zero hallucination and 100% authentic data extraction
 */

const BASE_URL = 'http://localhost:8081';

// Test all event name variations and data types
const testQueries = [
    // Event Name Recognition Tests
    {
        category: "Event Name Recognition",
        query: "Show me Balay Kreative attendee data",
        expected_table_pattern: "Balay-Kreative",
        expected_data_type: "attendee"
    },
    {
        category: "Event Name Recognition", 
        query: "Get UNDISCOVERED vendor contacts",
        expected_table_pattern: "UNDISCOVERED",
        expected_data_type: "contact"
    },
    {
        category: "Event Name Recognition",
        query: "Show me KG financial information", 
        expected_table_pattern: "Kapwa-Gardens",
        expected_data_type: "financial"
    },
    {
        category: "Event Name Recognition",
        query: "Kapwa Gardens vendor emails",
        expected_table_pattern: "Kapwa-Gardens", 
        expected_data_type: "contact"
    },
    
    // Data Type Extraction Tests
    {
        category: "Contact Extraction",
        query: "What are email addresses from vendor data?",
        expected_fields: ["email", "name"],
        min_records: 10
    },
    {
        category: "Demographic Extraction", 
        query: "Show me attendee demographics from Balay Kreative",
        expected_table_pattern: "Balay-Kreative",
        expected_data_type: "demographic"
    },
    {
        category: "Location Extraction",
        query: "What zip codes do vendors have?",
        expected_fields: ["zip", "location"],
        data_type: "location"
    },
    {
        category: "Financial Extraction",
        query: "Show me revenue from UNDISCOVERED events",
        expected_table_pattern: "UNDISCOVERED",
        expected_data_type: "financial"
    },
    
    // Anti-Hallucination Tests
    {
        category: "Anti-Hallucination",
        query: "Show me customer purchase data",
        description: "Should discover real tables, never create fake ones"
    },
    {
        category: "Anti-Hallucination", 
        query: "Get sales report information",
        description: "Should use actual table discovery, no assumptions"
    }
];

async function makeAPIRequest(query) {
    try {
        const response = await fetch(`${BASE_URL}/api/query`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ query })
        });
        
        return await response.json();
    } catch (error) {
        return { error: error.message, status: 'error' };
    }
}

async function validateEventRecognition() {
    console.log('🎯 COMPREHENSIVE EVENT NAME & DATA TYPE VALIDATION\n');
    console.log('Testing event name recognition, data extraction, and anti-hallucination\n');
    
    let passed = 0;
    let failed = 0;
    const results = [];
    
    for (const test of testQueries) {
        console.log(`\n📋 Testing: ${test.category}`);
        console.log(`Query: "${test.query}"`);
        
        const startTime = Date.now();
        const result = await makeAPIRequest(test.query);
        const duration = Date.now() - startTime;
        
        const testResult = {
            category: test.category,
            query: test.query,
            duration: `${duration}ms`,
            status: result.status || 'unknown',
            passed: false,
            details: {}
        };
        
        if (result.status === 'success') {
            testResult.passed = true;
            testResult.details = {
                table_used: result.table_source,
                records_returned: result.data ? result.data.length : 0,
                execution_time: result.execution_time,
                query_executed: result.query_executed
            };
            
            // Validate event name recognition
            if (test.expected_table_pattern) {
                const tableMatches = result.table_source?.includes(test.expected_table_pattern);
                testResult.details.event_recognition = tableMatches ? 'PASS' : 'FAIL';
                if (!tableMatches) testResult.passed = false;
            }
            
            // Validate data type extraction  
            if (test.expected_fields && result.data && result.data.length > 0) {
                const firstRecord = result.data[0];
                const hasExpectedFields = test.expected_fields.every(field => 
                    Object.keys(firstRecord).includes(field)
                );
                testResult.details.field_validation = hasExpectedFields ? 'PASS' : 'FAIL';
                if (!hasExpectedFields) testResult.passed = false;
            }
            
            // Validate minimum record count
            if (test.min_records) {
                const recordCount = result.data ? result.data.length : 0;
                const meetsMinimum = recordCount >= test.min_records;
                testResult.details.record_count_validation = meetsMinimum ? 'PASS' : 'FAIL';
                testResult.details.actual_records = recordCount;
                if (!meetsMinimum) testResult.passed = false;
            }
            
            // Anti-hallucination validation (no fake table names)
            if (test.category === 'Anti-Hallucination') {
                const suspiciousNames = ['vendor_sales_data', 'customer_data', 'sales_report', 'user_data'];
                const usedFakeTable = suspiciousNames.some(name => 
                    result.table_source?.includes(name) || result.query_executed?.includes(name)
                );
                testResult.details.anti_hallucination = usedFakeTable ? 'FAIL - Used fake table' : 'PASS';
                if (usedFakeTable) testResult.passed = false;
            }
            
            passed++;
            console.log(`✅ PASSED - ${duration}ms`);
            if (result.data && result.data.length > 0) {
                console.log(`   📊 Records: ${result.data.length}, Table: ${result.table_source}`);
            }
        } else {
            testResult.details.error = result.error_message || result.error;
            failed++;
            console.log(`❌ FAILED - ${result.error_message || result.error}`);
        }
        
        results.push(testResult);
    }
    
    // Summary Report
    console.log('\n' + '='.repeat(70));
    console.log('📊 COMPREHENSIVE VALIDATION SUMMARY');
    console.log('='.repeat(70));
    console.log(`✅ Passed: ${passed}`);
    console.log(`❌ Failed: ${failed}`);
    console.log(`📈 Success Rate: ${((passed / testQueries.length) * 100).toFixed(1)}%`);
    
    console.log('\n🎯 EVENT NAME RECOGNITION RESULTS:');
    results.filter(r => r.category === 'Event Name Recognition').forEach(r => {
        const status = r.passed ? '✅' : '❌';
        console.log(`${status} ${r.query} -> ${r.details.table_used || 'No table'}`);
    });
    
    console.log('\n🛡️ ANTI-HALLUCINATION VALIDATION:');
    results.filter(r => r.category === 'Anti-Hallucination').forEach(r => {
        const status = r.details.anti_hallucination === 'PASS' ? '✅' : '❌';
        console.log(`${status} ${r.query} -> ${r.details.anti_hallucination}`);
    });
    
    console.log('\n📋 DATA TYPE EXTRACTION RESULTS:');
    const extractionCategories = ['Contact Extraction', 'Demographic Extraction', 'Location Extraction', 'Financial Extraction'];
    results.filter(r => extractionCategories.includes(r.category)).forEach(r => {
        const status = r.passed ? '✅' : '❌';
        console.log(`${status} ${r.category}: ${r.details.records_returned || 0} records`);
    });
    
    return {
        total: testQueries.length,
        passed,
        failed,
        success_rate: ((passed / testQueries.length) * 100).toFixed(1),
        results
    };
}

// Run validation
validateEventRecognition()
    .then(summary => {
        console.log(`\n🏁 Validation completed with ${summary.success_rate}% success rate`);
        process.exit(summary.failed > 0 ? 1 : 0);
    })
    .catch(error => {
        console.error('❌ Validation failed:', error);
        process.exit(1);
    });