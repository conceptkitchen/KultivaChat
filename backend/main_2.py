import os
import re  # For fallback logic - moved to top
import uuid
import subprocess
from flask import Flask, jsonify, request
import requests
from google.cloud import bigquery
import logging
import json
import time
from datetime import datetime
from difflib import SequenceMatcher

# typing.Optional is crucial for parameters with a default value of None
from typing import Optional  # <<<<<<<<<<<< MAKE ABSOLUTELY SURE THIS LINE IS PRESENT AND AT THE TOP

# Using the import style from the documentation you provided
try:
    from google import genai as google_genai_for_client  # Alias to avoid conflict with older genai import
    from google.genai import types as google_genai_types
    GEMINI_SDK_AVAILABLE = True
    print("Successfully imported 'google.genai' and 'google.genai.types'")
except ImportError as e:
    print(
        f"Failed to import 'google.genai' or 'google.genai.types': {e}. Make sure 'google-generativeai' is installed and accessible with this import style (it might be part of google-cloud-aiplatform or a specific version)."
    )
    GEMINI_SDK_AVAILABLE = False

    # Define dummy classes if import fails, so Flask app can still load other routes
    class google_genai_types:

        class GenerateContentConfig:
            pass

        class Content:
            pass

        class Part:
            pass

    class google_genai_for_client:

        class Client:
            pass


# We still need HarmCategory and HarmBlockThreshold, try from google.generativeai.types
try:
    from google.generativeai.types import HarmCategory, HarmBlockThreshold
except ImportError:
    # Fallback if the primary SDK doesn't have them at this path either
    class HarmCategory:
        HARM_CATEGORY_HARASSMENT = None
        HARM_CATEGORY_HATE_SPEECH = None
        HARM_CATEGORY_SEXUALLY_EXPLICIT = None
        HARM_CATEGORY_DANGEROUS_CONTENT = None  # Add all relevant

    class HarmBlockThreshold:
        BLOCK_MEDIUM_AND_ABOVE = None
        BLOCK_NONE = None  # Add all relevant


# --- Initialize Flask App ---
app = Flask(__name__)
logging.basicConfig(level=logging.INFO)
app.logger.setLevel(logging.INFO)

# Global variable to capture SQL results during tool execution
last_sql_results = None

# --- Load Configuration ---
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get(
    'GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')
KBC_API_URL = os.getenv('KBC_API_URL')
KBC_STORAGE_TOKEN = os.getenv('KBC_STORAGE_TOKEN')

# Add configuration variables for project and workspace IDs
GOOGLE_PROJECT_ID = os.environ.get('GOOGLE_PROJECT_ID', 'kbc-use4-839-261b')
KBC_WORKSPACE_ID = os.environ.get('KBC_WORKSPACE_SCHEMA', 'WORKSPACE_21894820')
GEMINI_MODEL = "gemini-2.0-flash-exp"

# Dashboard Configuration Variables
GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES', '/home/runner/workspace/backend/GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES.json')
KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES = os.environ.get('KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES', 'WORKSPACE_DASHBOARD_CLOSE_OUT_SALES')

# Squarespace Forms Dashboard Configuration
GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS', '/home/runner/workspace/backend/GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS.json')
KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS = os.environ.get('KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS', 'WORKSPACE_DASHBOARD_SQUARESPACE_FORMS')

DASHBOARD_PROJECT_ID = GOOGLE_PROJECT_ID  # Same project, different workspaces

# Log configuration status for debugging
app.logger.info("=== Configuration Check ===")
app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'MISSING'}")
app.logger.info(f"KBC_WORKSPACE_SCHEMA: {'SET' if KBC_WORKSPACE_SCHEMA else 'MISSING'}")
app.logger.info(f"GEMINI_API_KEY: {'SET' if GEMINI_API_KEY else 'MISSING'}")
app.logger.info(f"GOOGLE_PROJECT_ID: {GOOGLE_PROJECT_ID}")
app.logger.info(f"KBC_WORKSPACE_ID: {KBC_WORKSPACE_ID}")

# --- Define System Instruction Constant ---
SYSTEM_INSTRUCTION_PROMPT = f"""You are an expert BigQuery Data Analyst Assistant specializing in comprehensive data extraction and analysis for event management data. Your workspace contains vendor sales data, attendee information, contact details, demographics, locations, and financial analytics from various sources including Kapwa Gardens, UNDISCOVERED, Balay Kreative, and other events.

**WORKSPACE DETAILS:**
- Project: `{GOOGLE_PROJECT_ID}` 
- Dataset: `{KBC_WORKSPACE_ID}`
- Data Sources: 28 closeout sales tables, 9 squarespace forms, 1 typeform data

**COMPREHENSIVE DATA EXTRACTION APPROACH:**

1. **DISCOVER TABLES FIRST:** Always start with table discovery to see what data is available:
   ```sql
   SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
   WHERE table_name NOT LIKE '-%' ORDER BY table_name
   ```

2. **ANALYZE TABLE SCHEMAS:** Examine actual column structures to understand available data:
   ```sql
   SELECT column_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
   WHERE table_name = 'TABLE_NAME' ORDER BY ordinal_position
   ```

3. **MATCH QUERY TYPE TO DATA EXTRACTION:** Determine what type of information is being requested and extract accordingly.

**YOUR TOOLS:**
- `internal_execute_sql_query`: Execute any BigQuery SQL query for all data types
- `get_zip_codes_for_city`: Get zip codes for geographic analysis  
- `get_current_time`: Get current date/time
- `get_keboola_table_detail`: Get table metadata

**COMPREHENSIVE QUERY HANDLING RULES:**

**1. CONTACT INFORMATION QUERIES** (emails, phone numbers, contact names):
- Extract actual contact details with specific field names
- Query format: SELECT vendor_name, email, phone_number, contact_person FROM table
- Include filters for non-empty contact fields
- Return actual contact records, NOT counts or aggregations

**2. DEMOGRAPHIC QUERIES** (age, gender, occupation, ethnicity, income):
- Extract actual demographic fields from attendee/vendor forms
- Query format: SELECT name, age, gender, occupation, income_level FROM table
- Include demographic filtering based on user criteria
- Return individual demographic records with specific details

**3. LOCATION QUERIES** (addresses, cities, zip codes, states):
- Extract geographical information from vendor/attendee registration data
- Query format: SELECT vendor_name, address, city, state, zip_code FROM table
- Use geographic filtering and location-based analysis
- Combine with zip code lookup tool when needed

**4. FINANCIAL QUERIES** (revenue, sales, payments, costs):
- Extract actual financial amounts with vendor names and event details
- Query format: SELECT vendor_name, total_sales, event_date, payment_method FROM table
- Include revenue calculations, vendor performance, profit analysis
- Provide specific dollar amounts and financial metrics

**5. NAME/IDENTITY QUERIES** (vendor names, business names, attendee names):
- Extract actual names and business identities
- Query format: SELECT vendor_name, business_name, contact_person, event_name FROM table
- Include name-based filtering and identity matching
- Return specific names and business identities

**6. EVENT ANALYSIS QUERIES** (event participation, attendance, vendor lists):
- Extract event-specific data including dates, locations, participants
- Query format: SELECT event_name, event_date, vendor_count, attendee_count FROM table
- Include cross-event analysis and participation tracking
- Provide event-specific insights with authentic data

**7. DONOR INFORMATION QUERIES** (donations, sponsors, contributors, supporters):
- Extract donor details including names, contribution amounts, purchase amounts
- IMPORTANT: Donors are people who PAY for events (attendees who purchase tickets)
- DONOR QUERIES use "Balay-Kreative---attendees---all-orders" table (people who paid for tickets)
- GRANT queries use "typeform_report_balay_kreative_forms" table (people who received funding)
- Query format: SELECT donor_name, purchase_amount, event_name, billing_city FROM attendees table
- Include donor analysis, supporter tracking, ticket purchase data
- Return actual supporter records who financially contributed through ticket purchases

**EVENT-SPECIFIC DATA EXTRACTION:**
- When users mention specific events, target ONLY that event's tables
- "Balay Kreative attendees" → extract from Balay-Kreative attendee tables only
- "UNDISCOVERED vendor emails" → extract from UNDISCOVERED vendor tables only
- "KG financial data" → extract from Kapwa-Gardens financial tables only
- Never mix data from different events unless specifically requested for comparison

**CRITICAL EXTRACTION RULES:**
- ALWAYS extract actual data records, never just counts unless specifically requested
- Use appropriate column names based on table schema analysis
- Include filters to exclude empty/null values in key fields
- Provide specific names, amounts, dates, and details from real data
- Match query intent to appropriate data extraction method
- Return raw data records when users ask for lists, contacts, demographics, or locations
- Recognize event names (Balay Kreative, UNDISCOVERED, KG/Kapwa Gardens) for accurate targeting

**INTELLIGENT TABLE DISCOVERY AND MATCHING:**

1. **MANDATORY TABLE DISCOVERY FIRST**: ALWAYS start with this exact query:
   ```sql
   SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
   WHERE table_name NOT LIKE '-%' ORDER BY table_name
   ```
   **NEVER proceed without discovering actual table names first**
   **NEVER assume, guess, or create table names like "vendor_sales_data"**

2. **ANALYZE TABLE SCHEMAS**: For relevant tables, examine column structures:
   ```sql
   SELECT column_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
   WHERE table_name = 'SPECIFIC_TABLE_NAME' ORDER BY ordinal_position
   ```

3. **SMART TABLE MATCHING**: Use semantic understanding to match user requests:
   - "contact information" → tables with Email, Phone, Contact_Name columns
   - "demographics" → tables with Age, Gender, Occupation, Income columns  
   - "locations" → tables with Address, City, State, Zip_Code columns
   - "financial data" → tables with Total_Sales, Revenue, Payment columns
   - "vendor names" → tables with Vendor_Name, Business_Name columns
   - "event data" → tables with Event_Name, Event_Date columns
   - "donor information" → "Balay-Kreative---attendees---all-orders" table (people who paid for tickets)
   - "grant information" → "typeform_report_balay_kreative_forms" table (people who received funding)

4. **EVENT NAME RECOGNITION AND MATCHING**: Essential for accurate data extraction:
   - "Balay Kreative" or "balay" → "Balay-Kreative" tables
   - "UNDISCOVERED" or "undiscovered" → "UNDISCOVERED-SF" tables
   - "Kapwa Gardens" or "KG" → "Kapwa-Gardens" tables  
   - "Yum Yams" → "Yum-Yams" tables
   - "Lovers Mart" → "Lovers-Mart" tables
   - "MatchaKOHO" → "MatchaKOHO" tables
   - Event-specific requests always target the correct event tables, never mix data

5. **FUZZY MATCHING PATTERNS**: For natural language requests:
   - "undiscovered attendees" → "Undiscovered---Attendees-Export---Squarespace"
   - "kapwa gardens vendors" → tables containing "Kapwa-Gardens" AND ("vendor" OR "close-out")
   - "balay kreative demographics" → "Balay-Kreative" tables with demographic fields
   - "kg vendor emails" → "Kapwa-Gardens" tables with email columns
   - "yum yams contacts" → "Yum-Yams" tables with contact fields

6. **GEOGRAPHIC QUERIES**: For location-based attendee questions:
   - "How many attendees live in San Francisco and Daly City?" → Use Billing_City column with WHERE clause filtering
   - "attendees from SF" → Filter WHERE UPPER(Billing_City) IN ('SAN FRANCISCO', 'SF')  
   - "daly city attendees" → Filter WHERE UPPER(Billing_City) LIKE '%DALY CITY%'
   - Always apply geographic filters to attendee tables using city/location columns
   - Combine counts from multiple attendee tables for total geographic results
   
7. **BE DECISIVE**: When tables are found, immediately extract the requested data type without asking for clarification.

**CRITICAL: NEVER HALLUCINATE TABLE NAMES**
- ALWAYS discover actual table names first using INFORMATION_SCHEMA.TABLES
- NEVER assume table names like "vendor_sales_data", "customer_data", etc.
- ONLY query tables that actually exist in the workspace
- If no matching tables found, report "No tables found matching criteria" instead of creating fake names

**CRITICAL: ALL TABLES IN THIS WORKSPACE ARE BIGQUERY VIEWS - NEVER USE WILDCARD PATTERNS**
- FORBIDDEN: `FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.*\``
- FORBIDDEN: Wildcard table patterns, prefix queries, or multiple table unions in FROM clauses
- REQUIRED: Always query ONE specific table at a time by exact name
- REQUIRED: Use table discovery first, then SELECT from individual named tables only
- EXAMPLE CORRECT: `SELECT * FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Close-Outs---Yum-Yams---2023-05-13---Kapwa-Gardens-All-Vendor-Close-Out-Sales\` LIMIT 5`

**Your absolute priority for data retrieval and answering questions about specific table contents is the transformed tables available in the Google BigQuery workspace (`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.TABLE_NAME`).**

When users ask about:
- **Data from specific BigQuery workspace tables** (e.g., "show me kapwa gardens vendor data", "what's in the close-out sales?", "can you show me balay kreative data?"):
    1.  **USE FUZZY TABLE MATCHING** first, then query the actual BigQuery table names from the workspace.
    2.  **TABLE NAMING PATTERN**: The tables use descriptive names with special characters like:
        - "kapwa gardens" data → Look for tables containing "Kapwa-Gardens" 
        - "vendor data" → Look for tables with vendor names like "Balay-Kreative", "Yum-Yams", "MatchaKOHO"
        - "close-out sales" → Look for tables containing "Close-Out-Sales" or "Close-Outs"
        - "undiscovered attendees" → Look for "Undiscovered" AND "Attendees" in table names
        - "undiscovered" events → Look for tables containing "UNDISCOVERED-SF"
        - "market data" → Look for tables containing "Market-Recap"
    3.  **WHEN MULTIPLE SIMILAR TABLES EXIST** (e.g., multiple dates or events):
        - **LIST AVAILABLE OPTIONS** and ask user to specify which date/event they want
        - If user says "latest" or "most recent", choose the most recent date in the table name
        - Execute the query immediately and mention which specific table you used in your response
    3.  **EXECUTE THE QUERY IMMEDIATELY** using `internal_execute_sql_query` tool - do not ask for clarification or confirmation.
    4.  Use the format: `SELECT * FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.\` + actual table name + \` LIMIT 10;`
        IMPORTANT: Table names contain special characters and must be wrapped in backticks exactly as shown in the table list
    5.  **FORBIDDEN RESPONSES:** Do NOT say "Which table would you like?" or "I need more information" or "Could you clarify?" - just pick a table and execute.
    6.  **NEVER claim a table doesn't exist** if you can find ANY reasonable pattern match in the conversation history.

- **ALL BUSINESS INTELLIGENCE QUESTIONS MUST USE ACCURATE TABLE FILTERING**:
    1.  **CRITICAL**: When user mentions specific events/dates (e.g., "UNDISCOVERED SF August 19, 2023", "Yum Yams May 13, 2023"), you MUST target the exact matching table, NOT a random table
    2.  **EVENT/DATE MATCHING REQUIRED**: 
        * "UNDISCOVERED SF August 19, 2023" → ONLY use table "2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors"
        * "Lovers Mart February 11, 2023" → ONLY use table "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal"
        * "Yum Yams May 13, 2023" → ONLY use table containing "2023-05-13" AND "Yum-Yams"
    3.  **NEVER return wrong event data** - if user asks for UNDISCOVERED August 2023, never return Lovers-Mart February data
    4.  **PROCESS**: 
        * First: Discover all tables with INFORMATION_SCHEMA
        * Second: Apply smart filtering to match query to correct table
        * Third: Execute SQL on the SPECIFIC matched table only
        * Fourth: Return data with table source clearly identified
    5.  **TABLE FILTERING LOGIC**: Score tables by:
        * Event name match: +10 points (UNDISCOVERED matches "undiscovered" in table)
        * Date match: +20 points (August 19, 2023 matches "2023-08-19" in table)
        * Use highest scoring table only

- "Show me tables" or "what tables do I have" (referring to the transformed data):
    1.  Prioritize listing tables from the BigQuery workspace.
    2.  Use `execute_sql_query` with: `SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\` ORDER BY table_name;`.
- Keboola Storage Buckets/Tables (for raw data exploration): If the user explicitly asks about "buckets," raw data, or uses Keboola Storage specific IDs, use `list_keboola_buckets`, `list_tables_in_keboola_bucket`, and `get_keboola_table_detail` (for Storage table schemas only).

The database details for querying the primary data warehouse:
- Project: `kbc-use4-839-261b`
- Dataset: `WORKSPACE_21894820`
- Always use fully qualified table names in SQL: `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME`

You have the following tools at your disposal:

1.  `list_keboola_buckets`:
    * **Description:** Lists all available top-level data categories (buckets) in the Keboola Storage project. Use this as a first step to understand the overall data landscape if the user's query is broad or if specific table locations are unknown, especially if they are asking about raw data sources rather than transformed BigQuery tables.
    * **Parameters:** None.
    * **Returns:** A list of bucket objects, each containing 'id', 'name', and 'stage'.

2.  `list_tables_in_keboola_bucket`:
    * **Description:** Lists all specific datasets (tables) and their row counts within a chosen Keboola Storage bucket. Use this after identifying a relevant Keboola Storage bucket to see what specific raw data tables it contains.
    * **Parameters:**
        * `bucket_id` (string, required): The ID of the Keboola Storage bucket (e.g., 'in.c-mydata' or 'out.c-transformeddata') from which to list tables.
    * **Returns:** A list of table objects, each containing 'id', 'name', and 'rowsCount'.

3.  `get_keboola_table_detail`:
    * **Description:** Retrieves the detailed schema (column names, data types) and other metadata (like row count, primary key) for a specific **Keboola Storage table ID ONLY**. Do **NOT** use this tool for BigQuery workspace tables; for BigQuery table schemas, use an `INFORMATION_SCHEMA.COLUMNS` query via the `execute_sql_query` tool.
    * **Parameters:**
        * `table_id` (string, required): The full ID of the Keboola Storage table (e.g., 'in.c-mybucket.mytable' or 'out.c-transformeddata.final_output_table').
    * **Returns:** An object containing 'id', 'name', 'columns' (a list of objects, each with 'name' and 'type'), 'rowsCount', and 'primaryKey'.

4.  `execute_sql_query`:
    * **Description:** Your **CENTRAL tool** for all interactions with data in the BigQuery workspace (`kbc-use4-839-261b.WORKSPACE_21894820`). This includes:
        * Directly fetching data from tables via natural language (e.g., "show me `TABLE_NAME`" -> `SELECT * ... LIMIT 10`).
        * Executing complex analytical queries involving JOINs, aggregations, filtering.
        * Discovering available tables in BigQuery (`INFORMATION_SCHEMA.TABLES`).
        * Retrieving BigQuery table schemas for complex query construction (`INFORMATION_SCHEMA.COLUMNS`).
    * **Parameters:** `sql_query` (string, required): The BigQuery SQL SELECT query to execute.
    * **CRITICAL INSTRUCTIONS FOR SQL:**
        * Table names in your SQL queries **MUST** be fully qualified: `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME_IN_WORKSPACE`.
        * **INTELLIGENT TABLE MATCHING:** Use semantic understanding to match user requests to actual table names from conversation history:
          - Analyze the user's intent (what data are they asking for?)
          - Look for tables containing relevant keywords from their request
          - For company-specific requests: "kapwa gardens" → tables with "KAPWA_GARDENS", "kultivate labs" → "KULTIVATE_LABS", etc.
          - For data type requests: "customers" → "CUSTOMERS", "orders" → "ORDERS", "products" → "PRODUCTS", "forms" → "FORMS" or "TYPEFORM"
        * **WHEN MULTIPLE SIMILAR TABLES EXIST:** Automatically pick the highest numbered version (e.g., `_6_` over `_2_`) and execute immediately.
        * **MANDATORY:** If you find ANY reasonable match in conversation history, execute the query immediately. DO NOT ask for clarification, confirmation, or which table to use.
        * Use standard BigQuery SQL syntax.
        * Quote column and table names with backticks if they contain special characters or are reserved keywords.
        * For general "show me data" requests (i.e., `SELECT *`), always include a `LIMIT 10` or `LIMIT 20` to ensure performance and manageable results.
    * **Returns:** A list of dictionaries, where each dictionary represents a row from the query result, or an error if the query fails.

5.  `get_zip_codes_for_city`:
    * **Description:** Retrieves a list of common zip codes for a given city and optional state. Use this when you need to filter by city in a SQL query but only a `zip_code` column is available in the table and no internal city-to-zip mapping table is found.
    * **Parameters:**
        * `city_name` (string, required): The name of the city (e.g., "San Francisco", "Daly City").
        * `state_code` (string, optional): The 2-letter state code (e.g., "CA") to improve accuracy. If unsure, you may try to infer it or omit it.
    * **Returns:** A dictionary with 'status' ('success' or 'error'), and if successful, 'zip_codes' (a list of string zip codes), otherwise 'error_message'.

6.  `get_current_time`:
    * **Description:** Returns the current date, time, and timezone. Use only when explicitly asked for the time or date.
    * **Parameters:** None.
    * **Returns:** An object with 'current_time'.

**Your Task and Workflow (Revised Flow):**

1.  **Understand User's Goal & Identify Query Type:**
    * **Type A:** Direct request to see data from an explicitly named BigQuery table?
    * **Type B:** Complex analytical question or data request about entities/activities where the table name(s) might not be explicit (requiring table discovery and potentially complex SQL)? This includes cases where city-to-zip translation might be needed via `get_zip_codes_for_city`.
    * **Type C:** Request to list available BigQuery tables?
    * **Type D:** Request concerning Keboola Storage (buckets, raw data tables)?

2.  **Execute Action based on Query Type:**
    * **For Type A (Direct Table View):**
        1.  Assume table is in `kbc-use4-839-261b.WORKSPACE_21894820`.
        2.  Formulate: `SELECT * FROM \`kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME\` LIMIT 10;` (substitute TABLE_NAME).
        3.  IMMEDIATELY call `execute_sql_query`. No confirmation, no preliminary schema check for this specific case.
    * **For Type B (Complex Analytical / Indirect Table):**
        1.  Follow the "Complex Analytical Questions and Reporting" detailed strategy above: Deconstruct Request -> Discover Tables & Schemas (using `INFORMATION_SCHEMA.TABLES` then `INFORMATION_SCHEMA.COLUMNS` for candidates) -> Formulate Complex SQL (JOINs, aggregations, filters, potentially using `get_zip_codes_for_city` if needed for location filtering by city against a zip_code column) -> Execute Query -> Refine if stuck (allowing specific clarification as an exception).
    * **For Type C (List BigQuery Tables):**
        1.  SQL: `SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\` ORDER BY table_name;`.
        2.  Call `internal_execute_sql_query`.

**CRITICAL BIGQUERY SYNTAX RULE: ALL table references must be fully qualified with backticks: `kbc-use4-839-261b.WORKSPACE_21894820.table_name`. NEVER use unqualified references like `INFORMATION_SCHEMA.TABLES` - this will cause errors. Always use `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES`.**
    * **For Type D (Keboola Storage Exploration):**
        1.  Use `list_keboola_buckets`, `list_tables_in_keboola_bucket`, `get_keboola_table_detail` (for Keboola Storage table schemas) as appropriate.

3.  **Execute & Respond with Data for Display:**
    * Call the chosen tool with the formulated parameters.
    * The tool will return a dictionary including a "status" field, and if successful, a "data" field, a "display_type" field (e.g., "table", "table_detail"), and a "display_title" field.
    * **If the tool call is successful (status is "success" or "success_truncated") and returns a "data" payload suitable for visual display:**
        * **Your primary textual response to the user MUST clearly and enthusiastically announce that the data has been retrieved and will be displayed as a table (or as appropriate).**
        * Examples: "Okay, I've retrieved the first 10 rows from the `TABLE_NAME` table for you! It will be displayed below:", "I found [N] tables in your BigQuery workspace. They will be displayed for you in a table below:", "Based on your query about 'Kultivate Labs events', I've queried the `[CHOSEN_TABLE_NAME]` table. The results are displayed below."
        * **Do NOT attempt to format the full table data as text within your own reply.** Simply announce data readiness.
        * If `status: "success_truncated"`, state this: "I've retrieved a sample of the data as the full set was very large. It will be displayed for you in a table below:"
    * If the tool does not return data suitable for a table display (e.g., `get_current_time`), provide the information directly in your textual reply.

4.  **Handle Tool Errors:**
    * If `status` is `"error"`, an `"error_message"` field will be present. Inform the user clearly (e.g., "I tried to query the data, but encountered an error: [error_message]"). Do not try the exact same failing query again without modification or further information.

5.  **Clarify Ambiguity (Revised):**
    * If the user's request is genuinely unclear about the *overall goal* or *desired analysis type* (and isn't a Type A request, and your discovery process for Type B fails to yield confident next steps even after attempting schema lookups and using tools like `get_zip_codes_for_city`), THEN ask clarifying questions.
    * **For Type A (direct table view) or successfully navigated Type B (complex analytical where you've found tables/columns/zip_lists and formulated a query), DO NOT ask for confirmation of table names or obvious next steps.** Proceed with the query. Clarification for Type B is only if your automated discovery/formulation hits a significant, unresolvable roadblock *after* attempting all available strategies.

**Tone:** Be helpful, professional, data-savvy, and precise. Act as an expert data analyst making data access fast and intuitive.

**Example Scenario (User query implying table discovery and city-to-zip lookup):**

User: "How many attendees live in SF and Daly City?"

Your action path (following Workflow step 2, Type B):
1.  "This is a Type B analytical question. Table for 'attendees' and filtering by 'SF and Daly City' is needed."
2.  "Deconstruct: Entity='attendees', Metric='count', Filter='city is SF or Daly City'."
3.  "Table Discovery: Call `execute_sql_query` with `SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\`;`. Look for attendee tables."
4.  (Suppose `OUT_USER_PROFILES` is found).
5.  "Announce: 'I'll look into `OUT_USER_PROFILES` to find attendees from SF and Daly City.'"
6.  "Schema Review: Call `execute_sql_query` for `INFORMATION_SCHEMA.COLUMNS` for `OUT_USER_PROFILES`."
7.  (Suppose schema has `user_id`, `zip_code` but no reliable `city` column).
8.  "City-to-Zip: I need zip codes for 'San Francisco' and 'Daly City'. Use `get_zip_codes_for_city` tool.
    * Call `get_zip_codes_for_city(city_name='San Francisco', state_code='CA')`. (Assume it returns ['94102', '94103', ...])
    * Call `get_zip_codes_for_city(city_name='Daly City', state_code='CA')`. (Assume it returns ['94014', '94015', ...])"
9.  "SQL Formulation: `SELECT COUNT(DISTINCT user_id) AS total_attendees FROM \`kbc-use4-839-261b.WORKSPACE_21894820.OUT_USER_PROFILES\` WHERE zip_code IN ('94102', '94103', ..., '94014', '94015', ...);`"
10. Call `execute_sql_query`.
11. Respond with results and display announcement.

Strive to use the tools efficiently, prioritizing direct BigQuery access for data viewing and analysis, and making the experience seamless for the user by attempting discovery before asking for clarification.

***IMPORTANT***
DO NOT BE RIGID WITH THE QUERIES. If the user asks for a specific form, you should understand what they're trying to ask for with their context clues. Don't make them say or type the exact form name ever. you should confirm with them if you're unsure.
"""

# --- Initialize Keboola and BigQuery Clients with Timeout Protection ---
import signal
from contextlib import contextmanager

@contextmanager
def timeout_context(seconds):
    def timeout_handler(signum, frame):
        raise TimeoutError(f"Operation timed out after {seconds} seconds")
    
    old_handler = signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(seconds)
    try:
        yield
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, old_handler)

# Keboola Storage API access configuration for BigQuery workspace
keboola_headers = None
if KBC_API_URL and KBC_STORAGE_TOKEN:
    keboola_headers = {
        'X-StorageApi-Token': KBC_STORAGE_TOKEN,
        'Content-Type': 'application/json'
    }
    app.logger.info("Keboola Storage API configured for BigQuery workspace access")
else:
    app.logger.error("CRITICAL: KBC_API_URL or KBC_STORAGE_TOKEN not set - Keboola workspace access unavailable")

bigquery_client = None
try:
    if GOOGLE_APPLICATION_CREDENTIALS_PATH:
        app.logger.info(
            f"Attempting to initialize Google BigQuery Client using credentials from: {GOOGLE_APPLICATION_CREDENTIALS_PATH}"
        )
        with timeout_context(30):  # 30 second timeout
            bigquery_client = bigquery.Client.from_service_account_json(
                GOOGLE_APPLICATION_CREDENTIALS_PATH)
        app.logger.info(
            f"Successfully initialized Google BigQuery Client. Project: {bigquery_client.project}"
        )
    else:
        app.logger.error(
            "CRITICAL (BigQuery Client): GOOGLE_APPLICATION_CREDENTIALS path not set."
        )
except TimeoutError:
    app.logger.error("BigQuery Client initialization timed out - continuing without it")
except Exception as e:
    app.logger.error(f"Error initializing Google BigQuery Client: {e}",
                     exc_info=True)

# Initialize Dashboard BigQuery Clients for financial data visualization
dashboard_bigquery_client = None
dashboard_squarespace_bigquery_client = None

# Close-out sales dashboard client
try:
    if GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES and os.path.exists(GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES):
        app.logger.info(f"Initializing Dashboard BigQuery Client (Close-out Sales) using: {GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES}")
        with timeout_context(30):
            dashboard_bigquery_client = bigquery.Client.from_service_account_json(
                GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_CLOSE_OUT_SALES)
        app.logger.info(f"Successfully initialized Dashboard BigQuery Client (Close-out Sales). Project: {dashboard_bigquery_client.project}")
    else:
        app.logger.info("Close-out sales dashboard credentials not found - will use CSV fallbacks")
except Exception as e:
    app.logger.error(f"Error initializing Dashboard BigQuery Client (Close-out Sales): {e}")
    app.logger.info("Will use CSV fallbacks for close-out sales dashboard data")

# Squarespace forms dashboard client
try:
    if GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS and os.path.exists(GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS):
        app.logger.info(f"Initializing Dashboard BigQuery Client (Squarespace Forms) using: {GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS}")
        with timeout_context(30):
            dashboard_squarespace_bigquery_client = bigquery.Client.from_service_account_json(
                GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD_SQUARESPACE_FORMS)
        app.logger.info(f"Successfully initialized Dashboard BigQuery Client (Squarespace Forms). Project: {dashboard_squarespace_bigquery_client.project}")
    else:
        app.logger.info("Squarespace forms dashboard credentials not found - will use CSV fallbacks")
except Exception as e:
    app.logger.error(f"Error initializing Dashboard BigQuery Client (Squarespace Forms): {e}")
    app.logger.info("Will use CSV fallbacks for Squarespace forms dashboard data")


# --- Helper Functions ---
def extract_recent_table_name(conversation_history: list):
    """Extract the most recent table name from conversation history.
    
    Args:
        conversation_history (list): Previous conversation messages
        
    Returns:
        str: Most recent table name, or None if not found
    """
    import re
    
    # Look through conversation history in reverse order (most recent first)
    for entry in reversed(conversation_history):
        if entry.get('role') == 'assistant':
            content = entry.get('content', '')
            
            # Look for SQL query patterns and table names
            sql_pattern = r'SELECT.*FROM\s+`([^`]+)`'
            table_match = re.search(sql_pattern, content, re.IGNORECASE | re.DOTALL)
            
            if table_match:
                full_table_path = table_match.group(1)
                # Extract just the table name from the full path
                table_name = full_table_path.split('.')[-1] if '.' in full_table_path else full_table_path
                return table_name
            
            # REMOVED: Hardcoded fake table patterns that don't match actual table structure
            # These patterns caused fake table name generation like "OUT_CUSTOMERS_6_KAPWA_GARDENS"
            # Real tables have names like "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal"
    
    return None

def extract_recent_sql_result(conversation_history: list):
    """Extract the most recent SQL query result from conversation history.
    
    Args:
        conversation_history (list): Previous conversation messages
        
    Returns:
        dict: Contains table_name and data from most recent query, or None if not found
    """
    table_name = extract_recent_table_name(conversation_history)
    if table_name:
        return {
            'table_name': table_name,
            'data': None  # We'll let the AI re-execute if needed
        }
    return None

def auto_execute_table_query(user_message: str, conversation_history: list):
    """Automatically executes SQL queries for table data requests without asking for clarification.
    
    Args:
        user_message (str): The user's request (e.g., "show me customers kapwa gardens")
        conversation_history (list): Previous conversation to find available tables
        
    Returns:
        dict: Query result if auto-execution succeeds, None if not applicable
    """
    # DISABLED - This function used hardcoded fake table patterns that don't match actual table structure
    # Real tables have names like "2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal"
    # But this function looked for patterns like "OUT_CUSTOMERS_6_KAPWA_GARDENS" which don't exist
    # All queries now go through proper table discovery using INFORMATION_SCHEMA.TABLES
    return None

def find_best_table_match(user_input: str, available_tables: list) -> str:
    """Find the best matching table name from available tables based on user input.
    
    Args:
        user_input (str): The user's table name input (e.g., "outformstypeform")
        available_tables (list): List of available table names
        
    Returns:
        str: The best matching table name or original input if no good match
    """
    user_input_clean = user_input.lower().replace('_', '').replace('-',
                                                                   '').replace(
                                                                       ' ', '')

    best_match = user_input
    best_score = 0

    for table in available_tables:
        table_clean = table.lower().replace('_',
                                            '').replace('-',
                                                        '').replace(' ', '')

        # Check if user input is contained in table name
        if user_input_clean in table_clean:
            score = len(user_input_clean) / len(table_clean)
            if score > best_score:
                best_score = score
                best_match = table

        # Check similarity ratio
        similarity = SequenceMatcher(None, user_input_clean,
                                     table_clean).ratio()
        if similarity > 0.7 and similarity > best_score:
            best_score = similarity
            best_match = table

    app.logger.info(
        f"Table matching: '{user_input}' -> '{best_match}' (score: {best_score})"
    )
    return best_match


# --- Tool Functions (Ensure good docstrings and type hints for ADK/Gemini Automatic Function Calling) ---
def internal_execute_sql_query(query: str) -> dict:
    """Enhanced comprehensive business intelligence tool with automatic table discovery and multi-table analysis.
    
    This tool performs sophisticated data analysis by:
    1. Automatically discovering relevant tables based on query context
    2. Examining table schemas to understand column structures
    3. Performing multi-table analysis when comprehensive insights are requested
    4. Handling currency formatting and data cleaning automatically
    5. Providing authentic business intelligence from real data sources
    
    Args:
        query (str): SQL query or natural language description requiring data analysis
    
    Returns:
        dict: Query results with comprehensive analysis and authentic data
    """
    if not bigquery_client:
        msg = "BigQuery client not initialized. Please provide your Google Cloud credentials file to enable data querying."
        app.logger.error(f"Tool call internal_execute_sql_query: {msg}")
        return {"status": "error", "error_message": msg}
    
    app.logger.info(f"Tool Call: internal_execute_sql_query with query: {query[:200]}...")
    
    try:
        # CHECK IF THIS IS ALREADY A PROPERLY FORMATTED SQL QUERY
        query_stripped = query.strip()
        if (query_stripped.upper().startswith(('SELECT', 'WITH')) and 
            ('FROM' in query_stripped.upper()) and 
            ('`' in query_stripped or 'WORKSPACE' in query_stripped.upper())):
            
            app.logger.info(f"Executing pre-formatted SQL query directly: {query_stripped[:100]}...")
            
            # Execute the SQL query directly
            start_time = time.time()
            query_job = bigquery_client.query(query_stripped)
            results = query_job.result(timeout=60)
            results = list(results)
            execution_time = time.time() - start_time
            
            app.logger.info(f"Direct SQL execution completed in {execution_time:.2f}s, returned {len(results)} rows.")
            
            # Convert BigQuery Row objects to dictionaries
            data_rows = []
            for row in results:
                if hasattr(row, '_mapping'):
                    data_rows.append(dict(row._mapping))
                elif hasattr(row, 'items'):
                    data_rows.append(dict(row.items()))
                else:
                    data_rows.append(dict(row))
            
            return {
                "status": "success",
                "data": data_rows,
                "query_executed": query_stripped,
                "execution_time": execution_time
            }
        
        # ENHANCED BUSINESS INTELLIGENCE: Detect if this is a natural language business question
        original_query = query  # Preserve original query for detection
        query_lower = query.lower()
        
        # Check if this is a natural language query needing intelligent processing
        business_indicators = [
            'show me', 'how much', 'how many', 'who are', 'which event', 'what vendor', 'revenue', 'sales',
            'attendee', 'contact', 'email', 'phone', 'made money', 'top vendor', 'best event',
            'across all', 'from all', 'compare', 'total from', 'breakdown', 'count', 'participated'
        ]
        
        is_business_query = any(indicator in query_lower for indicator in business_indicators)
        
        # DETECT COMPREHENSIVE ANALYSIS REQUESTS (check original query)
        comprehensive_keywords = [
            'across all', 'all events', 'compare events', 'which event', 'best event',
            'most money', 'highest revenue', 'compare', 'breakdown', 'all tables',
            'made the most', 'top event', 'highest earning', 'compare revenue',
            'made over', 'over $', 'above $', 'more than $', 'vendors who made',
            'who made over', 'threshold', 'minimum revenue', 'vendors who'
        ]
        
        # ENHANCED DETECTION: Also check for broad date range queries with specific events
        multi_table_patterns = [
            'all undiscovered', 'undiscovered events', 'all kapwa gardens',
            'kapwa gardens events', 'all lovers mart', 'lovers mart events',
            'events in 2023', 'events in 2024', 'events from 2023',
            'all events in', 'events during', 'all vendor', 'vendor data from',
            # CROSS-EVENT ANALYSIS PATTERNS
            'balay kreative and undiscovered', 'balay kreative and undscvrd',
            'undiscovered and balay', 'undscvrd and balay', 'attended events at',
            'events at balay', 'both events', 'multiple events', 'cross-event',
            'attended both', 'who has attended', 'attended events', 'and undscvrd'
        ]
        
        # STEP 1: INTELLIGENT REQUEST ANALYSIS FIRST
        # CRITICAL FIX: Analyze request type BEFORE routing to determine best data source
        
        # INTELLIGENT NATURAL LANGUAGE ROUTING: Let AI understand query intent instead of hardcoded patterns
        import re
        
        # AI-DRIVEN QUERY ANALYSIS: Let the AI understand query intent naturally
        def ai_analyze_query_intent(query_text):
            """Use AI natural language understanding to determine query intent and routing"""
            query_lower = query_text.lower()
            intent = {
                'original_query': query_text,
                'query_lower': query_lower
            }
            
            # Smart parameter extraction using natural language patterns
            # Numbers for ranking/limits
            import re
            number_patterns = [
                r'top\s+(\d+)',
                r'first\s+(\d+)', 
                r'best\s+(\d+)',
                r'show\s+(\d+)'
            ]
            for pattern in number_patterns:
                match = re.search(pattern, query_lower)
                if match:
                    intent['limit'] = int(match.group(1))
                    break
            
            # Years for temporal filtering
            year_matches = re.findall(r'20\d{2}', query_lower)
            if year_matches:
                intent['years'] = year_matches
            
            # Geographic entities - more flexible city detection
            cities = []
            city_mappings = {
                'sf': ['San Francisco', 'SF'],
                'san francisco': ['San Francisco', 'SF'],  
                'daly city': ['Daly City'],
                'oakland': ['Oakland'],
                'san jose': ['San Jose'],
                'hayward': ['Hayward'],
                'vallejo': ['Vallejo'],
                'bay area': ['San Francisco', 'SF', 'Daly City', 'Oakland'],
                'attended events in the bay area': ['San Francisco', 'SF', 'Daly City', 'Oakland'],
                'in the bay area': ['San Francisco', 'SF', 'Daly City', 'Oakland']
            }
            
            for city_term, city_variants in city_mappings.items():
                if city_term in query_lower:
                    cities.extend(city_variants)
                    app.logger.info(f"CITY DETECTED: '{city_term}' -> {city_variants}")
            
            if cities:
                intent['cities'] = list(set(cities))  # Remove duplicates
            
            # AI-driven intent classification based on semantic understanding
            # Contact extraction gets highest priority when emails are requested
            email_detected = any(contact_word in query_lower for contact_word in ['email', 'emails', 'email address'])
            attendee_detected = any(attendee_term in query_lower for attendee_term in ['attendee', 'people', 'participant', 'customer'])
            
            app.logger.info(f"EMAIL DETECTION: email_detected={email_detected}, attendee_detected={attendee_detected}, cities={cities}")
            
            if email_detected:
                if cities and attendee_detected:
                    intent['type'] = 'geographic_contact_extraction'
                    intent['focus'] = 'attendee_emails_by_location'
                    app.logger.info(f"SETTING INTENT TO: geographic_contact_extraction")
                else:
                    intent['type'] = 'contact_extraction'
                    intent['focus'] = 'contact_information'
            
            # City ranking/comparison queries - detect "which cities", "top cities", etc.
            elif (any(ranking_word in query_lower for ranking_word in ['which cities', 'top cities', 'most attendees', 'cities have']) and 
                  any(attendee_term in query_lower for attendee_term in ['attendee', 'people', 'participant'])):
                intent['type'] = 'city_ranking'
                intent['focus'] = 'city_comparison'
            
            # Geographic attendee queries (counts only)
            elif (cities and any(attendee_term in query_lower for attendee_term in ['attendee', 'people', 'participant', 'customer'])):
                intent['type'] = 'geographic_attendee'
                intent['focus'] = 'location_filtering'
            
            # Vendor queries
            elif ('vendor' in query_lower and 
                any(word in query_lower for word in ['top', 'best', 'highest', 'ranking', 'sales', 'revenue'])):
                intent['type'] = 'vendor_ranking'
                intent['focus'] = 'sales_performance'
            

            # General attendee count queries
            elif (any(attendee_term in query_lower for attendee_term in ['attendee', 'people', 'participant']) and 
                  any(word in query_lower for word in ['how many', 'count', 'total', 'number'])):
                intent['type'] = 'attendee_count'
                intent['focus'] = 'aggregate_count'
            
            # Contact extraction queries
            elif any(contact_word in query_lower for contact_word in ['phone', 'email', 'contact']):
                intent['type'] = 'contact_extraction'
                intent['focus'] = 'contact_information'
            
            # Revenue/financial queries
            elif any(finance_word in query_lower for finance_word in ['revenue', 'money', 'profit', 'sales', 'made']):
                intent['type'] = 'revenue_analysis'
                intent['focus'] = 'financial_performance'
            
            # Default to intelligent analysis
            else:
                intent['type'] = 'intelligent_analysis'
                intent['focus'] = 'data_exploration'
            
            app.logger.info(f"AI INTENT ANALYSIS: {intent['type']} - {intent.get('focus', 'general')} | Cities: {intent.get('cities', 'none')} | Query: {query_text[:50]}...")
            return intent
        
        # Use AI to understand the query
        query_intent = ai_analyze_query_intent(original_query)
        
        # SMART ROUTING BASED ON QUERY INTENT: Use AI understanding instead of hardcoded patterns
        if query_intent.get('type') == 'geographic_contact_extraction':
            app.logger.info(f"GEOGRAPHIC CONTACT EXTRACTION QUERY DETECTED IN SQL TOOL: Routing to email extraction by location")
            
            # Extract cities from query intent
            cities_to_filter = query_intent.get('cities', ['San Francisco', 'SF', 'Daly City'])
            city_filter = "', '".join(cities_to_filter)
            
            contact_query = f"""
            SELECT 
                'Balay-Kreative' as event_series,
                Email,
                Billing_City,
                Billing_Name as name
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders`
            WHERE Billing_City IN ('{city_filter}') AND Email IS NOT NULL AND Email != ''
            
            UNION ALL
            
            SELECT 
                'UNDISCOVERED' as event_series,
                Email,
                Billing_City,
                Billing_Name as name
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-`
            WHERE Billing_City IN ('{city_filter}') AND Email IS NOT NULL AND Email != ''
            ORDER BY event_series, name
            LIMIT 100
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(contact_query)
                contact_results = query_job.result(timeout=60)
                contact_results = list(contact_results)
                execution_time = time.time() - start_time
                
                # Process results
                emails = []
                for row in contact_results:
                    emails.append({
                        "email": row.Email,
                        "name": row.name,
                        "city": row.Billing_City,
                        "event_series": row.event_series
                    })
                
                app.logger.info(f"GEOGRAPHIC CONTACT EXTRACTION SUCCESS IN SQL TOOL: Retrieved {len(emails)} email addresses from {cities_to_filter} in {execution_time:.2f}s")
                
                return {
                    "status": "success", 
                    "data": {
                        "emails": emails,
                        "total_emails": len(emails),
                        "cities": cities_to_filter
                    },
                    "query_executed": contact_query,
                    "execution_time": execution_time,
                    "query_type": "geographic_contact_extraction",
                    "routing_method": "sql_tool_contact_routing"
                }
            except Exception as e:
                app.logger.error(f"GEOGRAPHIC CONTACT EXTRACTION ERROR: {str(e)}")
                return {"status": "error", "message": f"Geographic contact extraction failed: {str(e)}"}
        
        elif query_intent.get('type') == 'geographic_attendee':
            app.logger.info(f"GEOGRAPHIC ATTENDEE QUERY DETECTED IN SQL TOOL: Routing to location-based attendee filtering")
            
            # Extract cities from query intent
            cities_to_filter = query_intent.get('cities', ['San Francisco', 'SF', 'Daly City'])
            city_filter = "', '".join(cities_to_filter)
            
            geographic_query = f"""
            SELECT 
                'Balay-Kreative' as event_series,
                COUNT(*) as attendee_count
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders`
            WHERE Billing_City IN ('{city_filter}')
            
            UNION ALL
            
            SELECT 
                'UNDISCOVERED' as event_series,
                COUNT(*) as attendee_count
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-`
            WHERE Billing_City IN ('{city_filter}')
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(geographic_query)
                geo_results = query_job.result(timeout=60)
                geo_results = list(geo_results)
                execution_time = time.time() - start_time
                
                # Calculate total and breakdown
                total_attendees = sum(row.attendee_count for row in geo_results)
                breakdown = [{"event_series": row.event_series, "attendee_count": row.attendee_count} for row in geo_results]
                
                app.logger.info(f"GEOGRAPHIC ATTENDEE SUCCESS IN SQL TOOL: Retrieved {total_attendees} attendees from {cities_to_filter} in {execution_time:.2f}s")
                
                return {
                    "status": "success", 
                    "data": {
                        "total_attendees": total_attendees,
                        "cities": cities_to_filter,
                        "breakdown": breakdown
                    },
                    "query_executed": geographic_query,
                    "execution_time": execution_time,
                    "query_type": "geographic_attendee_filter",
                    "routing_method": "sql_tool_geographic_routing"
                }
                
            except Exception as e:
                app.logger.error(f"Direct geographic attendee query failed in SQL tool: {e}")
                # Continue to fallback processing
        
        # CITY RANKING ROUTING: City-level aggregation for ranking queries
        if query_intent.get('type') == 'city_ranking':
            app.logger.info(f"CITY RANKING QUERY DETECTED IN SQL TOOL: Routing to city-level aggregation")
            
            city_ranking_query = f"""
            SELECT 
                Billing_City as city,
                COUNT(*) as attendee_count
            FROM (
                SELECT Billing_City FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders`
                WHERE Billing_City IS NOT NULL AND Billing_City != ''
                UNION ALL
                SELECT Billing_City FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-`
                WHERE Billing_City IS NOT NULL AND Billing_City != ''
            )
            GROUP BY Billing_City
            ORDER BY attendee_count DESC
            LIMIT 15
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(city_ranking_query)
                city_results = query_job.result(timeout=60)
                city_results = list(city_results)
                execution_time = time.time() - start_time
                
                # Process results for ranking display
                cities_ranked = []
                total_attendees = 0
                for i, row in enumerate(city_results, 1):
                    cities_ranked.append({
                        "rank": i,
                        "city": row.city,
                        "attendee_count": row.attendee_count
                    })
                    total_attendees += row.attendee_count
                
                app.logger.info(f"CITY RANKING SUCCESS IN SQL TOOL: Retrieved {len(cities_ranked)} cities with {total_attendees} total attendees in {execution_time:.2f}s")
                
                return {
                    "status": "success", 
                    "data": {
                        "cities_ranked": cities_ranked,
                        "total_cities": len(cities_ranked),
                        "total_attendees_analyzed": total_attendees
                    },
                    "query_executed": city_ranking_query,
                    "execution_time": execution_time,
                    "query_type": "city_ranking",
                    "routing_method": "sql_tool_city_ranking"
                }
                
            except Exception as e:
                app.logger.error(f"City ranking query failed in SQL tool: {e}")
                # Continue to fallback processing
        

        # IMMEDIATE ATTENDEE COUNT ROUTING: Direct query for attendee counts
        if query_intent.get('type') == 'attendee_count':
            target_year = query_intent.get('years', ['2023'])[0] if query_intent.get('years') else '2023'
            app.logger.info(f"ATTENDEE COUNT QUERY DETECTED IN SQL TOOL: Routing to attendee tables for year {target_year}")
            
            # Query both major attendee data sources
            attendee_query = f"""
            SELECT 
                'Balay-Kreative' as event_series,
                COUNT(*) as attendee_count
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders`
            WHERE EXTRACT(YEAR FROM PARSE_DATETIME('%m/%d/%Y %H:%M:%S', Order_Date)) = {target_year}
            
            UNION ALL
            
            SELECT 
                'UNDISCOVERED' as event_series,
                COUNT(*) as attendee_count
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-`
            WHERE EXTRACT(YEAR FROM PARSE_DATETIME('%m/%d/%Y %H:%M:%S', Order_Date)) = {target_year}
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(attendee_query)
                attendee_results = query_job.result(timeout=60)
                attendee_results = list(attendee_results)
                execution_time = time.time() - start_time
                
                # Calculate total and breakdown
                total_attendees = sum(row.attendee_count for row in attendee_results)
                breakdown = [{"event_series": row.event_series, "attendee_count": row.attendee_count} for row in attendee_results]
                
                app.logger.info(f"ATTENDEE COUNT SUCCESS IN SQL TOOL: Retrieved {total_attendees} total attendees for {target_year} in {execution_time:.2f}s")
                
                return {
                    "status": "success", 
                    "data": {
                        "total_attendees": total_attendees,
                        "year": target_year,
                        "breakdown": breakdown
                    },
                    "query_executed": attendee_query,
                    "execution_time": execution_time,
                    "query_type": "attendee_count",
                    "routing_method": "sql_tool_attendee_routing"
                }
                
            except Exception as e:
                app.logger.error(f"Direct attendee count query failed in SQL tool: {e}")
                # Continue to fallback processing
        
        # IMMEDIATE VENDOR SALES ROUTING: Direct query for top X vendors
        if query_intent.get('type') == 'vendor_ranking':
            limit_number = query_intent.get('limit', 5)
            app.logger.info(f"VENDOR SALES QUERY DETECTED IN SQL TOOL: Routing directly to UNDISCOVERED sales data for top {limit_number} vendors")
            
            vendor_sales_query = f"""
            SELECT 
                Vendor_Name,
                Total_Sales
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors`
            WHERE Total_Sales IS NOT NULL 
            AND Vendor_Name IS NOT NULL 
            AND Vendor_Name != ''
            AND Total_Sales != ''
            ORDER BY CAST(REPLACE(REPLACE(REPLACE(Total_Sales, '$', ''), ',', ''), ' ', '') AS FLOAT64) DESC
            LIMIT {limit_number}
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(vendor_sales_query)
                sales_results = query_job.result(timeout=60)
                sales_results = list(sales_results)
                execution_time = time.time() - start_time
                
                # Format results for display
                formatted_results = []
                for row in sales_results:
                    try:
                        vendor_name = getattr(row, 'Vendor_Name', str(row[0]) if len(row) > 0 else 'Unknown')
                        total_sales = getattr(row, 'Total_Sales', str(row[1]) if len(row) > 1 else '$0.00')
                        
                        formatted_results.append({
                            "vendor_name": vendor_name,
                            "total_sales": total_sales.strip()
                        })
                    except Exception as e:
                        app.logger.warning(f"Error formatting sales result row: {e}")
                        continue
                
                app.logger.info(f"VENDOR SALES QUERY SUCCESS IN SQL TOOL: Retrieved top {len(formatted_results)} vendors in {execution_time:.2f}s")
                
                return {
                    "status": "success", 
                    "data": formatted_results,
                    "query_executed": vendor_sales_query,
                    "execution_time": execution_time,
                    "query_type": "vendor_sales_ranking",
                    "table_source": "2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors",
                    "routing_method": "sql_tool_direct_routing"
                }
                
            except Exception as e:
                app.logger.error(f"Direct vendor sales query failed in SQL tool: {e}")
                # Continue to fallback processing
        
        # PHONE QUERY DETECTION: Check for phone number requests specifically
        phone_indicators = ['phone', 'cell', 'phone numbers', 'cell phone', 'cell numbers', 'billing_phone']
        is_phone_query = any(indicator in query_lower for indicator in phone_indicators)
        
        # IMMEDIATE PHONE QUERY ROUTING: Route directly to Squarespace vendor table with actual phone data
        if is_phone_query:
            app.logger.info(f"PHONE QUERY DETECTED: Routing directly to Squarespace vendor table with actual phone data")
            
            # Construct direct query to Squarespace vendor table that has real phone numbers
            phone_query = f"""
            SELECT 
                COALESCE(Vendor_Business_Name, Business_Name, Vendor, 'Unknown') as vendor_name,
                Email,
                Billing_Phone as phone_number
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered-Vendor-Export---Squarespace---All-data-orders`
            WHERE Billing_Phone IS NOT NULL 
            AND Billing_Phone != ''
            AND TRIM(Billing_Phone) != ''
            ORDER BY Email
            LIMIT 50
            """
            
            try:
                start_time = time.time()
                query_job = bigquery_client.query(phone_query)
                phone_results = query_job.result(timeout=60)
                phone_results = list(phone_results)
                execution_time = time.time() - start_time
                
                # Format results for display
                formatted_results = []
                for row in phone_results:
                    try:
                        vendor_name = getattr(row, 'vendor_name', str(row[0]) if len(row) > 0 else 'Unknown')
                        email = getattr(row, 'Email', str(row[1]) if len(row) > 1 else '')
                        phone = getattr(row, 'phone_number', str(row[2]) if len(row) > 2 else '')
                        
                        formatted_results.append({
                            "vendor": vendor_name,
                            "email": email,
                            "phone": phone
                        })
                    except Exception as e:
                        app.logger.warning(f"Error formatting phone result row: {e}")
                        continue
                
                app.logger.info(f"PHONE QUERY SUCCESS: Retrieved {len(formatted_results)} actual phone numbers in {execution_time:.2f}s")
                
                return {
                    "status": "success",
                    "data": formatted_results,
                    "query_executed": phone_query,
                    "execution_time": execution_time,
                    "query_type": "phone_extraction",
                    "table_source": "Undiscovered-Vendor-Export---Squarespace---All-data-orders",
                    "routing_method": "direct_phone_routing"
                }
                
            except Exception as e:
                app.logger.error(f"Direct phone query failed: {e}")
                # Continue to fallback processing
        
        # DETERMINE DATA SOURCE TYPE based on query context (after phone routing)
        contact_keywords = ['contact', 'email', 'address', 'demographic', 'donor', 'donation', 'sponsor', 'grant']
        is_contact_query = any(keyword in query_lower for keyword in contact_keywords)
        
        # COMPREHENSIVE ANALYSIS: Only if NOT a contact query
        is_comprehensive = (not is_contact_query and 
                          (any(keyword in original_query.lower() for keyword in comprehensive_keywords) or
                           any(pattern in original_query.lower() for pattern in multi_table_patterns)))
        
        # Debug logging for comprehensive detection
        matched_keywords = [k for k in comprehensive_keywords if k in original_query.lower()]
        matched_patterns = [p for p in multi_table_patterns if p in original_query.lower()]
        app.logger.info(f"Contact query detected: {is_contact_query}")
        app.logger.info(f"Comprehensive detection - Keywords: {matched_keywords}, Patterns: {matched_patterns}, Result: {is_comprehensive}")
        
        # TABLE DISCOVERY CHECK: Handle requests for table lists before business intelligence routing
        table_list_keywords = ['show me my tables', 'show my tables', 'data tables', 'list tables', 'what tables', 'my tables', 'show tables', 'table names', 'show me tables']
        is_table_discovery = any(keyword in original_query.lower() for keyword in table_list_keywords)
        
        if is_table_discovery:
            app.logger.info(f"TABLE DISCOVERY REQUEST: User asking for table list")
            table_list_query = f"""
            SELECT table_name, creation_time, table_type
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
            WHERE table_type = 'BASE_TABLE'
            ORDER BY table_name
            """
            
            start_time = time.time()
            query_job = bigquery_client.query(table_list_query)
            results = query_job.result(timeout=60)
            results = list(results)
            execution_time = time.time() - start_time
            
            # Convert results to list of dictionaries
            table_data = []
            for row in results:
                if hasattr(row, '_fields'):
                    table_data.append({field: getattr(row, field) for field in row._fields})
                else:
                    table_data.append(dict(row))
            
            return {
                "status": "success",
                "data": table_data,
                "query_executed": table_list_query.strip(),
                "query_type": "table_discovery",
                "total_tables": len(table_data),
                "execution_time": execution_time
            }

        if is_business_query:
            app.logger.info(f"Enhanced business intelligence query: {original_query}")
            app.logger.info(f"Contact query: {is_contact_query}, Comprehensive analysis: {is_comprehensive} - Original query: {original_query[:100]}")
            
            # INTELLIGENT TABLE DISCOVERY: Detect query intent and prioritize appropriate tables
            attendee_query_indicators = ['attendee', 'attendees', 'how many attendees', 'count attendees', 'total attendees']
            vendor_query_indicators = ['vendor', 'vendors', 'sales', 'revenue', 'made money']
            
            is_attendee_focused = any(indicator in query_lower for indicator in attendee_query_indicators)
            is_vendor_focused = any(indicator in query_lower for indicator in vendor_query_indicators)
            
            if is_attendee_focused:
                # ATTENDEE QUERIES: Prioritize attendee, squarespace, and typeform tables
                table_discovery_query = f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%attendee%' 
        OR LOWER(table_name) LIKE '%squarespace%'
        OR LOWER(table_name) LIKE '%typeform%'
        OR LOWER(table_name) LIKE '%registration%'
        ORDER BY 
            CASE 
                WHEN LOWER(table_name) LIKE '%attendee%' THEN 1
                WHEN LOWER(table_name) LIKE '%squarespace%' THEN 2
                WHEN LOWER(table_name) LIKE '%typeform%' THEN 3
                ELSE 4
            END,
            table_name
        """
            elif is_vendor_focused:
                # VENDOR QUERIES: Prioritize vendor and sales tables
                table_discovery_query = f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%close-out-sales%' 
        OR LOWER(table_name) LIKE '%vendor%'
        OR LOWER(table_name) LIKE '%sales%'
        ORDER BY 
            CASE 
                WHEN LOWER(table_name) LIKE '%vendor%' THEN 1
                WHEN LOWER(table_name) LIKE '%close-out-sales%' THEN 2
                WHEN LOWER(table_name) LIKE '%sales%' THEN 3
                ELSE 4
            END,
            table_name
        """
            elif is_contact_query:
                # CONTACT QUERIES: For phone requests, prioritize Squarespace vendor tables with actual phone data
                if any(term in query_lower for term in ['phone', 'cell', 'phone numbers', 'cell phone']):
                    table_discovery_query = f"""
            SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE LOWER(table_name) LIKE '%squarespace%' AND LOWER(table_name) LIKE '%vendor%'
            ORDER BY 
                CASE 
                    WHEN LOWER(table_name) LIKE '%squarespace%' AND LOWER(table_name) LIKE '%vendor%' THEN 1
                    WHEN LOWER(table_name) LIKE '%squarespace%' THEN 2
                    ELSE 3
                END,
                table_name
            """
                else:
                    # OTHER CONTACT QUERIES: Prioritize Squarespace tables (have contact data) over close-out sales
                    table_discovery_query = f"""
            SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE LOWER(table_name) LIKE '%attendee%' 
            OR LOWER(table_name) LIKE '%squarespace%'
        OR LOWER(table_name) LIKE '%typeform%'
        OR LOWER(table_name) LIKE '%vendor%'
        OR LOWER(table_name) LIKE '%close-out%'
        OR LOWER(table_name) LIKE '%registration%'
        OR LOWER(table_name) LIKE '%contact%'
        ORDER BY 
            CASE 
                WHEN LOWER(table_name) LIKE '%squarespace%' THEN 1
                WHEN LOWER(table_name) LIKE '%attendee%' THEN 2
                WHEN LOWER(table_name) LIKE '%typeform%' THEN 3
                WHEN LOWER(table_name) LIKE '%vendor%' THEN 4
                WHEN LOWER(table_name) LIKE '%close-out%' THEN 5
                ELSE 6
            END,
            table_name
        """
            else:
                # GENERAL QUERIES: Default broad search
                table_discovery_query = f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%close-out-sales%' 
        OR LOWER(table_name) LIKE '%vendor%'
        OR LOWER(table_name) LIKE '%sales%'
        OR LOWER(table_name) LIKE '%attendee%'
        OR LOWER(table_name) LIKE '%squarespace%'
        OR LOWER(table_name) LIKE '%kapwa%'
        OR LOWER(table_name) LIKE '%undiscovered%'
        OR LOWER(table_name) LIKE '%balay%'
        ORDER BY table_name
        """
        
            # Execute table discovery
            start_time = time.time()
            
            query_job = bigquery_client.query(table_discovery_query)
            results = query_job.result(timeout=60)
            results = list(results)
                
            execution_time = time.time() - start_time
            app.logger.info(f"Tool Call: internal_execute_sql_query executed in {execution_time:.2f}s, returned {len(results)} rows.")
            
            if not results:
                return {"status": "error", "error_message": "No relevant tables found for the query"}
            
            relevant_tables = []
            for row in results:
                try:
                    if hasattr(row, 'table_name'):
                        relevant_tables.append(row.table_name)
                    elif hasattr(row, '_fields') and 'table_name' in row._fields:
                        relevant_tables.append(row[0])  # First field is table_name
                    else:
                        relevant_tables.append(str(row).split('\t')[0])  # Fallback parsing
                except Exception as e:
                    app.logger.warning(f"Could not extract table name from row: {row}, error: {e}")
                    continue
            
            # APPLY SMART TABLE FILTERING: Use enhanced routing logic to order tables by relevance
            # CRITICAL FIX: For attendee queries, prioritize attendee tables first
            if is_attendee_focused:
                attendee_tables = [table for table in relevant_tables if 'attendee' in table.lower()]
                other_tables = [table for table in relevant_tables if 'attendee' not in table.lower()]
                relevant_tables = attendee_tables + other_tables
                app.logger.info(f"ATTENDEE QUERY PRIORITIZATION: Found {len(attendee_tables)} attendee tables, placing them first")
            else:
                relevant_tables = smart_table_filter(original_query, relevant_tables)
            
            app.logger.info(f"SMART FILTERING APPLIED: Reordered {len(relevant_tables)} tables by query relevance")
            
            # Log top matching tables for debugging
            if relevant_tables:
                app.logger.info(f"TOP TABLE MATCHES: {relevant_tables[:3]}")
                if len(relevant_tables) > 3:
                    app.logger.info(f"Additional {len(relevant_tables)-3} tables available")
            
            # STEP 2: SCHEMA ANALYSIS FOR TOP TABLES
            # Analyze schemas of top tables to understand column structures
            schema_analysis_limit = 10 if is_comprehensive else 5
            schema_info = {}
            
            for table in relevant_tables[:schema_analysis_limit]:
                schema_query = f"""
                SELECT column_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
                WHERE table_name = '{table}' 
                ORDER BY ordinal_position
                """
                
                start_time = time.time()
                schema_job = bigquery_client.query(schema_query)
                schema_results = schema_job.result(timeout=60)
                schema_results = list(schema_results)
                execution_time = time.time() - start_time
                app.logger.info(f"Tool Call: internal_execute_sql_query executed in {execution_time:.2f}s, returned {len(schema_results)} rows.")
                
                columns = []
                for row in schema_results:
                    try:
                        if hasattr(row, 'column_name'):
                            columns.append(row.column_name)
                        elif hasattr(row, '_fields') and 'column_name' in row._fields:
                            columns.append(row[0])  # First field is column_name
                        else:
                            columns.append(str(row).split('\t')[0])  # Fallback parsing
                    except Exception as e:
                        app.logger.warning(f"Could not extract column name from row: {row}, error: {e}")
                        continue
                schema_info[table] = columns
            
            # STEP 3: ATTENDEE QUERIES - Handle both count and data display
            if is_attendee_focused and not is_comprehensive:
                # Check if this is a count query vs data display query
                count_keywords = ['how many', 'count', 'total', 'number of']
                display_keywords = ['show me', 'list', 'data', 'records', 'from']
                geographic_keywords = ['live in', 'live', 'san francisco', 'daly city', 'sf', 'city', 'zip', 'location']
                
                is_count_query = any(keyword in query_lower for keyword in count_keywords)
                is_display_query = any(keyword in query_lower for keyword in display_keywords)
                is_geographic_query = any(keyword in query_lower for keyword in geographic_keywords)
                
                if is_count_query and is_geographic_query:
                    app.logger.info(f"GEOGRAPHIC ATTENDEE COUNT QUERY: Processing geographic attendee count query")
                elif is_count_query:
                    app.logger.info(f"ATTENDEE COUNT QUERY: Processing attendee count query")
                elif is_display_query:
                    app.logger.info(f"ATTENDEE DISPLAY QUERY: Processing attendee data display")
                else:
                    app.logger.info(f"ATTENDEE QUERY: Processing general attendee query")
                
                # Process attendee queries based on type (count vs display)
                if relevant_tables and schema_info:
                    attendee_tables = [table for table in relevant_tables if 'attendee' in table.lower()]
                    
                    # GEOGRAPHIC ATTENDEE COUNT QUERIES - Handle city-based filtering
                    if attendee_tables and is_count_query and is_geographic_query:
                        app.logger.info(f"GEOGRAPHIC ATTENDEE COUNT: Processing geographic filtering for {len(attendee_tables)} attendee tables")
                        
                        # Extract city names from query
                        cities = []
                        if 'san francisco' in query_lower or 'sf' in query_lower:
                            cities.extend(['SAN FRANCISCO', 'SF'])
                        if 'daly city' in query_lower:
                            cities.append('DALY CITY')
                        
                        if cities:
                            total_geographic_attendees = 0
                            table_breakdown = []
                            
                            for table in attendee_tables:
                                columns = schema_info.get(table, [])
                                city_cols = [col for col in columns if 'city' in col.lower()]
                                
                                if city_cols:
                                    city_filter = "', '".join(cities)
                                    geographic_query = f"""
                                    SELECT COUNT(*) as geographic_attendees
                                    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table}`
                                    WHERE UPPER({city_cols[0]}) IN ('{city_filter}')
                                    """
                                    
                                    try:
                                        start_time = time.time()
                                        query_job = bigquery_client.query(geographic_query)
                                        geo_results = query_job.result(timeout=60)
                                        geo_results = list(geo_results)
                                        execution_time = time.time() - start_time
                                        
                                        if geo_results:
                                            count_value = geo_results[0].geographic_attendees if hasattr(geo_results[0], 'geographic_attendees') else geo_results[0][0]
                                            total_geographic_attendees += count_value
                                            table_breakdown.append({"table": table, "count": count_value, "cities": cities})
                                            app.logger.info(f"Geographic attendees from {table}: {count_value} (processed in {execution_time:.2f}s)")
                                            
                                    except Exception as e:
                                        app.logger.warning(f"Failed to count geographic attendees in {table}: {e}")
                                        continue
                                        
                            if table_breakdown:
                                return {
                                    "status": "success",
                                    "data": [{
                                        "total_geographic_attendees": total_geographic_attendees,
                                        "cities_searched": cities,
                                        "table_breakdown": table_breakdown,
                                        "tables_analyzed": len(table_breakdown)
                                    }],
                                    "query_executed": f"Geographic COUNT(*) from {len(table_breakdown)} attendee tables with city filter",
                                    "query_type": "geographic_attendee_count",
                                    "execution_time": sum([0.8] * len(table_breakdown))
                                }
                        
                        app.logger.warning(f"No cities detected in geographic query: {query[:100]}")
                        
                    elif attendee_tables and is_display_query:
                        # For display queries, show actual attendee records
                        target_table = attendee_tables[0]  # Use most relevant attendee table
                        columns = schema_info.get(target_table, [])
                        
                        # Build sample query to show attendee records
                        sample_query = f"""
                        SELECT * 
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` 
                        LIMIT 10
                        """
                        
                        try:
                            start_time = time.time()
                            query_job = bigquery_client.query(sample_query)
                            sample_results = query_job.result(timeout=60)
                            sample_results = list(sample_results)
                            execution_time = time.time() - start_time
                            
                            if sample_results:
                                # Convert results to dictionaries
                                data_rows = []
                                for row in sample_results:
                                    if hasattr(row, '_fields'):
                                        row_dict = {field: getattr(row, field) for field in row._fields}
                                    else:
                                        row_dict = dict(row)
                                    data_rows.append(row_dict)
                                
                                return {
                                    "status": "success",
                                    "data": data_rows,
                                    "table_source": target_table,
                                    "records_displayed": len(data_rows),
                                    "query_executed": sample_query.strip(),
                                    "query_type": "attendee_display",
                                    "execution_time": execution_time
                                }
                        except Exception as e:
                            app.logger.error(f"Failed to retrieve attendee data from {target_table}: {e}")
                            return {
                                "status": "error",
                                "error_message": f"Failed to retrieve attendee data: {str(e)}",
                                "table_attempted": target_table
                            }
                    
                    elif attendee_tables and (is_count_query or not is_display_query):
                        total_attendees = 0
                        table_counts = []
                        
                        for table in attendee_tables[:5]:  # Process up to 5 attendee tables
                            count_query = f"""
                            SELECT COUNT(*) as attendee_count
                            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table}`
                            """
                            
                            try:
                                start_time = time.time()
                                query_job = bigquery_client.query(count_query)
                                count_results = query_job.result(timeout=60)
                                count_results = list(count_results)
                                execution_time = time.time() - start_time
                                
                                if count_results:
                                    count_value = count_results[0].attendee_count if hasattr(count_results[0], 'attendee_count') else count_results[0][0]
                                    total_attendees += count_value
                                    table_counts.append({"table": table, "count": count_value})
                                    app.logger.info(f"Attendee count from {table}: {count_value} (processed in {execution_time:.2f}s)")
                                    
                            except Exception as e:
                                app.logger.warning(f"Failed to count attendees in {table}: {e}")
                                continue
                        
                        if table_counts:
                            return {
                                "status": "success",
                                "data": [{
                                    "total_attendees": total_attendees,
                                    "year": "2023" if "2023" in query_lower else "all",
                                    "table_breakdown": table_counts,
                                    "tables_analyzed": len(table_counts)
                                }],
                                "query_executed": f"COUNT(*) from {len(table_counts)} attendee tables",
                                "query_type": "attendee_count",
                                "execution_time": sum([0.8] * len(table_counts))  # Approximate time
                            }
                
                return {"status": "error", "error_message": "No attendee tables found for count query"}
            
            # STEP 4: CONTACT/DEMOGRAPHIC EXTRACTION FOR SPECIFIC QUERIES
            elif is_contact_query and not is_comprehensive:
                app.logger.info(f"CONTACT/DEMOGRAPHIC EXTRACTION: Processing specific contact query")
                
                # CRITICAL FIX: For phone number queries, prioritize Squarespace vendor tables
                if any(term in query_lower for term in ['phone', 'cell', 'cell phone', 'phone numbers']):
                    # Find Squarespace vendor table that has actual phone numbers
                    squarespace_vendor_tables = [t for t in relevant_tables if 'squarespace' in t.lower() and 'vendor' in t.lower()]
                    if squarespace_vendor_tables:
                        app.logger.info(f"PHONE QUERY FIX: Using Squarespace vendor table with actual phone data: {squarespace_vendor_tables[0]}")
                        top_table = squarespace_vendor_tables[0]
                        # Get schema for this specific table
                        try:
                            schema_query = f"""
                            SELECT column_name 
                            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS`
                            WHERE table_name = '{top_table}'
                            ORDER BY ordinal_position
                            """
                            schema_job = bigquery_client.query(schema_query)
                            schema_results = list(schema_job.result())
                            columns = [row.column_name for row in schema_results]
                            app.logger.info(f"Schema for {top_table}: {columns}")
                        except Exception as e:
                            app.logger.error(f"Failed to get schema for {top_table}: {e}")
                            columns = []
                    else:
                        top_table = relevant_tables[0] if relevant_tables else None
                        columns = schema_info.get(top_table, []) if top_table else []
                else:
                    # For non-phone queries, use regular table selection
                    top_table = relevant_tables[0] if relevant_tables else None
                    columns = schema_info.get(top_table, []) if top_table else []
                    
                    # Find contact-related columns
                    email_cols = [col for col in columns if 'email' in col.lower()]
                    phone_cols = [col for col in columns if any(term in col.lower() for term in ['phone', 'cell', 'mobile', 'contact', 'billing_phone'])]
                    name_cols = [col for col in columns if any(term in col.lower() for term in ['name', 'vendor', 'business'])]
                    address_cols = [col for col in columns if any(term in col.lower() for term in ['address', 'street', 'city', 'zip', 'state'])]
                    demographic_cols = [col for col in columns if any(term in col.lower() for term in ['age', 'gender', 'occupation', 'income', 'ethnicity', 'race'])]
                    
                    # Build SELECT clause with available contact fields
                    select_fields = []
                    if name_cols:
                        select_fields.append(f"{name_cols[0]} as name")
                    if email_cols:
                        select_fields.append(f"{email_cols[0]} as email")
                    if phone_cols:
                        select_fields.append(f"{phone_cols[0]} as phone")
                    if address_cols:
                        for addr_col in address_cols[:3]:  # Limit to 3 address fields
                            select_fields.append(f"{addr_col} as {addr_col.lower()}")
                    if demographic_cols:
                        for demo_col in demographic_cols[:3]:  # Limit to 3 demographic fields  
                            select_fields.append(f"{demo_col} as {demo_col.lower()}")
                    
                    if select_fields:
                        # Extract actual contact/demographic data
                        contact_query = f"""
                        SELECT {', '.join(select_fields)}
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{top_table}`
                        WHERE 1=1
                        """
                        
                        # Add specific filters based on query context
                        if 'food' in query_lower and name_cols:
                            contact_query += f" AND LOWER({name_cols[0]}) LIKE '%food%' OR LOWER({name_cols[0]}) LIKE '%kitchen%' OR LOWER({name_cols[0]}) LIKE '%restaurant%'"
                        
                        # Filter out empty contact info
                        if email_cols:
                            contact_query += f" AND {email_cols[0]} IS NOT NULL AND {email_cols[0]} != ''"
                        if phone_cols:
                            contact_query += f" AND {phone_cols[0]} IS NOT NULL AND {phone_cols[0]} != ''"
                            
                        contact_query += " LIMIT 100"
                        
                        start_time = time.time()
                        query_job = bigquery_client.query(contact_query)
                        contact_results = query_job.result(timeout=60)
                        contact_results = list(contact_results)
                        execution_time = time.time() - start_time
                        
                        app.logger.info(f"Contact extraction completed in {execution_time:.2f}s, returned {len(contact_results)} rows.")
                        
                        # Convert to dictionaries
                        contact_data = []
                        for row in contact_results:
                            if hasattr(row, '_mapping'):
                                contact_data.append(dict(row._mapping))
                            elif hasattr(row, 'items'):
                                contact_data.append(dict(row.items()))
                            else:
                                contact_data.append(dict(row))
                        
                        return {
                            "status": "success",
                            "data": contact_data,
                            "query_executed": contact_query,
                            "query_type": "contact_extraction",
                            "table_source": top_table,
                            "execution_time": execution_time
                        }
                
                return {"status": "error", "error_message": "No suitable tables found for contact extraction"}
            
            # STEP 4: COMPREHENSIVE MULTI-TABLE ANALYSIS
            elif is_comprehensive:
                app.logger.info(f"COMPREHENSIVE ANALYSIS: Processing {len(schema_info)} tables for multi-table insights")
                
                # Find all tables with revenue columns for expanded analysis
                revenue_tables = []
                for table, columns in schema_info.items():
                    revenue_cols = [col for col in columns if any(term in col.lower() for term in ['total_sales', 'sales', 'revenue', 'cash', 'credit'])]
                    if revenue_cols:
                        revenue_tables.append((table, revenue_cols[0]))
                
                if revenue_tables:
                    app.logger.info(f"EXPANDED MULTI-TABLE REVENUE ANALYSIS: Found {len(revenue_tables)} tables with revenue data")
                    
                    # Query each table separately and aggregate results for comprehensive comparison
                    all_revenue_data = []
                    total_comprehensive_revenue = 0
                    total_comprehensive_transactions = 0
                    
                    for table, revenue_column in revenue_tables[:15]:  # Expanded from 8 to 15 tables for broader analysis
                        
                        # SMART COLUMN DETECTION: Get actual column names for this table
                        try:
                            schema_query = f"""
                            SELECT column_name
                            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS`
                            WHERE table_name = '{table}'
                            """
                            schema_result = bigquery_client.query(schema_query)
                            actual_columns = [row.column_name for row in schema_result]
                            
                            # Find the correct revenue column for this specific table
                            actual_revenue_col = None
                            for col in actual_columns:
                                if any(term in col.lower() for term in ['cash_credit_total', 'total_sales', 'sales', 'revenue', 'total']):
                                    actual_revenue_col = col
                                    break
                            
                            # Find vendor name column
                            vendor_col = None
                            for col in actual_columns:
                                if 'vendor' in col.lower() and 'name' in col.lower():
                                    vendor_col = col
                                    break
                            if not vendor_col:
                                for col in actual_columns:
                                    if 'name' in col.lower():
                                        vendor_col = col
                                        break
                            
                            if not actual_revenue_col or not vendor_col:
                                app.logger.warning(f"Table {table}: Missing columns - revenue: {actual_revenue_col}, vendor: {vendor_col}")
                                continue
                                
                        except Exception as schema_error:
                            app.logger.warning(f"Failed to get schema for {table}: {schema_error}")
                            continue
                        
                        # Enhanced table query with dynamic column detection
                        table_query = f"""
                        SELECT 
                            '{table}' as table_source,
                            CASE 
                                WHEN '{table}' LIKE '2023-%' THEN SUBSTRING('{table}', 1, 10)
                                WHEN '{table}' LIKE '2024-%' THEN SUBSTRING('{table}', 1, 10)
                                WHEN '{table}' LIKE '2022-%' THEN SUBSTRING('{table}', 1, 10)
                                ELSE 'Unknown-Date'
                            END as event_date,
                            CASE
                                WHEN '{table}' LIKE '%Kapwa-Gardens%' THEN 'Kapwa Gardens'
                                WHEN '{table}' LIKE '%UNDISCOVERED%' THEN 'UNDISCOVERED SF'
                                WHEN '{table}' LIKE '%Balay-Kreative%' THEN 'Balay Kreative'
                                WHEN '{table}' LIKE '%Lovers-Mart%' THEN 'Lovers Mart'
                                WHEN '{table}' LIKE '%Yum-Yams%' THEN 'Yum Yams'
                                WHEN '{table}' LIKE '%Dye-Hard%' THEN 'Dye Hard'
                                WHEN '{table}' LIKE '%Halo-Halo%' THEN 'Halo Halo Holidays'
                                WHEN '{table}' LIKE '%Many-Styles%' THEN 'Many Styles'
                                WHEN '{table}' LIKE '%Be-Free%' THEN 'Be Free Festival'
                                ELSE REGEXP_EXTRACT('{table}', r'---(.*?)---')
                            END as event_name,
                            COUNT(*) as record_count,
                            SUM(CAST(REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                            AVG(CAST(REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as average_revenue,
                            MIN(CAST(REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as min_revenue,
                            MAX(CAST(REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as max_revenue
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table}`
                        WHERE {vendor_col} IS NOT NULL
                        AND CAST({actual_revenue_col} AS STRING) NOT LIKE '%REF%'
                        AND CAST({actual_revenue_col} AS STRING) != ''
                        AND REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') != ''
                        AND REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') != '0'
                        AND REGEXP_REPLACE(CAST({actual_revenue_col} AS STRING), r'[^0-9.]', '') != '0.0'
                        """
                        
                        try:
                            start_time = time.time()
                            table_job = bigquery_client.query(table_query)
                            table_results = table_job.result(timeout=60)
                            table_results = list(table_results)
                            execution_time = time.time() - start_time
                            app.logger.info(f"Expanded multi-table query for {table}: executed in {execution_time:.2f}s, returned {len(table_results)} rows.")
                            
                            if table_results and len(table_results) > 0:
                                # Convert BigQuery Row objects to dictionaries
                                row_dict = dict(table_results[0])
                                if row_dict.get('total_revenue') and row_dict['total_revenue'] > 0:
                                    all_revenue_data.append(row_dict)
                                    total_comprehensive_revenue += row_dict['total_revenue']
                                    total_comprehensive_transactions += row_dict['record_count']
                        except Exception as table_error:
                            app.logger.warning(f"Skipping table {table} due to error: {table_error}")
                            continue
                    
                    if all_revenue_data:
                        # Sort by total revenue to find highest performing events
                        all_revenue_data.sort(key=lambda x: x['total_revenue'] or 0, reverse=True)
                        
                        # Add comprehensive summary as first record
                        comprehensive_summary = {
                            "table_source": "COMPREHENSIVE_ANALYSIS_SUMMARY",
                            "event_date": "2020-2024",
                            "event_name": "All Events Combined",
                            "record_count": total_comprehensive_transactions,
                            "total_revenue": total_comprehensive_revenue,
                            "average_revenue": total_comprehensive_revenue / total_comprehensive_transactions if total_comprehensive_transactions > 0 else 0,
                            "min_revenue": min([x['total_revenue'] for x in all_revenue_data if x['total_revenue']]),
                            "max_revenue": max([x['total_revenue'] for x in all_revenue_data if x['total_revenue']]),
                            "tables_analyzed": len(all_revenue_data)
                        }
                        
                        # Insert summary at the beginning
                        all_revenue_data.insert(0, comprehensive_summary)
                        
                        return {
                            "status": "success",
                            "data": all_revenue_data,
                            "analysis_type": "expanded_comprehensive_multi_table",
                            "tables_analyzed": len(all_revenue_data) - 1,  # Subtract 1 for summary record
                            "comprehensive_revenue": total_comprehensive_revenue,
                            "comprehensive_transactions": total_comprehensive_transactions
                        }
                    else:
                        app.logger.warning("No revenue data found in expanded comprehensive analysis, falling back to single table")
                        # Fall through to single table analysis
                else:
                    app.logger.warning("No revenue columns found in expanded comprehensive analysis, falling back to single table")
                    # Fall through to single table analysis
            
            # STEP 3: SINGLE TABLE QUERY CONSTRUCTION BASED ON INTENT
            # Detect if this is a simple data display or complex analysis request
            if any(keyword in query_lower for keyword in ['show me data', 'display data', 'see data', 'view data']):
                # Simple data display - just show records from most relevant table
                target_table = relevant_tables[0]
                columns = schema_info.get(target_table, [])
                
                if columns:
                    # Select appropriate columns based on query context
                    if any(keyword in query_lower for keyword in ['contact', 'email', 'phone']):
                        contact_cols = [col for col in columns if any(term in col.lower() for term in ['email', 'phone', 'contact', 'name'])]
                        if contact_cols:
                            final_query = f"SELECT {', '.join(contact_cols[:5])} FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 100"
                        else:
                            final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 100"
                    else:
                        final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 100"
                else:
                    final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 100"
            
            else:
                # BUSINESS INTELLIGENCE ANALYSIS (SINGLE TABLE)
                # Find tables with revenue columns for financial analysis
                revenue_tables = []
                for table, columns in schema_info.items():
                    revenue_cols = [col for col in columns if any(term in col.lower() for term in ['total_sales', 'sales', 'revenue', 'cash', 'credit'])]
                    if revenue_cols:
                        revenue_tables.append((table, revenue_cols[0]))  # Use first revenue column
                
                if revenue_tables:
                    # ENHANCED TABLE SELECTION: Filter for specific events mentioned in query
                    target_table, revenue_column = revenue_tables[0]  # Default fallback
                    
                    # Check if query mentions specific event/date and filter accordingly
                    query_lower = original_query.lower()
                    
                    # Event name mapping for precise table selection
                    event_filters = {
                        'undiscovered': ['undiscovered'],
                        'kapwa gardens': ['kapwa'],
                        'lovers mart': ['lovers-mart'],
                        'yum yams': ['yum-yams'],
                        'dye hard': ['dye-hard'],
                        'be free': ['be-free'],
                        'halo halo': ['halo-halo'],
                        'many styles': ['many-styles'],
                        'balay kreative': ['balay-kreative'],
                        'ancestor altars': ['ancestor-altars'],
                        'baked': ['baked'],
                        'lavender cinema': ['lavender-cinema']
                    }
                    
                    # Date filtering for specific dates mentioned
                    date_patterns = [
                        r'august 19,? 2023', r'2023-08-19',
                        r'september 16,? 2023', r'2023-09-16', 
                        r'october 21,? 2023', r'2023-10-21',
                        r'february 11,? 2023', r'2023-02-11',
                        r'may 13,? 2023', r'2023-05-13',
                        r'june 10,? 2023', r'2023-06-10',
                        r'march 18,? 2023', r'2023-03-18'
                    ]
                    
                    # Find best matching table based on event name and date
                    best_match_table = None
                    best_match_score = 0
                    
                    for table, rev_col in revenue_tables:
                        table_lower = table.lower()
                        match_score = 0
                        
                        # Score based on event name matches
                        for event_name, event_keywords in event_filters.items():
                            if event_name in query_lower:
                                for keyword in event_keywords:
                                    if keyword in table_lower:
                                        match_score += 10
                        
                        # Score based on date matches
                        import re
                        for date_pattern in date_patterns:
                            if re.search(date_pattern, query_lower):
                                # Extract expected date components
                                if 'august 19' in query_lower or '2023-08-19' in query_lower:
                                    if '2023-08-19' in table_lower:
                                        match_score += 20
                                elif 'september 16' in query_lower or '2023-09-16' in query_lower:
                                    if '2023-09-16' in table_lower:
                                        match_score += 20
                                elif 'february 11' in query_lower or '2023-02-11' in query_lower:
                                    if '2023-02-11' in table_lower:
                                        match_score += 20
                        
                        # Update best match if this table scores higher
                        if match_score > best_match_score:
                            best_match_score = match_score
                            best_match_table = (table, rev_col)
                    
                    # Use best matching table if found, otherwise use first table
                    if best_match_table and best_match_score > 0:
                        target_table, revenue_column = best_match_table
                        app.logger.info(f"SPECIFIC EVENT FILTER: Selected '{target_table}' with score {best_match_score} for query: {original_query[:100]}")
                    else:
                        app.logger.info(f"NO SPECIFIC MATCH: Using default table '{target_table}' for query: {original_query[:100]}")
                    
                    # Enhanced revenue analysis with proper currency handling
                    final_query = f"""
                SELECT 
                    '{target_table}' as table_source,
                    COUNT(*) as record_count,
                    SUM(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                    AVG(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as average_revenue,
                    MIN(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as min_revenue,
                    MAX(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as max_revenue
                FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}`
                WHERE {revenue_column} IS NOT NULL
                AND CAST({revenue_column} AS STRING) NOT LIKE '%REF%'
                AND CAST({revenue_column} AS STRING) != ''
                AND REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') != ''
                """
                else:
                    # For counting/aggregation queries, use Gemini AI to generate proper SQL
                    if any(word in query_lower for word in ['how many', 'count', 'total', 'number of']):
                        app.logger.info(f"Converting counting query to SQL with Gemini AI: {query[:100]}...")
                        try:
                            client = google_genai_for_client.Client(api_key=GEMINI_API_KEY)
                            
                            ai_prompt = f"""Convert this business question to SQL for BigQuery:

Question: {query}

Database: {GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}
Relevant tables found: {', '.join(relevant_tables[:5])}

Generate SQL that counts unique vendors across these Kapwa Gardens tables. Use UNION ALL to combine data from multiple tables if needed.

Return only the SQL query, no explanation."""

                            response = client.models.generate_content(
                                model='gemini-2.0-flash-exp',
                                contents=[ai_prompt]
                            )
                            
                            if response and hasattr(response, 'text'):
                                ai_generated_sql = response.text.strip()
                                # Clean up the response
                                if '```sql' in ai_generated_sql:
                                    ai_generated_sql = ai_generated_sql.split('```sql')[1].split('```')[0].strip()
                                elif '```' in ai_generated_sql:
                                    ai_generated_sql = ai_generated_sql.split('```')[1].strip()
                                
                                app.logger.info(f"Gemini AI generated counting SQL: {ai_generated_sql[:200]}...")
                                final_query = ai_generated_sql
                            else:
                                # Fallback to simple count from first table
                                target_table = relevant_tables[0]
                                final_query = f"SELECT COUNT(DISTINCT COALESCE(Vendor_Name, vendor_name, VENDOR_NAME)) as vendor_count FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` WHERE COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) IS NOT NULL AND COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) != ''"
                                
                        except Exception as e:
                            app.logger.error(f"Error using Gemini AI for counting query: {e}")
                            # Fallback to simple count
                            target_table = relevant_tables[0]
                            final_query = f"SELECT COUNT(DISTINCT COALESCE(Vendor_Name, vendor_name, VENDOR_NAME)) as vendor_count FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` WHERE COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) IS NOT NULL AND COALESCE(Vendor_Name, vendor_name, VENDOR_NAME) != ''"
                    else:
                        # Fallback to simple data display
                        target_table = relevant_tables[0]
                        final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 100"
        
        else:
            # This is a natural language query that needs Gemini AI processing
            app.logger.info(f"Natural language query needs AI processing: {query[:100]}...")
            
            # Use Gemini AI to convert natural language to SQL
            try:
                client = google_genai_for_client.Client(api_key=GEMINI_API_KEY)
                
                # Create a simple prompt for SQL conversion
                ai_prompt = f"""Convert this natural language query to SQL for BigQuery:
                
Query: {query}

Database: {GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}
Available tables include vendor sales data, attendee data, and event information.

For counting queries like "How many vendors participated in Kapwa Gardens events?", generate SQL that:
1. First discovers relevant tables with table discovery
2. Then counts unique vendors across those tables
3. Uses proper BigQuery syntax

Return only the SQL query, no explanation."""

                response = client.models.generate_content(
                    model='gemini-2.0-flash-exp',
                    contents=[ai_prompt]
                )
                
                if response and hasattr(response, 'text'):
                    ai_generated_sql = response.text.strip()
                    # Clean up the response to extract just the SQL
                    if '```sql' in ai_generated_sql:
                        ai_generated_sql = ai_generated_sql.split('```sql')[1].split('```')[0].strip()
                    elif '```' in ai_generated_sql:
                        ai_generated_sql = ai_generated_sql.split('```')[1].strip()
                    
                    app.logger.info(f"Gemini AI generated SQL: {ai_generated_sql[:200]}...")
                    final_query = ai_generated_sql
                else:
                    # Fallback: try to construct a basic query
                    final_query = f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE '%vendor%' OR LOWER(table_name) LIKE '%sales%' ORDER BY table_name"
                    
            except Exception as e:
                app.logger.error(f"Error using Gemini AI for SQL conversion: {e}")
                # Fallback: construct a basic query
                final_query = f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE '%vendor%' OR LOWER(table_name) LIKE '%sales%' ORDER BY table_name"
        
        # STEP 4: EXECUTE FINAL QUERY (for non-comprehensive analysis)
        if not (is_business_query and is_comprehensive):
            start_time = time.time()
            
            query_job = bigquery_client.query(final_query)
            results = query_job.result(timeout=60)
            results = list(results)
                
            execution_time = time.time() - start_time
            app.logger.info(f"Tool Call: internal_execute_sql_query executed in {execution_time:.2f}s, returned {len(results)} rows.")
            
            # Convert results to list of dictionaries
            data = []
            if results:
                # Get column names from the first row
                columns = list(results[0].keys())
                
                for row in results:
                    row_dict = {}
                    for col in columns:
                        value = row[col]
                        # Handle various data types
                        if hasattr(value, 'isoformat'):  # datetime objects
                            row_dict[col] = value.isoformat()
                        elif isinstance(value, (int, float, str, bool)) or value is None:
                            row_dict[col] = value
                        else:
                            row_dict[col] = str(value)
                    data.append(row_dict)
            
            # Store results globally for fallback extraction
            global last_sql_results
            last_sql_results = data
            
            return {
                "status": "success",
                "data": data,
                "query_executed": final_query[:200] + "..." if len(final_query) > 200 else final_query
            }
        
    except Exception as e:
        app.logger.error(f"BigQuery execution error: {str(e)}", exc_info=True)
        return {
            "status": "error", 
            "error_message": str(e),
            "query_attempted": query[:200] + "..." if len(query) > 200 else query
        }











def get_current_time() -> dict:
    """Returns the current date, time, and timezone.
    Returns:
        dict: A dictionary containing the current time string with a key 'current_time' and 'status'.
    """
    app.logger.info("Tool Call: get_current_time")
    current_time_str = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    return {"status": "success", "current_time": current_time_str}


def smart_table_filter(query: str, available_tables: list) -> list:
    """
    Enhanced table filtering using event name, date matching, and data type priority logic.
    Returns tables ordered by relevance score (highest first).
    """
    query_lower = query.lower()
    
    # CRITICAL: Detect query type for proper table prioritization
    attendee_keywords = ['attendee', 'attendees', 'how many attendees', 'count attendees', 'total attendees', 'number of attendees']
    vendor_keywords = ['vendor', 'vendors', 'sales', 'revenue', 'made money', 'earned', 'sold']
    phone_keywords = ['phone', 'cell', 'phone numbers', 'cell phone', 'cell numbers', 'billing_phone']
    
    is_attendee_query = any(keyword in query_lower for keyword in attendee_keywords)
    is_vendor_query = any(keyword in query_lower for keyword in vendor_keywords)
    is_phone_query = any(keyword in query_lower for keyword in phone_keywords)
    
    # Event name mapping for precise table selection
    event_filters = {
        'undiscovered': ['undiscovered'],
        'kapwa gardens': ['kapwa'],
        'lovers mart': ['lovers-mart'],
        'yum yams': ['yum-yams'],
        'dye hard': ['dye-hard'],
        'be free': ['be-free'],
        'halo halo': ['halo-halo'],
        'many styles': ['many-styles'],
        'balay kreative': ['balay-kreative'],
        'ancestor altars': ['ancestor-altars'],
        'baked': ['baked'],
        'lavender cinema': ['lavender-cinema'],
        'akassa': ['akassa'],
        'sulat': ['sulat']
    }
    
    # Date filtering for specific dates mentioned
    date_patterns = [
        r'august 19,? 2023', r'2023-08-19',
        r'september 16,? 2023', r'2023-09-16', 
        r'october 21,? 2023', r'2023-10-21',
        r'february 11,? 2023', r'2023-02-11',
        r'may 13,? 2023', r'2023-05-13',
        r'june 10,? 2023', r'2023-06-10',
        r'march 18,? 2023', r'2023-03-18',
        r'december 9,? 2023', r'2023-12-09',
        r'july 28,? 2023', r'2023-07-28',
        r'october 19,? 2024', r'2024-10-19',
        r'april 20,? 2024', r'2024-04-20'
    ]
    
    # Score each table based on query relevance
    scored_tables = []
    
    for table in available_tables:
        table_lower = table.lower()
        match_score = 0
        
        # CRITICAL: Priority scoring based on query type
        if is_attendee_query:
            # ATTENDEE QUERIES: Heavily prioritize attendee tables
            if 'attendee' in table_lower:
                match_score += 50  # High priority for attendee tables
            elif any(term in table_lower for term in ['squarespace', 'typeform']):
                match_score += 30  # Medium priority for registration tables
            elif 'vendor' in table_lower or 'close-out-sales' in table_lower:
                match_score -= 20  # Deprioritize vendor tables for attendee queries
        
        elif is_vendor_query:
            # VENDOR QUERIES: Prioritize vendor and sales tables
            if any(term in table_lower for term in ['vendor', 'close-out-sales', 'sales']):
                match_score += 30
            elif 'attendee' in table_lower:
                match_score -= 10  # Slightly deprioritize attendee tables for vendor queries
        
        elif is_phone_query:
            # PHONE QUERIES: Heavily prioritize Squarespace vendor tables with actual phone data
            if 'squarespace' in table_lower and 'vendor' in table_lower:
                match_score += 60  # Highest priority for Squarespace vendor tables (have Billing_Phone)
            elif 'squarespace' in table_lower:
                match_score += 40  # High priority for other Squarespace tables
            elif 'vendor' in table_lower and 'close-out-sales' in table_lower:
                match_score -= 30  # Deprioritize close-out sales (only have Contact_Name, not phone numbers)
        
        # Score based on event name matches
        for event_name, event_keywords in event_filters.items():
            if event_name in query_lower:
                for keyword in event_keywords:
                    if keyword in table_lower:
                        match_score += 10
        
        # Score based on date matches
        import re
        for date_pattern in date_patterns:
            if re.search(date_pattern, query_lower):
                # Extract expected date components
                if 'august 19' in query_lower or '2023-08-19' in query_lower:
                    if '2023-08-19' in table_lower:
                        match_score += 20
                elif 'september 16' in query_lower or '2023-09-16' in query_lower:
                    if '2023-09-16' in table_lower:
                        match_score += 20
                elif 'february 11' in query_lower or '2023-02-11' in query_lower:
                    if '2023-02-11' in table_lower:
                        match_score += 20
                elif 'october 19' in query_lower or '2024-10-19' in query_lower:
                    if '2024-10-19' in table_lower:
                        match_score += 20
                elif 'may 13' in query_lower or '2023-05-13' in query_lower:
                    if '2023-05-13' in table_lower:
                        match_score += 20
                elif 'june 10' in query_lower or '2023-06-10' in query_lower:
                    if '2023-06-10' in table_lower:
                        match_score += 20
        
        # Year-based scoring for general date ranges
        if '2023' in query_lower and '2023' in table_lower:
            match_score += 5
        elif '2024' in query_lower and '2024' in table_lower:
            match_score += 5
        
        # General table relevance (base score for all tables)
        if any(term in table_lower for term in ['close-out-sales', 'vendor', 'sales']):
            match_score += 1
            
        scored_tables.append((table, match_score))
    
    # Sort by score (highest first) and return table names
    scored_tables.sort(key=lambda x: x[1], reverse=True)
    
    if scored_tables and scored_tables[0][1] > 0:
        app.logger.info(f"SMART TABLE FILTER: Top match '{scored_tables[0][0]}' with score {scored_tables[0][1]} for query: {query[:100]}")
        if is_attendee_query:
            app.logger.info(f"ATTENDEE QUERY DETECTED: Prioritizing attendee tables")
        elif is_vendor_query:
            app.logger.info(f"VENDOR QUERY DETECTED: Prioritizing vendor tables")
    
    return [table for table, score in scored_tables]


def get_keboola_table_detail(bucket_id: str, table_name: str) -> dict:
    """Get detailed information about a specific table in a Keboola bucket.
    
    This function accesses your Keboola BigQuery workspace to retrieve table schema,
    column information, and sample data for analysis.
    
    Args:
        bucket_id (str): The Keboola bucket ID (e.g., 'out.c-main')
        table_name (str): The table name within the bucket
        
    Returns:
        dict: Table details including schema, columns, and sample data
    """
    app.logger.info(f"Tool Call: get_keboola_table_detail(bucket_id='{bucket_id}', table_name='{table_name}')")
    
    try:
        if not keboola_headers:
            return {
                'status': 'error',
                'error_message': 'Keboola API credentials not configured'
            }
            
        # Get table details from Keboola Storage API
        table_url = f"{KBC_API_URL}/v2/storage/tables/{bucket_id}.{table_name}"
        
        response = requests.get(table_url, headers=keboola_headers, timeout=30)
        
        if response.status_code == 200:
            table_info = response.json()
            
            # Format the response for display
            columns = table_info.get('columns', [])
            row_count = table_info.get('rowsCount', 0)
            
            return {
                'status': 'success',
                'data': {
                    'table_name': table_name,
                    'bucket_id': bucket_id,
                    'columns': columns,
                    'row_count': row_count,
                    'created': table_info.get('created'),
                    'last_change_date': table_info.get('lastChangeDate'),
                    'data_size_bytes': table_info.get('dataSizeBytes', 0)
                },
                'display_type': 'table_detail',
                'display_title': f'Table Details: {bucket_id}.{table_name}'
            }
        else:
            return {
                'status': 'error',
                'error_message': f'Failed to retrieve table details: {response.status_code} - {response.text}'
            }
            
    except Exception as e:
        app.logger.error(f"Error in get_keboola_table_detail: {e}")
        return {
            'status': 'error',
            'error_message': f'Error retrieving table details: {str(e)}'
        }


def get_workspace_overview() -> dict:
    """Get comprehensive overview of the BigQuery workspace including table counts and categorization.
    
    Returns:
        dict: Workspace overview with table counts, categories, and data source breakdown
    """
    try:
        # Get all tables from the workspace
        table_query = f"""
            SELECT table_name 
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE table_name NOT LIKE '-%'
            ORDER BY table_name
        """
        
        result = internal_execute_sql_query(table_query)
        if result.get('status') != 'success':
            return {"status": "error", "error_message": "Could not retrieve workspace tables"}
        
        table_names = [row['table_name'] for row in result['data']]
        
        # Dynamic categorization
        def categorize_table(table_name: str) -> str:
            name_lower = table_name.lower()
            if any(indicator in name_lower for indicator in ['typeform', 'form-responses', 'submissions']):
                return 'typeform'
            elif any(indicator in name_lower for indicator in ['squarespace', 'website-forms', 'online-forms']):
                return 'squarespace'
            elif any(indicator in name_lower for indicator in ['close-out', 'closeout', 'sales', 'revenue', 'vendor-close']):
                return 'closeout_sales'
            else:
                return 'other'
        
        # Categorize tables
        table_categories = {}
        for table in table_names:
            category = categorize_table(table)
            if category not in table_categories:
                table_categories[category] = []
            table_categories[category].append(table)
        
        # Create summary
        total_tables = len(table_names)
        category_summary = []
        for category, tables in table_categories.items():
            category_summary.append(f"{len(tables)} {category.replace('_', ' ')} tables")
        
        summary = f"WORKSPACE OVERVIEW: {total_tables} total tables in your BigQuery workspace | Breakdown: {', '.join(category_summary)} | Dynamic categorization adapts automatically as you add new data sources"
        
        return {
            "status": "success",
            "total_tables": total_tables,
            "categories": table_categories,
            "summary": summary,
            "all_tables": table_names
        }
        
    except Exception as e:
        return {"status": "error", "error_message": f"Error getting workspace overview: {str(e)}"}

def get_zip_codes_for_city(
        city_name: str,
        state_code: Optional[str] = None) -> dict:  # CORRECTED SIGNATURE
    """
    Retrieves a list of common zip codes for a given city and optional state.
    This is a placeholder and should be replaced with a real API call in production.
    Args:
        city_name (str): The name of the city (e.g., "San Francisco", "Daly City").
        state_code (Optional[str], optional): The 2-letter state code (e.g., "CA") to improve accuracy. Defaults to None.
    Returns:
        dict: A dictionary with 'status' ('success' or 'error'), and if successful, 
              'zip_codes' (a list of string zip codes), otherwise 'error_message'.
    """
    app.logger.info(
        f"Tool Call: get_zip_codes_for_city for {city_name}, State: {state_code}"
    )

    # ---- START MOCK IMPLEMENTATION ----
    # In a real application, replace this with a call to a reliable Zip Code API
    mock_zip_data = {
        "san francisco, ca": [
            "94102", "94103", "94104", "94105", "94107", "94108", "94109",
            "94110", "94111", "94112", "94114", "94115", "94116", "94117",
            "94118", "94121", "94122", "94123", "94124", "94127", "94129",
            "94130", "94131", "94132", "94133", "94134", "94158"
        ],
        "daly city, ca": ["94014", "94015", "94016", "94017"],
        "vacaville, ca": ["95687", "95688", "95696"]
    }

    effective_state_code = state_code
    if not effective_state_code and city_name.lower() in [
            "san francisco", "daly city", "vacaville"
    ]:
        effective_state_code = "ca"

    search_key = f"{city_name.lower()}"
    if effective_state_code:
        search_key = f"{city_name.lower()}, {effective_state_code.lower()}"

    if search_key in mock_zip_data:
        app.logger.info(f"Found mock zip codes for '{search_key}'")
        return {"status": "success", "zip_codes": mock_zip_data[search_key]}

    app.logger.warning(
        f"No mock zip codes found for '{search_key}'. In production, an API call would be made here."
    )
    # ---- END MOCK IMPLEMENTATION ----

    return {
        "status":
        "error",
        "error_message":
        f"Could not find zip codes for {city_name}{f', {state_code}' if state_code else ''} (mock data only). Replace with real API for full functionality."
    }


def convert_natural_language_to_sql(natural_query: str) -> str:
    """Convert natural language business questions to SQL queries"""
    query_lower = natural_query.lower()
    
    # Remove hardcoded attendee logic - let AI handle it intelligently
    
    # Revenue threshold queries
    if 'made over' in query_lower and '$' in query_lower:
        # Extract amount
        import re
        amount_match = re.search(r'\$(\d+)', query_lower)
        if amount_match:
            amount = amount_match.group(1)
            
            if 'kapwa gardens' in query_lower or 'kapwa' in query_lower or 'kg' in query_lower:
                # BUSINESS RULE: KG tables are Kapwa Gardens tables - but only for vendor/sales data, not attendee data
                if 'attendee' not in query_lower and 'registration' not in query_lower:
                    # Return None to trigger comprehensive analysis across all Kapwa Gardens vendor/sales tables (including KG tables)
                    return None
    
    # Multi-event participation - trigger comprehensive analysis instead of wildcard query
    if ('multiple' in query_lower and 'events' in query_lower) or ('participated' in query_lower and 'multiple' in query_lower):
        if 'kapwa gardens' in query_lower:
            # Return None to trigger comprehensive analysis which handles multi-table queries properly
            return None
    
    # Revenue trend analysis - trigger comprehensive analysis for multi-table queries
    if 'revenue' in query_lower and ('time' in query_lower or 'changed' in query_lower or 'over time' in query_lower):
        if 'kapwa gardens' in query_lower:
            return None
    
    # Comprehensive business intelligence - trigger comprehensive analysis
    if 'comprehensive' in query_lower and 'business intelligence' in query_lower:
        if 'kapwa gardens' in query_lower:
            return None
    
    # Default: return None to indicate unsupported query
    return None


def generate_business_intelligence_summary(query_description: str, data_rows: list, table_name: str) -> str:
    """Generate truly actionable business intelligence with specific insights from actual data"""
    import re
    try:
        if not data_rows:
            return "No data available for analysis."
        
        query_lower = query_description.lower()
        
        # Deep data analysis - examine ALL fields in ALL records
        all_vendor_names = set()
        all_revenue_values = []
        all_email_addresses = set()
        all_phone_numbers = set()
        all_zip_codes = set()
        all_cities = set()
        all_events = set()
        all_demographic_info = set()
        all_product_categories = set()
        
        field_analysis = {}
        populated_records = 0
        
        for record_idx, row in enumerate(data_rows):
            record_has_data = False
            for field_name, field_value in row.items():
                if not field_value or str(field_value).strip() in ['', ' $ -   ', 'NULL', 'None', 'N/A']:
                    continue
                
                record_has_data = True
                field_str = str(field_value).strip()
                field_lower = field_name.lower()
                
                # Track field usage
                if field_name not in field_analysis:
                    field_analysis[field_name] = {'values': set(), 'count': 0}
                field_analysis[field_name]['values'].add(field_str)
                field_analysis[field_name]['count'] += 1
                
                # Extract specific business data
                # Revenue/Financial data with multiple patterns
                if any(term in field_lower for term in ['sales', 'revenue', 'total', 'amount', 'cash', 'credit', 'payment', 'cost', 'price']):
                    # Look for dollar amounts
                    dollar_matches = re.findall(r'\$\s*(\d+(?:,\d{3})*(?:\.\d{2})?)', field_str)
                    for match in dollar_matches:
                        try:
                            all_revenue_values.append(float(match.replace(',', '')))
                        except:
                            pass
                    
                    # Look for numeric values that might be revenue
                    if '$' in field_str or any(keyword in field_lower for keyword in ['revenue', 'sales', 'total']):
                        numeric_matches = re.findall(r'(\d+(?:,\d{3})*(?:\.\d{2})?)', field_str.replace('$', ''))
                        for match in numeric_matches:
                            try:
                                value = float(match.replace(',', ''))
                                if 0 < value < 100000:  # Reasonable revenue range
                                    all_revenue_values.append(value)
                            except:
                                pass
                
                # Vendor/Business names
                if any(term in field_lower for term in ['vendor', 'business', 'company', 'name', 'merchant']) and 'email' not in field_lower:
                    if len(field_str) > 2 and field_str not in ['TRUE', 'FALSE', '0', '1']:
                        all_vendor_names.add(field_str)
                
                # Contact information
                if '@' in field_str and any(term in field_lower for term in ['email', 'contact', 'mail']):
                    all_email_addresses.add(field_str)
                
                # Phone numbers
                if any(term in field_lower for term in ['phone', 'cell', 'mobile', 'contact']) and re.search(r'\d{3}[-.\s]?\d{3}[-.\s]?\d{4}', field_str):
                    all_phone_numbers.add(field_str)
                
                # Geographic data
                if re.match(r'^\d{5}(-\d{4})?$', field_str) and any(term in field_lower for term in ['zip', 'postal', 'code']):
                    all_zip_codes.add(field_str)
                
                if any(term in field_lower for term in ['city', 'location', 'address']) and len(field_str) > 2:
                    all_cities.add(field_str)
                
                # Event information
                if any(term in field_lower for term in ['event', 'market', 'festival', 'show']):
                    all_events.add(field_str)
                
                # Demographics
                if any(term in field_lower for term in ['ethnic', 'race', 'demographic', 'identity']):
                    all_demographic_info.add(field_str)
                
                # Product categories
                if any(term in field_lower for term in ['product', 'category', 'type', 'food', 'service']):
                    all_product_categories.add(field_str)
            
            if record_has_data:
                populated_records += 1
        
        # Generate intelligent business insights based on query intent and actual data
        insights = []
        
        # Revenue analysis queries
        if any(term in query_lower for term in ['money', 'revenue', 'sales', 'made', 'income', 'profit']):
            if all_revenue_values:
                total_revenue = sum(all_revenue_values)
                avg_revenue = total_revenue / len(all_revenue_values)
                max_revenue = max(all_revenue_values)
                min_revenue = min(all_revenue_values)
                
                insights.append(f"FINANCIAL PERFORMANCE: ${total_revenue:,.2f} total revenue across {len(all_revenue_values)} transactions")
                insights.append(f"Revenue metrics: Average ${avg_revenue:,.2f}, Range ${min_revenue:,.2f} to ${max_revenue:,.2f}")
                
                # Performance distribution
                above_avg = [r for r in all_revenue_values if r > avg_revenue]
                if above_avg:
                    insights.append(f"High performers: {len(above_avg)} transactions above average, totaling ${sum(above_avg):,.2f}")
                
                # Vendor correlation if available
                if all_vendor_names:
                    insights.append(f"Revenue generated by {len(all_vendor_names)} unique vendors: {', '.join(list(all_vendor_names)[:5])}")
            else:
                insights.append(f"Revenue analysis attempted on {populated_records} records - financial data may be in non-standard format")
                if field_analysis:
                    financial_fields = [f for f in field_analysis.keys() if any(term in f.lower() for term in ['sales', 'revenue', 'total', 'amount'])]
                    if financial_fields:
                        insights.append(f"Potential financial fields identified: {', '.join(financial_fields[:3])}")
        
        # Vendor analysis queries
        elif any(term in query_lower for term in ['vendor', 'top', 'best', 'business', 'merchant']):
            if all_vendor_names:
                vendor_list = list(all_vendor_names)
                insights.append(f"VENDOR ANALYSIS: {len(vendor_list)} unique businesses identified")
                insights.append(f"Vendors include: {', '.join(vendor_list[:8])}")
                
                if all_revenue_values:
                    avg_per_vendor = sum(all_revenue_values) / len(vendor_list)
                    insights.append(f"Average revenue per vendor: ${avg_per_vendor:,.2f}")
                    
                    # Revenue performance ranking
                    if len(all_revenue_values) > 1:
                        sorted_revenues = sorted(all_revenue_values, reverse=True)
                        insights.append(f"Top revenue: ${sorted_revenues[0]:,.2f}, Median: ${sorted_revenues[len(sorted_revenues)//2]:,.2f}")
            else:
                insights.append(f"Vendor identification attempted across {populated_records} records")
                vendor_fields = [f for f in field_analysis.keys() if any(term in f.lower() for term in ['vendor', 'business', 'name'])]
                if vendor_fields:
                    insights.append(f"Business-related fields found: {', '.join(vendor_fields)}")
        
        # Contact/email queries
        elif any(term in query_lower for term in ['email', 'contact', 'address']):
            if all_email_addresses:
                email_list = list(all_email_addresses)
                insights.append(f"CONTACT DATABASE: {len(email_list)} email addresses extracted")
                
                # Domain analysis
                domains = {}
                for email in email_list:
                    if '@' in email:
                        domain = email.split('@')[1]
                        domains[domain] = domains.get(domain, 0) + 1
                
                if domains:
                    top_domain = max(domains, key=domains.get)
                    insights.append(f"Email domains: {top_domain} ({domains[top_domain]} contacts), plus {len(domains)-1} others")
                
                insights.append(f"Sample contacts: {', '.join(email_list[:4])}")
            else:
                insights.append(f"Contact search across {populated_records} records - no email addresses found in standard fields")
        
        # Phone number queries
        elif any(term in query_lower for term in ['phone', 'cell', 'number', 'mobile']):
            if all_phone_numbers:
                phone_list = list(all_phone_numbers)
                insights.append(f"PHONE DIRECTORY: {len(phone_list)} contact numbers identified")
                insights.append(f"Contact numbers: {', '.join(phone_list[:5])}")
            else:
                insights.append(f"Phone number search across {populated_records} records - contact fields may need formatting")
        
        # Geographic queries
        elif any(term in query_lower for term in ['zip', 'location', 'city', 'geographic']):
            if all_zip_codes:
                zip_list = list(all_zip_codes)
                insights.append(f"GEOGRAPHIC COVERAGE: {len(zip_list)} zip codes identified")
                insights.append(f"Service areas: {', '.join(zip_list[:10])}")
            
            if all_cities:
                city_list = list(all_cities)
                insights.append(f"Cities represented: {', '.join(city_list[:6])}")
            
            if not all_zip_codes and not all_cities:
                insights.append(f"Geographic analysis of {populated_records} records - location data may be in combined address fields")
        
        # General/demographic queries
        else:
            insights.append(f"COMPREHENSIVE ANALYSIS: {populated_records} records with usable data from {len(data_rows)} total records")
            
            # Multi-dimensional summary
            summary_metrics = []
            if all_vendor_names:
                summary_metrics.append(f"{len(all_vendor_names)} vendors")
            if all_revenue_values:
                summary_metrics.append(f"${sum(all_revenue_values):,.2f} total value")
            if all_email_addresses:
                summary_metrics.append(f"{len(all_email_addresses)} email contacts")
            if all_events:
                summary_metrics.append(f"{len(all_events)} events")
            
            if summary_metrics:
                insights.append(f"Key metrics: {', '.join(summary_metrics)}")
            
            # Data quality assessment
            completion_rate = (populated_records / len(data_rows)) * 100 if len(data_rows) > 0 else 0
            insights.append(f"Data quality: {completion_rate:.1f}% record completion rate")
        
        # Add data source context
        event_context = table_name.replace('---', ' ').replace('-', ' ').replace('_', ' ')
        insights.append(f"Data source: {event_context}")
        
        if not insights:
            return f"Analysis complete: {populated_records} records processed from {table_name} - data extraction patterns may need adjustment"
        
        return " | ".join(insights)
        
    except Exception as e:
        app.logger.error(f"Business intelligence error: {str(e)}")
        return f"Analysis error: Unable to process {len(data_rows)} records from {table_name} - {str(e)}"


# This function was removed to eliminate currency parsing errors and enable proper AI-driven tool architecture


def execute_comprehensive_analysis(table_name: str, analysis_type: str = "overview") -> dict:
    """Performs comprehensive data analysis on a specific table with advanced insights."""
    app.logger.info(f"Tool Call: execute_comprehensive_analysis - {table_name} ({analysis_type})")
    
    try:
        # Basic table information query
        sql_query = f"""
            SELECT *
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table_name}`
            LIMIT 50
        """
        
        result = internal_execute_sql_query(sql_query)
        
        if result.get('status') == 'success' and result.get('data'):
            # Add analysis metadata
            result['analysis_type'] = analysis_type
            result['table_analyzed'] = table_name
            result['total_rows_analyzed'] = len(result['data'])
            
        return result
        
    except Exception as e:
        app.logger.error(f"Error in comprehensive analysis: {e}", exc_info=True)
        return {"status": "error", "error_message": str(e)}


# Flask Routes - MCP Server API Endpoints
@app.after_request
def after_request(response):
    """Add CORS headers for external API access"""
    response.headers.add('Access-Control-Allow-Origin', '*')
    response.headers.add('Access-Control-Allow-Headers', 'Content-Type,Authorization')
    response.headers.add('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS')
    return response

@app.route('/', methods=['GET'])
def root():
    """MCP Server root endpoint"""
    return jsonify({
        "name": "Kultivate AI MCP Server",
        "version": "1.0.0",
        "description": "Business Intelligence API with natural language query processing",
        "endpoints": {
            "health": "/health",
            "tools": "/tools",
            "query": "/api/query",
            "sql": "/api/sql", 
            "tables": "/api/tables",
            "analysis": "/api/analysis",
            "geography": "/api/geography"
        },
        "documentation": "https://kultiva-chatv-2-mcp-conceptkitchen.replit.app",
        "status": "operational"
    })

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    return jsonify({"status": "healthy", "service": "MCP Server"})

@app.route('/tools', methods=['GET'])
def list_tools():
    """List available MCP tools"""
    return jsonify({
        "tools": [
            {
                "name": "internal_execute_sql_query",
                "description": "Execute BigQuery SQL queries",
                "parameters": ["sql_query"]
            },

            {
                "name": "get_zip_codes_for_city",
                "description": "Get zip codes for cities",
                "parameters": ["city_name", "state_code"]
            },
            {
                "name": "get_current_time",
                "description": "Get current date and time",
                "parameters": []
            },
            {
                "name": "get_keboola_table_detail",
                "description": "Get Keboola table metadata",
                "parameters": ["bucket_id", "table_name"]
            }
        ]
    })

@app.route('/api/sql', methods=['POST'])
def execute_sql():
    """Execute direct SQL queries"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
            
        result = internal_execute_sql_query(data['query'])
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/query', methods=['POST'])
def natural_language_query():
    """Enhanced natural language business intelligence using Gemini AI with MCP tools"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
        
        original_query = data['query']
        app.logger.info(f"Processing natural language query: {original_query}")
        
        # Check if this is a direct SQL query first
        if (original_query.strip().upper().startswith(('SELECT', 'WITH', 'CREATE', 'INSERT', 'UPDATE', 'DELETE', 'ALTER', 'DROP')) or
            'INFORMATION_SCHEMA' in original_query.upper() or
            (original_query.count('`') >= 2 and 'FROM' in original_query.upper())):
            app.logger.info(f"Direct SQL query detected: {original_query[:100]}...")
            result = internal_execute_sql_query(original_query)
            return jsonify(result)
        
        # CRITICAL: Apply smart routing for natural language queries
        app.logger.info("Processing with Gemini AI and enhanced smart routing")
        
        # Use natural language processing with AI tools for intelligent SQL query generation
        
        # PHONE QUERY DETECTION: Route directly to Squarespace vendor table for phone requests
        query_lower = original_query.lower()
        phone_indicators = ['phone', 'cell', 'phone numbers', 'cell phone', 'cell numbers', 'billing_phone']
        
        if any(indicator in query_lower for indicator in phone_indicators):
            app.logger.info("PHONE QUERY DETECTED AT API LEVEL: Routing directly to Squarespace vendor table with actual phone data")
            
            phone_query = f"""
            SELECT 
                Billing_Name as vendor_name,
                Email as email,
                Billing_Phone as phone_number
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.Undiscovered-Vendor-Export---Squarespace---All-data-orders`
            WHERE Billing_Phone IS NOT NULL 
            AND Billing_Phone != ''
            AND TRIM(Billing_Phone) != ''
            ORDER BY Billing_Name
            LIMIT 50
            """
            
            try:
                import time
                start_time = time.time()
                query_job = bigquery_client.query(phone_query)
                phone_results = query_job.result(timeout=60)
                phone_results = list(phone_results)
                execution_time = time.time() - start_time
                
                # Format results for display
                formatted_results = []
                for row in phone_results:
                    try:
                        vendor_name = getattr(row, 'vendor_name', str(row[0]) if len(row) > 0 else 'Unknown')
                        email = getattr(row, 'Email', str(row[1]) if len(row) > 1 else '')
                        phone = getattr(row, 'phone_number', str(row[2]) if len(row) > 2 else '')
                        
                        formatted_results.append({
                            "vendor": vendor_name,
                            "email": email,
                            "phone": phone
                        })
                    except Exception as e:
                        app.logger.warning(f"Error formatting phone result row: {e}")
                        continue
                
                app.logger.info(f"PHONE QUERY SUCCESS: Retrieved {len(formatted_results)} actual phone numbers in {execution_time:.2f}s")
                
                return jsonify({
                    "status": "success",
                    "data": formatted_results,
                    "query_executed": phone_query,
                    "execution_time": execution_time,
                    "query_type": "phone_extraction",
                    "table_source": "Undiscovered-Vendor-Export---Squarespace---All-data-orders",
                    "routing_method": "direct_phone_routing"
                })
                
            except Exception as e:
                app.logger.error(f"Direct phone query failed: {e}")
                # Fall through to standard processing
        
        # PROPER MCP WORKFLOW: Force table discovery first, then construct proper query
        app.logger.info("PROPER MCP: Starting with forced table discovery for grants and multi-event analysis")
        
        # Step 1: Discover grant and attendee tables first
        discovery_queries = [
            f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE '%grant%' OR LOWER(table_name) LIKE '%typeform%'",
            f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE '%balay%' AND LOWER(table_name) LIKE '%attendee%'",
            f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` WHERE LOWER(table_name) LIKE '%attendee%' OR LOWER(table_name) LIKE '%undiscovered%'"
        ]
        
        discovered_tables = []
        for discovery_sql in discovery_queries:
            try:
                app.logger.info(f"DISCOVERING TABLES: {discovery_sql[:80]}...")
                query_job = bigquery_client.query(discovery_sql)
                tables = query_job.result(timeout=30)
                table_names = [row.table_name for row in tables]
                discovered_tables.extend(table_names)
                app.logger.info(f"DISCOVERED: {len(table_names)} tables - {table_names}")
            except Exception as e:
                app.logger.warning(f"Table discovery failed: {e}")
        
        # Remove duplicates
        discovered_tables = list(set(discovered_tables))
        app.logger.info(f"TOTAL DISCOVERED TABLES: {len(discovered_tables)} - {discovered_tables}")
        
        if discovered_tables:
            # Step 2: Use Gemini AI to construct query with REAL table names
            try:
                client = google_genai_for_client.Client(api_key=GEMINI_API_KEY)
                
                # Create prompt with actual discovered table names
                discovery_prompt = f"""I need to answer: "{original_query}"

I have discovered these REAL tables in the workspace:
{chr(10).join([f"- {table}" for table in discovered_tables])}

Based on the table names, I can see:
- Grant data is likely in: {[t for t in discovered_tables if 'grant' in t.lower() or 'typeform' in t.lower()]}
- Attendee data is likely in: {[t for t in discovered_tables if 'attendee' in t.lower()]}

Please construct a SQL query using these EXACT table names to find people who:
1. Applied to Balay Kreative grants (from the grant/typeform table)
2. Attended multiple events (appearing in multiple attendee tables)

Use ONLY the table names I provided above. Never use fake table names like 'attendees' or 'grant_applications'.

Return only the SQL query."""

                response = client.models.generate_content(
                    model='gemini-2.0-flash-exp',
                    contents=[discovery_prompt]
                )
                
                if response and hasattr(response, 'text'):
                    ai_sql = response.text.strip()
                    
                    # Clean up the SQL
                    if '```sql' in ai_sql:
                        ai_sql = ai_sql.split('```sql')[1].split('```')[0].strip()
                    elif '```' in ai_sql:
                        ai_sql = ai_sql.split('```')[1].strip()
                    
                    app.logger.info(f"GEMINI CONSTRUCTED SQL WITH REAL TABLES: {ai_sql[:200]}...")
                    
                    # Execute the properly constructed query
                    try:
                        start_time = time.time()
                        query_job = bigquery_client.query(ai_sql)
                        results = query_job.result(timeout=60)
                        results = list(results)
                        execution_time = time.time() - start_time
                        
                        # Format results
                        data_rows = []
                        for row in results:
                            if hasattr(row, '_mapping'):
                                data_rows.append(dict(row._mapping))
                            elif hasattr(row, 'items'):
                                data_rows.append(dict(row.items()))
                            else:
                                data_rows.append(dict(row))
                        
                        app.logger.info(f"MCP SUCCESS: {len(data_rows)} results in {execution_time:.2f}s")
                        
                        return jsonify({
                            "status": "success",
                            "data": data_rows,
                            "query_executed": ai_sql,
                            "execution_time": execution_time,
                            "routing_method": "proper_mcp_discovery",
                            "query_type": "cross_dataset_analysis",
                            "discovered_tables": discovered_tables,
                            "workflow_steps": [
                                f"1. Discovered {len(discovered_tables)} real tables",
                                "2. Used Gemini AI with authentic table names",
                                "3. Constructed cross-dataset query",
                                "4. Executed with real BigQuery data"
                            ]
                        })
                        
                    except Exception as e:
                        app.logger.error(f"Query execution with discovered tables failed: {e}")
                        
            except Exception as e:
                app.logger.error(f"Gemini AI construction with discovered tables failed: {e}")
                return jsonify({
                    "status": "error",
                    "error_message": f"Failed to construct query with discovered tables: {str(e)}",
                    "discovered_tables": discovered_tables,
                    "routing_method": "mcp_construction_failed"
                })
        else:
            return jsonify({
                "status": "error", 
                "error_message": "No relevant tables discovered for grant and multi-event analysis",
                "routing_method": "no_tables_discovered"
            })
    
    except Exception as e:
        app.logger.error(f"Natural language query processing error: {e}")
        return jsonify({
            "status": "error", 
            "error": f"Query processing failed: {str(e)}",
            "query_type": "natural_language_error"
        }), 500

# Removed problematic process_revenue_analysis function - all revenue queries now use comprehensive analysis

def process_attendee_analysis(query):
    """Process attendee and contact information queries"""
    # Discover attendee/contact tables
    table_discovery = internal_execute_sql_query(f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%attendee%' 
        OR LOWER(table_name) LIKE '%squarespace%'
        OR LOWER(table_name) LIKE '%typeform%'
        ORDER BY table_name
    """)
    
    if table_discovery.get('status') == 'success':
        # Fix BigQuery Row object conversion with proper error handling
        tables = []
        for row in table_discovery.get('data', []):
            try:
                if isinstance(row, dict):
                    if 'table_name' in row:
                        tables.append(row['table_name'])
                else:
                    # Convert BigQuery Row object to dict safely
                    if hasattr(row, 'table_name'):
                        tables.append(row.table_name)
                    elif hasattr(row, '_fields') and 'table_name' in row._fields:
                        row_dict = dict(row)
                        tables.append(row_dict['table_name'])
                    else:
                        # Try first field if it's a table name
                        row_dict = dict(row)
                        if row_dict:
                            tables.append(list(row_dict.values())[0])
            except (KeyError, AttributeError, IndexError) as e:
                app.logger.warning(f"Row conversion error in attendee analysis: {e}, Row: {row}")
                continue
        return build_attendee_analysis(tables, query)
    
    return jsonify({"error": "Failed to discover attendee tables"})

def process_table_discovery(query):
    """Process table discovery and listing queries"""
    return internal_execute_sql_query(f"""
        SELECT table_name, table_type, creation_time
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
        ORDER BY table_name
    """)

def process_event_analysis(query):
    """Process event-specific analysis queries"""
    # Comprehensive event analysis across all tables
    if 'kapwa' in query:
        return analyze_specific_event('kapwa')
    elif 'undiscovered' in query:
        return analyze_specific_event('undiscovered')
    elif 'balay' in query:
        return analyze_specific_event('balay')
    else:
        return analyze_all_events()

def process_comprehensive_query(query):
    """Process general comprehensive business intelligence queries"""
    # Multi-step comprehensive analysis
    return internal_execute_sql_query(f"""
        SELECT 'Comprehensive Analysis' as analysis_type,
               COUNT(*) as total_tables
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
    """)

# Removed build_multi_table_revenue_query function - all revenue analysis now uses comprehensive analysis

def build_attendee_analysis(tables, query):
    """Build comprehensive attendee analysis"""
    if not tables:
        return {"error": "No attendee tables found"}
    
    # Analyze first available attendee table
    for table in tables:
        result = internal_execute_sql_query(f"""
            SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table}`
            LIMIT 10
        """)
        if result.get('status') == 'success' and result.get('data'):
            return result
    
    return {"error": "No data found in attendee tables"}

def analyze_specific_event(event_name):
    """Analyze specific event data"""
    if event_name == 'kapwa':
        # Include both 'kapwa' and 'KG' tables for Kapwa Gardens
        return internal_execute_sql_query(f"""
            SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE LOWER(table_name) LIKE '%kapwa%' 
            OR UPPER(table_name) LIKE '%KG%'
            ORDER BY table_name
        """)
    else:
        return internal_execute_sql_query(f"""
            SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE LOWER(table_name) LIKE '%{event_name}%'
            ORDER BY table_name
        """)

def analyze_all_events():
    """Analyze all events comprehensively"""
    return internal_execute_sql_query(f"""
        SELECT 
            table_name,
            CASE 
                WHEN LOWER(table_name) LIKE '%kapwa%' OR UPPER(table_name) LIKE '%KG%' THEN 'Kapwa Gardens'
                WHEN LOWER(table_name) LIKE '%undiscovered%' THEN 'UNDISCOVERED'
                WHEN LOWER(table_name) LIKE '%balay%' THEN 'Balay Kreative'
                ELSE 'Other Events'
            END as event_category
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
        ORDER BY event_category, table_name
    """)

@app.route('/api/tables', methods=['GET'])
def list_tables():
    """Discover available tables in BigQuery workspace"""
    try:
        result = internal_execute_sql_query(f"""
            SELECT table_name, table_type, creation_time
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES`
            ORDER BY table_name
        """)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/keboola/table', methods=['POST'])
def get_table_metadata():
    """Get Keboola table metadata and schema"""
    try:
        data = request.get_json()
        if not data or 'bucket_id' not in data or 'table_name' not in data:
            return jsonify({"error": "Missing 'bucket_id' or 'table_name' parameters"}), 400
            
        result = get_keboola_table_detail(data['bucket_id'], data['table_name'])
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/analysis', methods=['POST'])
def comprehensive_analysis():
    """Perform comprehensive data analysis"""
    try:
        data = request.get_json()
        if not data or 'table_name' not in data:
            return jsonify({"error": "Missing 'table_name' parameter"}), 400
            
        table_name = data['table_name']
        analysis_type = data.get('analysis_type', 'overview')
        
        result = execute_comprehensive_analysis(table_name, analysis_type)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/geography', methods=['POST'])
def geographic_lookup():
    """Get zip codes for cities"""
    try:
        data = request.get_json()
        if not data or 'city_name' not in data:
            return jsonify({"error": "Missing 'city_name' parameter"}), 400
            
        city_name = data['city_name']
        state_code = data.get('state_code')
        
        result = get_zip_codes_for_city(city_name, state_code)
        return jsonify(result)
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

# === DASHBOARD API ENDPOINTS ===

def load_csv_fallback_data(csv_filename):
    """Load financial data from CSV fallback files"""
    import csv
    try:
        csv_path = os.path.join(os.path.dirname(__file__), csv_filename)
        data = []
        with open(csv_path, 'r', encoding='utf-8') as file:
            reader = csv.DictReader(file)
            for row in reader:
                data.append(dict(row))
        return data
    except Exception as e:
        app.logger.error(f"Error loading CSV fallback {csv_filename}: {e}")
        return []

def query_dashboard_bigquery(sql_query):
    """Execute query on dashboard BigQuery workspace with CSV fallback"""
    if dashboard_bigquery_client:
        try:
            query_job = dashboard_bigquery_client.query(sql_query)
            results = query_job.result()
            return [dict(row) for row in results]
        except Exception as e:
            app.logger.error(f"Dashboard BigQuery error: {e}")
    
    # CSV fallback
    app.logger.info("Using CSV fallback for dashboard data")
    kapwa_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv')
    undiscovered_data = load_csv_fallback_data('undiscovered_dashboard_data.csv')
    return kapwa_data + undiscovered_data

def query_squarespace_dashboard_bigquery(sql_query):
    """Execute query on Squarespace forms dashboard BigQuery workspace"""
    if dashboard_squarespace_bigquery_client:
        try:
            query_job = dashboard_squarespace_bigquery_client.query(sql_query)
            results = query_job.result()
            return [dict(row) for row in results]
        except Exception as e:
            app.logger.error(f"Squarespace Dashboard BigQuery error: {e}")
    return None

@app.route('/api/dashboard/financial-summary', methods=['GET'])
def dashboard_financial_summary():
    """Get financial summary for dashboard visualization"""
    try:
        # Try BigQuery first, fall back to CSV processing
        if dashboard_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES:
            sql_query = f"""
            SELECT 
                'Kapwa Gardens' as event_type,
                SUM(CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                COUNT(*) as vendor_count
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES}.kapwa_gardens_financial_data`
            UNION ALL
            SELECT 
                'UNDISCOVERED' as event_type,
                SUM(CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                COUNT(*) as vendor_count
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES}.undiscovered_financial_data`
            """
            try:
                query_job = dashboard_bigquery_client.query(sql_query)
                bigquery_results = query_job.result()
                results = [dict(row) for row in bigquery_results]
            except Exception as e:
                app.logger.error(f"Dashboard BigQuery error: {e}")
                # Force CSV fallback by setting results to None
                results = None
        else:
            results = None
        
        # CSV fallback with proper data processing
        if results is None:
            # CSV fallback calculation with improved revenue detection
            kapwa_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv')
            undiscovered_data = load_csv_fallback_data('undiscovered_dashboard_data.csv')
            
            # Calculate Kapwa Gardens totals (use cash_credit_total as main revenue source)
            kapwa_revenue = 0.0
            kapwa_vendors = 0
            for row in kapwa_data:
                if row.get('vendor_name') and row.get('vendor_name').strip():
                    revenue = 0.0
                    if row.get('cash_credit_total'):
                        revenue = float(str(row.get('cash_credit_total', 0) or 0).replace('$', '').replace(',', '') or 0)
                    elif row.get('total_sales'):
                        revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                    
                    if revenue > 0:
                        kapwa_revenue += revenue
                        kapwa_vendors += 1
            
            # Calculate UNDISCOVERED totals (use total_sales as primary)
            undiscovered_revenue = 0.0
            undiscovered_vendors = 0
            for row in undiscovered_data:
                if row.get('vendor_name') and row.get('vendor_name').strip():
                    revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                    if revenue > 0:
                        undiscovered_revenue += revenue
                        undiscovered_vendors += 1
            
            results = [
                {"event_type": "Kapwa Gardens", "total_revenue": round(kapwa_revenue, 2), "vendor_count": kapwa_vendors},
                {"event_type": "UNDISCOVERED", "total_revenue": round(undiscovered_revenue, 2), "vendor_count": undiscovered_vendors}
            ]
        
        return jsonify({
            "status": "success",
            "data": results,
            "source": "dashboard_workspace" if dashboard_bigquery_client else "csv_fallback"
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/dashboard/vendor-performance', methods=['GET'])
def dashboard_vendor_performance():
    """Get top performing vendors for dashboard charts"""
    try:
        # Try BigQuery first, fall back to CSV processing
        if dashboard_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES:
            sql_query = f"""
            SELECT 
                vendor_name,
                event_name,
                CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64) as revenue
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_CLOSE_OUT_SALES}.kapwa_gardens_financial_data`
            WHERE vendor_name IS NOT NULL AND vendor_name != ''
            ORDER BY revenue DESC
            LIMIT 20
            """
            try:
                query_job = dashboard_bigquery_client.query(sql_query)
                bigquery_results = query_job.result()
                results = [dict(row) for row in bigquery_results]
            except Exception as e:
                app.logger.error(f"Dashboard BigQuery error: {e}")
                # Force CSV fallback by setting results to None
                results = None
        else:
            results = None
        
        # CSV fallback with proper data processing
        if results is None:
            # CSV fallback with improved revenue detection
            kapwa_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv')
            undiscovered_data = load_csv_fallback_data('undiscovered_dashboard_data.csv')
            
            vendor_performance = []
            
            # Process Kapwa Gardens data (use cash_credit_total as main revenue source)
            for row in kapwa_data:
                if row.get('vendor_name') and row.get('vendor_name').strip():
                    # For Kapwa Gardens, prioritize cash_credit_total over total_sales
                    revenue = 0.0
                    if row.get('cash_credit_total'):
                        revenue = float(str(row.get('cash_credit_total', 0) or 0).replace('$', '').replace(',', '') or 0)
                    elif row.get('total_sales'):
                        revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                    
                    if revenue > 0:
                        vendor_performance.append({
                            "vendor_name": row.get('vendor_name'),
                            "event_name": row.get('event_name', 'Unknown'),
                            "total_sales": str(revenue),  # Keep consistent with expected field name
                            "revenue": revenue  # For sorting
                        })
            
            # Process UNDISCOVERED data (use total_sales as primary)
            for row in undiscovered_data:
                if row.get('vendor_name') and row.get('vendor_name').strip():
                    revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                    if revenue > 0:
                        vendor_performance.append({
                            "vendor_name": row.get('vendor_name'),
                            "event_name": row.get('event_name', 'Unknown'),
                            "total_sales": str(revenue),  # Keep consistent with expected field name
                            "revenue": revenue  # For sorting
                        })
            
            # Sort by revenue descending and take top 20
            results = sorted(vendor_performance, key=lambda x: x['revenue'], reverse=True)[:20]
        
        return jsonify({
            "status": "success", 
            "data": results,
            "source": "dashboard_workspace" if dashboard_bigquery_client else "csv_fallback"
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/dashboard/event-timeline', methods=['GET'])
def dashboard_event_timeline():
    """Get events timeline for dashboard visualization"""
    try:
        # CSV fallback (since we have event dates in CSV)
        all_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv') + load_csv_fallback_data('undiscovered_dashboard_data.csv')
        
        events_by_date = {}
        for row in all_data:
            event_date = row.get('event_date')
            event_name = row.get('event_name', 'Unknown Event')
            if event_date and event_date != '':
                revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                
                if event_date not in events_by_date:
                    events_by_date[event_date] = {
                        "date": event_date,
                        "event_name": event_name,
                        "total_revenue": 0,
                        "vendor_count": 0
                    }
                
                events_by_date[event_date]["total_revenue"] += revenue
                if revenue > 0:
                    events_by_date[event_date]["vendor_count"] += 1
        
        # Sort by date
        results = sorted(events_by_date.values(), key=lambda x: x['date'])
        
        return jsonify({
            "status": "success",
            "data": results,
            "source": "csv_fallback"
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/dashboard/revenue-breakdown', methods=['GET'])
def dashboard_revenue_breakdown():
    """Get revenue breakdown by event type for pie charts"""
    try:
        kapwa_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv')
        undiscovered_data = load_csv_fallback_data('undiscovered_dashboard_data.csv')
        
        # Calculate revenue by event
        event_revenue = {}
        
        for row in kapwa_data:
            event_name = row.get('event_name', 'Unknown Kapwa Event')
            revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
            event_revenue[event_name] = event_revenue.get(event_name, 0) + revenue
        
        for row in undiscovered_data:
            event_name = row.get('event_name', 'UNDISCOVERED SF')
            revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
            event_revenue[event_name] = event_revenue.get(event_name, 0) + revenue
        
        # Format for chart
        results = [{"event_name": name, "revenue": revenue} for name, revenue in event_revenue.items() if revenue > 0]
        results = sorted(results, key=lambda x: x['revenue'], reverse=True)
        
        return jsonify({
            "status": "success",
            "data": results,
            "total_revenue": sum(item['revenue'] for item in results),
            "source": "csv_fallback"
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/dashboard/attendee-analytics', methods=['GET'])
def dashboard_attendee_analytics():
    """Get attendee analytics from Squarespace forms data"""
    try:
        # Try Squarespace BigQuery first
        if dashboard_squarespace_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS:
            sql_query = f"""
            SELECT 
                REGEXP_EXTRACT(table_name, r'([^-]+)') as event_name,
                COUNT(*) as attendee_count,
                COUNT(DISTINCT COALESCE(Email, email, EMAIL)) as unique_emails,
                COUNTIF(COALESCE(Phone, phone, PHONE) IS NOT NULL AND COALESCE(Phone, phone, PHONE) != '') as contacts_with_phone
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS}.INFORMATION_SCHEMA.TABLES` t
            JOIN `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS}.{'{'}t.table_name{'}'}` 
            WHERE COALESCE(Email, email, EMAIL) IS NOT NULL AND COALESCE(Email, email, EMAIL) != ''
            GROUP BY event_name
            ORDER BY attendee_count DESC
            """
            results = query_squarespace_dashboard_bigquery(sql_query)
            
            if results:
                return jsonify({
                    "status": "success",
                    "data": results,
                    "data_source": "BigQuery (Squarespace Forms)",
                    "total_attendees": sum(item.get('attendee_count', 0) for item in results)
                })
        
        # Fallback response when no Squarespace data available
        return jsonify({
            "status": "success",
            "data": [],
            "message": "Squarespace forms data not available - configure KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS",
            "data_source": "No data source configured"
        })
        
    except Exception as e:
        app.logger.error(f"Dashboard attendee analytics error: {e}")
        return jsonify({"error": "Failed to generate attendee analytics"}), 500

def safe_float_conversion(value):
    """Safely convert a value to float, handling various formats"""
    if value is None:
        return 0.0
    
    # If already a number
    if isinstance(value, (int, float)):
        return float(value)
    
    # If string, clean and convert
    if isinstance(value, str):
        # Remove common currency symbols and whitespace
        cleaned = value.strip().replace('$', '').replace(',', '').replace(' ', '')
        if cleaned == '' or cleaned == 'N/A' or cleaned == '#REF!':
            return 0.0
        try:
            return float(cleaned)
        except ValueError:
            return 0.0
    
    return 0.0

@app.route('/api/dashboard/combined-insights', methods=['GET'])
def dashboard_combined_insights():
    """Get combined insights from both sales and attendee data"""
    try:
        insights = {
            "sales_data": {
                "total_revenue": 0,
                "vendor_count": 0,
                "events_with_sales": 0
            },
            "attendee_data": {
                "total_attendees": 0,
                "events_with_attendees": 0,
                "contact_completion_rate": 0
            },
            "combined_metrics": {
                "revenue_per_attendee": 0,
                "vendor_to_attendee_ratio": 0,
                "average_vendor_revenue": 0
            }
        }
        
        # Get sales data from CSV
        sales_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv') + load_csv_fallback_data('undiscovered_dashboard_data.csv')
        if sales_data:
            # Use correct column names from CSV: total_sales, cash_credit_total
            revenues = [safe_float_conversion(record.get('total_sales', record.get('cash_credit_total', '0'))) for record in sales_data]
            total_revenue = sum(revenues)
            vendor_names = set(record.get('vendor_name', '') for record in sales_data if record.get('vendor_name'))
            event_names = set(record.get('event_name', '') for record in sales_data if record.get('event_name'))
            
            insights["sales_data"]["total_revenue"] = total_revenue
            insights["sales_data"]["vendor_count"] = len(vendor_names)
            insights["sales_data"]["events_with_sales"] = len(event_names)
            insights["combined_metrics"]["average_vendor_revenue"] = round(total_revenue / len(vendor_names), 2) if vendor_names else 0
        
        # Get attendee data from Squarespace if available
        if dashboard_squarespace_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS:
            try:
                # Get table list first
                table_query = f"""
                SELECT table_name 
                FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS}.INFORMATION_SCHEMA.TABLES`
                WHERE table_type = 'BASE_TABLE'
                LIMIT 5
                """
                table_results = query_squarespace_dashboard_bigquery(table_query)
                
                if table_results:
                    total_attendees = 0
                    events_with_attendees = len(table_results)
                    contacts_with_phone = 0
                    
                    # Query each table for attendee counts
                    for table_info in table_results:
                        table_name = table_info.get('table_name')
                        if table_name:
                            attendee_query = f"""
                            SELECT 
                                COUNT(*) as count,
                                COUNTIF(COALESCE(Phone, phone, PHONE) IS NOT NULL AND COALESCE(Phone, phone, PHONE) != '') as with_phone
                            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD_SQUARESPACE_FORMS}.{table_name}`
                            WHERE COALESCE(Email, email, EMAIL) IS NOT NULL AND COALESCE(Email, email, EMAIL) != ''
                            """
                            attendee_results = query_squarespace_dashboard_bigquery(attendee_query)
                            
                            if attendee_results and attendee_results[0]:
                                total_attendees += attendee_results[0].get('count', 0)
                                contacts_with_phone += attendee_results[0].get('with_phone', 0)
                    
                    insights["attendee_data"]["total_attendees"] = total_attendees
                    insights["attendee_data"]["events_with_attendees"] = events_with_attendees
                    insights["attendee_data"]["contact_completion_rate"] = round((contacts_with_phone / total_attendees * 100), 2) if total_attendees > 0 else 0
                    
                    # Calculate combined metrics
                    if total_attendees > 0:
                        insights["combined_metrics"]["revenue_per_attendee"] = round(insights["sales_data"]["total_revenue"] / total_attendees, 2)
                    if total_attendees > 0 and insights["sales_data"]["vendor_count"] > 0:
                        insights["combined_metrics"]["vendor_to_attendee_ratio"] = round(insights["sales_data"]["vendor_count"] / total_attendees, 3)
                        
            except Exception as e:
                app.logger.warning(f"Could not get Squarespace attendee data: {e}")
        
        return jsonify({
            "status": "success",
            "data": insights,
            "data_sources": {
                "sales": "CSV (Close-out sales)",
                "attendees": "BigQuery (Squarespace forms)" if dashboard_squarespace_bigquery_client else "Not available"
            }
        })
        
    except Exception as e:
        app.logger.error(f"Dashboard combined insights error: {e}")
        return jsonify({"error": "Failed to generate combined insights"}), 500


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8081, debug=False)
