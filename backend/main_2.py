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
GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD', '/home/runner/workspace/backend/GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD.json')
KBC_WORKSPACE_SCHEMA_DASHBOARD = os.environ.get('KBC_WORKSPACE_SCHEMA_DASHBOARD', 'WORKSPACE_DASHBOARD')
DASHBOARD_PROJECT_ID = GOOGLE_PROJECT_ID  # Same project, different workspace

# Log configuration status for debugging
app.logger.info("=== Configuration Check ===")
app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'MISSING'}")
app.logger.info(f"KBC_WORKSPACE_SCHEMA: {'SET' if KBC_WORKSPACE_SCHEMA else 'MISSING'}")
app.logger.info(f"GEMINI_API_KEY: {'SET' if GEMINI_API_KEY else 'MISSING'}")
app.logger.info(f"GOOGLE_PROJECT_ID: {GOOGLE_PROJECT_ID}")
app.logger.info(f"KBC_WORKSPACE_ID: {KBC_WORKSPACE_ID}")

# --- Define System Instruction Constant ---
SYSTEM_INSTRUCTION_PROMPT = f"""You are an expert BigQuery Data Analyst Assistant specializing in business intelligence for event management data. Your workspace contains vendor sales data, attendee information, and event analytics from various sources including Kapwa Gardens, UNDISCOVERED, Balay Kreative, and other events.

**WORKSPACE DETAILS:**
- Project: `{GOOGLE_PROJECT_ID}` 
- Dataset: `{KBC_WORKSPACE_ID}`
- Data Sources: 28 closeout sales tables, 9 squarespace forms, 1 typeform data

**APPROACH FOR ALL BUSINESS QUESTIONS:**

1. **DISCOVER TABLES FIRST:** Always start with table discovery to see what data is available:
   ```sql
   SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
   WHERE table_name NOT LIKE '-%' ORDER BY table_name
   ```

2. **ANALYZE RELEVANT TABLES:** For multi-table questions, examine schemas of all relevant tables:
   ```sql
   SELECT column_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
   WHERE table_name = 'TABLE_NAME' ORDER BY ordinal_position
   ```

3. **CONSTRUCT COMPREHENSIVE QUERIES:** Build SQL that addresses the full scope of the question across all relevant tables, not just one table.

**YOUR TOOLS:**
- `internal_execute_sql_query`: Execute any BigQuery SQL query
- `get_zip_codes_for_city`: Get zip codes for geographic analysis  
- `get_current_time`: Get current date/time
- `get_keboola_table_detail`: Get table metadata

**CRITICAL RULES:**
- For revenue questions: Query ALL relevant sales tables, not just one
- For vendor analysis: Include ALL events they participated in
- For geographic questions: Use zip code lookup tool for city filtering
- Always use authentic data from actual table queries
- Provide specific dollar amounts, vendor names, and record counts from real data

**CRITICAL TABLE SEARCH LOGIC: When users ask for data in natural language (e.g., "show me undiscovered attendees squarespace data"), you MUST:**

1. **FIRST**: Get all available tables with: `SELECT table_name FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES\` ORDER BY table_name`

2. **THEN**: Use FUZZY MATCHING to find the best table. For natural language requests:
   - "undiscovered attendees squarespace" should match "Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-"
   - "kapwa gardens vendor data" should match tables containing "Kapwa-Gardens" 
   - "balay kreative" should match "Balay-Kreative" tables
   - Use CASE-INSENSITIVE pattern matching with LIKE or REGEXP_CONTAINS
   - Match partial keywords, not exact strings

3. **SEARCH PATTERN**: Use this BigQuery pattern for fuzzy matching:
   ```sql
   SELECT table_name FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES\` 
   WHERE LOWER(table_name) LIKE '%undiscovered%' 
   AND LOWER(table_name) LIKE '%attendees%' 
   AND LOWER(table_name) LIKE '%squarespace%'
   ORDER BY table_name
   ```

4. **NEVER say "table not found"** - if fuzzy search returns tables, pick the best match and query it directly.

5. **BE DECISIVE**: When user says "undiscovered events" and you find tables with "undiscovered", immediately pick the most relevant one and show the data. Do NOT ask "Did you want to see..." or "which date" - just pick the first/best match and show it.

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

- **Complex Business Intelligence Questions** (e.g., "How much money was made by vendors at Yum Yams event from 2020-2023?", "Which vendors who identify as X made more than $500 sales?", "How many attendees live in SF and Daly City?", "Who applied to Balay Kreative Grant and attended events more than 2x?", "Which vendors participated in Kapwa Gardens AND UNDSCVRD events and made at least $500?"):
    1.  **USE execute_complex_business_query FIRST** - This specialized tool handles sophisticated business intelligence queries with:
        * Multi-table analysis across all 64 workspace tables
        * Date range filtering (2020-2023, specific years)
        * Geographic analysis (zip codes, cities like SF, Daly City)
        * Revenue thresholds ($500+, income levels)
        * Multi-event participation tracking (Kapwa Gardens AND UNDSCVRD)
        * Demographic filtering (identity categories)
        * Contact information extraction (emails, phone numbers)
        * Cross-event attendance analysis
        * Grant application correlation with event participation
    2.  **The tool automatically handles:**
        * Table discovery across vendor, attendee, donor, and event data
        * Complex JOIN operations between related tables
        * Date range parsing and filtering
        * Geographic zip code mapping for SF, Daly City, etc.
        * Revenue calculations and financial analysis
        * Multi-condition filtering (event participation + revenue thresholds)
    3.  **If execute_complex_business_query doesn't fully address the query**, then fall back to manual SQL construction using internal_execute_sql_query with the detailed schema analysis approach.

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

# Initialize Dashboard BigQuery Client for financial data visualization
dashboard_bigquery_client = None
try:
    if GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD and os.path.exists(GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD):
        app.logger.info(f"Initializing Dashboard BigQuery Client using: {GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD}")
        with timeout_context(30):
            dashboard_bigquery_client = bigquery.Client.from_service_account_json(
                GOOGLE_APPLICATION_CREDENTIALS_DASHBOARD)
        app.logger.info(f"Successfully initialized Dashboard BigQuery Client. Project: {dashboard_bigquery_client.project}")
    else:
        app.logger.info("Dashboard credentials not found - will use CSV fallbacks for dashboard data")
except Exception as e:
    app.logger.error(f"Error initializing Dashboard BigQuery Client: {e}")
    app.logger.info("Will use CSV fallbacks for dashboard data")


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
            
            # Also look for table patterns in text (OUT_*, DIM_*, etc.)
            table_pattern = r'\b(OUT_[A-Z_]+_\d+_[A-Z_]+|OUT_[A-Z_]+_[A-Z_]+|DIM_[A-Z_]+|FACT_[A-Z_]+)\b'
            table_matches = re.findall(table_pattern, content)
            if table_matches:
                return table_matches[0]
    
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
    # Keywords that indicate a table data request
    data_keywords = ['show me', 'data from', 'customers', 'orders', 'products', 'table', 'query']
    
    if not any(keyword in user_message.lower() for keyword in data_keywords):
        return None
        
    # Extract table names from conversation history
    available_tables = []
    for entry in conversation_history:
        if entry.get('role') == 'assistant':
            content = entry.get('content', '')
            # Look for table patterns like OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS
            import re
            table_matches = re.findall(r'OUT_[A-Z_]+_\d+_[A-Z_]+|OUT_[A-Z_]+_[A-Z_]+', content)
            available_tables.extend(table_matches)
    
    if not available_tables:
        return None
        
    # Find best matching table
    user_lower = user_message.lower()
    best_table = None
    
    # Look for company + data type matches
    if 'kapwa gardens' in user_lower and 'customer' in user_lower:
        kapwa_customer_tables = [t for t in available_tables if 'CUSTOMERS' in t and 'KAPWA_GARDENS' in t]
        if kapwa_customer_tables:
            # Pick highest numbered version
            best_table = max(kapwa_customer_tables, key=lambda x: int(re.search(r'_(\d+)_', x).group(1)) if re.search(r'_(\d+)_', x) else 0)
    
    elif 'kultivate labs' in user_lower and 'customer' in user_lower:
        kultivate_customer_tables = [t for t in available_tables if 'CUSTOMERS' in t and 'KULTIVATE_LABS' in t]
        if kultivate_customer_tables:
            best_table = max(kultivate_customer_tables, key=lambda x: int(re.search(r'_(\d+)_', x).group(1)) if re.search(r'_(\d+)_', x) else 0)
    
    # Generic fallback - match any table with relevant keywords
    if not best_table:
        if 'customer' in user_lower:
            customer_tables = [t for t in available_tables if 'CUSTOMERS' in t]
            if customer_tables:
                best_table = max(customer_tables, key=lambda x: int(re.search(r'_(\d+)_', x).group(1)) if re.search(r'_(\d+)_', x) else 0)
    
    # Execute query if we found a table
    if best_table:
        sql_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{best_table}` LIMIT 10;"
        result = internal_execute_sql_query(sql_query)
        if result.get('status') == 'success':
            return {
                'auto_executed': True,
                'table_used': best_table,
                'result': result
            }
    
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
            'show me', 'how much', 'who are', 'which event', 'what vendor', 'revenue', 'sales',
            'attendee', 'contact', 'email', 'phone', 'made money', 'top vendor', 'best event',
            'across all', 'from all', 'compare', 'total from', 'breakdown'
        ]
        
        is_business_query = any(indicator in query_lower for indicator in business_indicators)
        
        # DETECT COMPREHENSIVE ANALYSIS REQUESTS (check original query)
        comprehensive_keywords = [
            'across all', 'all events', 'compare events', 'which event', 'best event',
            'most money', 'highest revenue', 'compare', 'breakdown', 'all tables',
            'made the most', 'top event', 'highest earning', 'compare revenue'
        ]
        
        is_comprehensive = any(keyword in original_query.lower() for keyword in comprehensive_keywords)
        
        if is_business_query:
            app.logger.info(f"Enhanced business intelligence query: {original_query}")
            app.logger.info(f"Comprehensive analysis detected: {is_comprehensive} for keywords: {[k for k in comprehensive_keywords if k in original_query.lower()]} - Original query: {original_query[:100]}")
            
            # STEP 1: INTELLIGENT TABLE DISCOVERY
            # Determine data source type based on query context
            if any(keyword in query_lower for keyword in ['attendee', 'contact', 'email', 'phone', 'squarespace', 'typeform']):
                # Contact/attendee data query
                table_discovery_query = f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%attendee%' 
        OR LOWER(table_name) LIKE '%squarespace%'
        OR LOWER(table_name) LIKE '%typeform%'
        ORDER BY table_name
        """
            else:
                # Revenue/sales data query (default) - FIXED to match actual table names
                table_discovery_query = f"""
        SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
        WHERE LOWER(table_name) LIKE '%close-out-sales%' 
        OR LOWER(table_name) LIKE '%vendor%'
        OR LOWER(table_name) LIKE '%sales%'
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
            
            # STEP 3: COMPREHENSIVE MULTI-TABLE ANALYSIS
            if is_comprehensive:
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
                        # Enhanced table query with more detailed event extraction
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
                            SUM(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                            AVG(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as average_revenue,
                            MIN(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as min_revenue,
                            MAX(CAST(REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') AS FLOAT64)) as max_revenue
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{table}`
                        WHERE {revenue_column} IS NOT NULL
                        AND CAST({revenue_column} AS STRING) NOT LIKE '%REF%'
                        AND CAST({revenue_column} AS STRING) != ''
                        AND REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') != ''
                        AND REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') != '0'
                        AND REGEXP_REPLACE(CAST({revenue_column} AS STRING), r'[^0-9.]', '') != '0.0'
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
                            final_query = f"SELECT {', '.join(contact_cols[:5])} FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"
                        else:
                            final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"
                    else:
                        final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"
                else:
                    final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"
            
            else:
                # BUSINESS INTELLIGENCE ANALYSIS (SINGLE TABLE)
                # Find tables with revenue columns for financial analysis
                revenue_tables = []
                for table, columns in schema_info.items():
                    revenue_cols = [col for col in columns if any(term in col.lower() for term in ['total_sales', 'sales', 'revenue', 'cash', 'credit'])]
                    if revenue_cols:
                        revenue_tables.append((table, revenue_cols[0]))  # Use first revenue column
                
                if revenue_tables:
                    # Use the first revenue table for analysis
                    target_table, revenue_column = revenue_tables[0]
                    
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
                    # Fallback to simple data display
                    target_table = relevant_tables[0]
                    final_query = f"SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"
        
        else:
            # Direct SQL query - execute as provided
            final_query = query
        
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
    
    # Revenue threshold queries
    if 'made over' in query_lower and '$' in query_lower:
        # Extract amount
        import re
        amount_match = re.search(r'\$(\d+)', query_lower)
        if amount_match:
            amount = amount_match.group(1)
            
            if 'kapwa gardens' in query_lower:
                # Use a specific table instead of wildcard query
                return f"""
                SELECT 
                    CASE 
                        WHEN Vendor_Name IS NOT NULL AND Vendor_Name != '' THEN Vendor_Name
                        WHEN vendor_name IS NOT NULL AND vendor_name != '' THEN vendor_name
                        WHEN VENDOR_NAME IS NOT NULL AND VENDOR_NAME != '' THEN VENDOR_NAME
                        ELSE 'Unknown Vendor'
                    END as vendor_name,
                    CASE 
                        WHEN Total_Sales IS NOT NULL AND Total_Sales != '' AND Total_Sales NOT LIKE '%REF%' THEN 
                            SAFE_CAST(REGEXP_REPLACE(CAST(Total_Sales AS STRING), r'[^0-9.]', '') AS FLOAT64)
                        WHEN total_sales IS NOT NULL AND total_sales != '' AND total_sales NOT LIKE '%REF%' THEN 
                            SAFE_CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)
                        WHEN Cash__Credit_Total IS NOT NULL AND Cash__Credit_Total != '' AND Cash__Credit_Total NOT LIKE '%REF%' THEN 
                            SAFE_CAST(REGEXP_REPLACE(CAST(Cash__Credit_Total AS STRING), r'[^0-9.]', '') AS FLOAT64)
                        ELSE 0
                    END as total_revenue
                FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.2023-02-11-Lovers-Mart-_-Close-Out-Sales---Kapwa-Gardens-START-HERE-Vendor-Close-Out-Sal`
                WHERE (
                    (Total_Sales IS NOT NULL AND Total_Sales != '' AND Total_Sales NOT LIKE '%REF%') OR
                    (total_sales IS NOT NULL AND total_sales != '' AND total_sales NOT LIKE '%REF%') OR
                    (Cash__Credit_Total IS NOT NULL AND Cash__Credit_Total != '' AND Cash__Credit_Total NOT LIKE '%REF%')
                )
                AND CASE 
                    WHEN Total_Sales IS NOT NULL AND Total_Sales != '' AND Total_Sales NOT LIKE '%REF%' THEN 
                        SAFE_CAST(REGEXP_REPLACE(CAST(Total_Sales AS STRING), r'[^0-9.]', '') AS FLOAT64)
                    WHEN total_sales IS NOT NULL AND total_sales != '' AND total_sales NOT LIKE '%REF%' THEN 
                        SAFE_CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)
                    WHEN Cash__Credit_Total IS NOT NULL AND Cash__Credit_Total != '' AND Cash__Credit_Total NOT LIKE '%REF%' THEN 
                        SAFE_CAST(REGEXP_REPLACE(CAST(Cash__Credit_Total AS STRING), r'[^0-9.]', '') AS FLOAT64)
                    ELSE 0
                END > {amount}
                ORDER BY total_revenue DESC
                LIMIT 20
                """
    
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
    """Enhanced natural language business intelligence with comprehensive multi-table analysis"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
        
        original_query = data['query']
        query = data['query'].lower()
        app.logger.info(f"Enhanced business intelligence query: {query}")
        app.logger.info(f"Original query: {original_query}")
        
        # Check if this is a direct SQL query first (before keyword routing)
        if (original_query.strip().upper().startswith(('SELECT', 'WITH', 'CREATE', 'INSERT', 'UPDATE', 'DELETE', 'ALTER', 'DROP')) or
            'INFORMATION_SCHEMA' in original_query.upper() or
            (original_query.count('`') >= 2 and 'FROM' in original_query.upper())):
            app.logger.info(f"DIRECT SQL QUERY detected: {original_query[:100]}...")
            result = internal_execute_sql_query(original_query)
            return jsonify(result)
        
        # Enhanced comprehensive multi-table analysis routing
        comprehensive_keywords = [
            'across all', 'all events', 'compare events', 'which event', 'best event',
            'most money', 'highest revenue', 'compare', 'breakdown', 'all tables',
            'made the most', 'top event', 'highest earning', 'compare revenue'
        ]
        
        is_comprehensive = any(keyword in query for keyword in comprehensive_keywords)
        
        if is_comprehensive:
            app.logger.info(f"COMPREHENSIVE ANALYSIS REQUEST detected with keywords: {[k for k in comprehensive_keywords if k in query]}")
            # Use enhanced internal tool for comprehensive multi-table analysis
            result = internal_execute_sql_query(data['query'])
            return jsonify(result)
        else:
            # Process natural language queries with pattern-based SQL generation
            app.logger.info(f"PROCESSING NATURAL LANGUAGE QUERY: {query}")
            
            # Convert natural language to SQL using pattern matching
            sql_query = convert_natural_language_to_sql(original_query)
            
            if sql_query:
                app.logger.info(f"Generated SQL from natural language: {sql_query[:200]}...")
                result = internal_execute_sql_query(sql_query)
                
                if result.get('status') == 'success':
                    result['ai_interpretation'] = f"Analysis of: {original_query}"
                    result['query_type'] = "natural_language"
                
                return jsonify(result)
            else:
                return jsonify({
                    "status": "error",
                    "error_message": f"Unable to process natural language query: {original_query}"
                })
        
    except Exception as e:
        app.logger.error(f"Error in natural language query: {str(e)}")
        return jsonify({"error": str(e)}), 500

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
                WHEN LOWER(table_name) LIKE '%kapwa%' THEN 'Kapwa Gardens'
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

@app.route('/api/dashboard/financial-summary', methods=['GET'])
def dashboard_financial_summary():
    """Get financial summary for dashboard visualization"""
    try:
        # Try BigQuery first, fall back to CSV
        if dashboard_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD:
            sql_query = f"""
            SELECT 
                'Kapwa Gardens' as event_type,
                SUM(CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                COUNT(*) as vendor_count
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD}.kapwa_gardens_financial_data`
            UNION ALL
            SELECT 
                'UNDISCOVERED' as event_type,
                SUM(CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64)) as total_revenue,
                COUNT(*) as vendor_count
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD}.undiscovered_financial_data`
            """
            results = query_dashboard_bigquery(sql_query)
        else:
            # CSV fallback calculation
            kapwa_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv')
            undiscovered_data = load_csv_fallback_data('undiscovered_dashboard_data.csv')
            
            # Calculate Kapwa Gardens totals
            kapwa_revenue = sum(float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0) for row in kapwa_data)
            kapwa_count = len([row for row in kapwa_data if float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0) > 0])
            
            # Calculate UNDISCOVERED totals
            undiscovered_revenue = sum(float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0) for row in undiscovered_data)
            undiscovered_count = len([row for row in undiscovered_data if float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0) > 0])
            
            results = [
                {"event_type": "Kapwa Gardens", "total_revenue": kapwa_revenue, "vendor_count": kapwa_count},
                {"event_type": "UNDISCOVERED", "total_revenue": undiscovered_revenue, "vendor_count": undiscovered_count}
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
        # Try BigQuery first, fall back to CSV
        if dashboard_bigquery_client and KBC_WORKSPACE_SCHEMA_DASHBOARD:
            sql_query = f"""
            SELECT 
                vendor_name,
                event_name,
                CAST(REGEXP_REPLACE(CAST(total_sales AS STRING), r'[^0-9.]', '') AS FLOAT64) as revenue
            FROM `{DASHBOARD_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA_DASHBOARD}.kapwa_gardens_financial_data`
            WHERE vendor_name IS NOT NULL AND vendor_name != ''
            ORDER BY revenue DESC
            LIMIT 20
            """
            results = query_dashboard_bigquery(sql_query)
        else:
            # CSV fallback
            all_data = load_csv_fallback_data('kapwa_gardens_dashboard_data.csv') + load_csv_fallback_data('undiscovered_dashboard_data.csv')
            
            vendor_performance = []
            for row in all_data:
                if row.get('vendor_name') and row.get('vendor_name').strip():
                    revenue = float(str(row.get('total_sales', 0) or 0).replace('$', '').replace(',', '') or 0)
                    if revenue > 0:
                        vendor_performance.append({
                            "vendor_name": row.get('vendor_name'),
                            "event_name": row.get('event_name', 'Unknown'),
                            "revenue": revenue
                        })
            
            # Sort by revenue and take top 20
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


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8081, debug=False)
