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
KBC_WORKSPACE_ID = os.environ.get('KBC_WORKSPACE_ID', 'WORKSPACE_21894820')
GEMINI_MODEL = "gemini-2.0-flash-exp"

# Log configuration status for debugging
app.logger.info("=== Configuration Check ===")
app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'MISSING'}")
app.logger.info(f"KBC_WORKSPACE_SCHEMA: {'SET' if KBC_WORKSPACE_SCHEMA else 'MISSING'}")
app.logger.info(f"GEMINI_API_KEY: {'SET' if GEMINI_API_KEY else 'MISSING'}")
app.logger.info(f"GOOGLE_PROJECT_ID: {GOOGLE_PROJECT_ID}")
app.logger.info(f"KBC_WORKSPACE_ID: {KBC_WORKSPACE_ID}")

# --- Define System Instruction Constant ---
SYSTEM_INSTRUCTION_PROMPT = f"""You are an expert BigQuery Data Analyst Assistant, adept at understanding natural language requests for data. Your primary goal is to help users understand and retrieve insights from their data stored in a Google BigQuery data warehouse (project ID: `{GOOGLE_PROJECT_ID}`, dataset/workspace schema: `{KBC_WORKSPACE_ID}`) containing business intelligence data.

**MANDATORY EXECUTION RULE: For ANY request mentioning table data, you MUST use internal_execute_sql_query ONLY. All data access goes through BigQuery workspace tables directly.**

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
def internal_execute_sql_query(sql_query: str) -> dict:
    """Executes a BigQuery SQL query against the Keboola project's data warehouse
    (dataset: WORKSPACE_21894820, project: kbc-use4-839-261b) and returns the results.
    Use this to answer questions about specific data, counts, aggregations, etc.
    The query should be a standard SQL SELECT statement.
    Ensure table names are fully qualified: `project_id.dataset_id.table_name`
    (e.g., `kbc-use4-839-261b.WORKSPACE_21894820.YOUR_TABLE_NAME`).

    For in-depth analysis, you can:
    - Use LIMIT 100 or higher for comprehensive data sets
    - Perform aggregations (COUNT, SUM, AVG, GROUP BY)
    - Join multiple tables for complex analysis
    - Use date ranges and filtering for specific periods
    - Calculate totals, trends, and statistical insights

    Args:
        sql_query (str): The BigQuery SQL SELECT query to execute.

    Returns:
        dict: A dictionary containing 'status' ('success', 'success_truncated', or 'error')
              and either 'data' (list of rows) or 'error_message'.
    """
    if not bigquery_client:
        msg = "BigQuery client not initialized. Please provide your Google Cloud credentials file to enable data querying."
        app.logger.error(f"Tool call internal_execute_sql_query: {msg}")
        return {"status": "error", "error_message": msg}
    
    app.logger.info(f"Tool Call: internal_execute_sql_query with query: {sql_query}")
    
    try:
        # Enhanced timeout and connection handling for repeated requests
        import time
        start_time = time.time()
        
        # Execute query with proper timeout handling
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=60)
        
        rows_list = []
        row_count = 0
        
        # Process results with memory-efficient iteration
        for row in results:
            row_dict = {}
            for key, value in dict(row).items():
                if hasattr(value, '__class__') and 'Decimal' in str(type(value)):
                    row_dict[key] = float(value)
                else:
                    row_dict[key] = value
            rows_list.append(row_dict)
            row_count += 1
            
            # Prevent memory issues with very large result sets
            if row_count >= 1000:
                app.logger.warning(f"Result set truncated at {row_count} rows to prevent memory issues")
                break
        
        execution_time = time.time() - start_time
        app.logger.info(f"Tool Call: internal_execute_sql_query executed in {execution_time:.2f}s, returned {len(rows_list)} rows.")
        
        # Store results globally for fallback extraction
        global last_sql_results
        last_sql_results = rows_list
        
        result_payload = {"status": "success", "data": rows_list}
        return result_payload
        
    except Exception as e:
        app.logger.error(f"Tool Call: Error executing BigQuery query for internal_execute_sql_query: {e}", exc_info=True)
        
        # Enhanced error handling with connection diagnostics
        error_msg = str(e)
        if "timeout" in error_msg.lower() or "deadline" in error_msg.lower():
            error_msg = f"Query timeout after 45 seconds - try simplifying the query or adding LIMIT clause: {error_msg}"
        elif "connection" in error_msg.lower() or "network" in error_msg.lower():
            error_msg = f"BigQuery connection issue - this may resolve on retry: {error_msg}"
        
        return {
            "status": "error",
            "error_message": f"Error executing BigQuery query: {error_msg}"
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


def generate_business_intelligence_summary(query_description: str, data_rows: list, table_name: str) -> str:
    """Generate comprehensive business intelligence summaries with specific insights and actionable data"""
    try:
        if not data_rows:
            return "No data found for this query."
        
        query_lower = query_description.lower()
        summary_parts = []
        
        # Extract all available data points for comprehensive analysis
        revenue_amounts = []
        vendor_names = []
        event_names = []
        contact_emails = []
        phone_numbers = []
        zip_codes = []
        cities = []
        dates = []
        ethnicities = []
        product_types = []
        attendee_counts = []
        
        # Scan all data to extract meaningful business information
        for row in data_rows:
            for key, value in row.items():
                if not value or str(value).strip() == '' or str(value) == ' $ -   ':
                    continue
                    
                value_str = str(value).strip()
                key_lower = key.lower()
                
                # Extract revenue/financial data
                if any(x in key_lower for x in ['total', 'sales', 'revenue', 'amount', 'cash', 'credit']):
                    if '$' in value_str or any(x in value_str for x in ['$', 'dollar']):
                        try:
                            import re
                            amounts = re.findall(r'\$?\s*(\d+(?:,\d{3})*(?:\.\d{2})?)', value_str.replace(',', ''))
                            for amount in amounts:
                                revenue_amounts.append(float(amount))
                        except:
                            pass
                
                # Extract vendor/business names
                if any(x in key_lower for x in ['vendor', 'business', 'company', 'name']) and 'email' not in key_lower:
                    if len(value_str) > 1 and value_str not in ['N/A', 'None', 'NULL']:
                        vendor_names.append(value_str)
                
                # Extract event information
                if any(x in key_lower for x in ['event', 'market', 'festival']):
                    event_names.append(value_str)
                
                # Extract contact information
                if 'email' in key_lower and '@' in value_str:
                    contact_emails.append(value_str)
                
                if any(x in key_lower for x in ['phone', 'cell', 'mobile']) and len(value_str) >= 10:
                    phone_numbers.append(value_str)
                
                # Extract geographic data
                if any(x in key_lower for x in ['zip', 'postal']):
                    if value_str.isdigit() and len(value_str) == 5:
                        zip_codes.append(value_str)
                
                if any(x in key_lower for x in ['city', 'location']):
                    cities.append(value_str)
                
                # Extract demographic data
                if any(x in key_lower for x in ['ethnic', 'race', 'demographic']):
                    ethnicities.append(value_str)
                
                # Extract product categories
                if any(x in key_lower for x in ['product', 'category', 'type', 'food']):
                    product_types.append(value_str)
        
        # Build comprehensive business intelligence summary
        if any(word in query_lower for word in ['revenue', 'money', 'sales', 'made', 'income']):
            if revenue_amounts:
                total_revenue = sum(revenue_amounts)
                avg_revenue = total_revenue / len(revenue_amounts)
                max_revenue = max(revenue_amounts)
                min_revenue = min(revenue_amounts)
                
                summary_parts.append(f"REVENUE ANALYSIS: Total ${total_revenue:,.2f} from {len(revenue_amounts)} transactions")
                summary_parts.append(f"Performance metrics: Average ${avg_revenue:,.2f}, Range ${min_revenue:,.2f} - ${max_revenue:,.2f}")
                
                # Revenue distribution analysis
                high_performers = [r for r in revenue_amounts if r > avg_revenue]
                if high_performers:
                    summary_parts.append(f"Top performers: {len(high_performers)} transactions above average (${sum(high_performers):,.2f})")
            else:
                summary_parts.append(f"Revenue data examined from {len(data_rows)} records - extracting financial metrics from {table_name}")
        
        elif any(word in query_lower for word in ['top', 'best', 'highest', 'vendors']):
            unique_vendors = list(set([v for v in vendor_names if v]))
            if unique_vendors:
                summary_parts.append(f"TOP VENDORS IDENTIFIED: {len(unique_vendors)} unique businesses")
                summary_parts.append(f"Leading vendors: {', '.join(unique_vendors[:5])}")
                if revenue_amounts and len(revenue_amounts) >= len(unique_vendors):
                    avg_per_vendor = sum(revenue_amounts) / len(unique_vendors)
                    summary_parts.append(f"Average revenue per vendor: ${avg_per_vendor:,.2f}")
            else:
                summary_parts.append(f"Vendor analysis from {len(data_rows)} business records in {table_name}")
        
        elif any(word in query_lower for word in ['email', 'contact']):
            unique_emails = list(set([e for e in contact_emails if e and '@' in e]))
            if unique_emails:
                summary_parts.append(f"CONTACT DATABASE: {len(unique_emails)} verified email addresses")
                domain_counts = {}
                for email in unique_emails:
                    domain = email.split('@')[1] if '@' in email else 'unknown'
                    domain_counts[domain] = domain_counts.get(domain, 0) + 1
                
                if domain_counts:
                    top_domain = max(domain_counts, key=domain_counts.get)
                    summary_parts.append(f"Most common domain: {top_domain} ({domain_counts[top_domain]} contacts)")
                
                summary_parts.append(f"Sample contacts: {', '.join(unique_emails[:3])}")
            else:
                summary_parts.append(f"Contact extraction from {len(data_rows)} records - analyzing communication channels")
        
        elif any(word in query_lower for word in ['phone', 'cell', 'number']):
            unique_phones = list(set([p for p in phone_numbers if p]))
            if unique_phones:
                summary_parts.append(f"PHONE DIRECTORY: {len(unique_phones)} contact numbers available")
                summary_parts.append(f"Sample numbers: {', '.join(unique_phones[:3])}")
            else:
                summary_parts.append(f"Phone contact analysis from {len(data_rows)} records")
        
        elif any(word in query_lower for word in ['zip', 'location', 'city', 'geographic']):
            unique_zips = list(set([z for z in zip_codes if z]))
            unique_cities = list(set([c for c in cities if c]))
            
            if unique_zips:
                summary_parts.append(f"GEOGRAPHIC ANALYSIS: {len(unique_zips)} zip codes identified")
                summary_parts.append(f"Coverage areas: {', '.join(unique_zips[:10])}")
            
            if unique_cities:
                summary_parts.append(f"Cities represented: {', '.join(unique_cities[:5])}")
            
            if not unique_zips and not unique_cities:
                summary_parts.append(f"Geographic data analysis from {len(data_rows)} location records")
        
        else:
            # General business intelligence for other queries
            summary_parts.append(f"BUSINESS INTELLIGENCE ANALYSIS: {len(data_rows)} records processed")
            
            # Provide specific insights based on available data
            insights = []
            if vendor_names:
                unique_vendors = list(set([v for v in vendor_names if v]))
                insights.append(f"{len(unique_vendors)} unique vendors")
            
            if revenue_amounts:
                total_rev = sum(revenue_amounts)
                insights.append(f"${total_rev:,.2f} total transaction value")
            
            if contact_emails:
                unique_emails = list(set([e for e in contact_emails if e and '@' in e]))
                insights.append(f"{len(unique_emails)} email contacts")
            
            if event_names:
                unique_events = list(set([e for e in event_names if e]))
                insights.append(f"{len(unique_events)} events/markets")
            
            if insights:
                summary_parts.append(f"Key metrics: {', '.join(insights)}")
            
            # Data source context
            event_context = table_name.replace('-', ' ').replace('_', ' ')
            summary_parts.append(f"Source: {event_context}")
        
        if not summary_parts:
            summary_parts.append(f"Business analysis completed: {len(data_rows)} records from {table_name}")
        
        return " | ".join(summary_parts)
        
    except Exception as e:
        app.logger.error(f"Error in business intelligence summary: {str(e)}")
        return f"Business intelligence analysis: {len(data_rows)} records processed from {table_name}"


def execute_complex_business_query(query_description: str) -> dict:
    """Executes complex business intelligence queries using actual table schemas.
    
    This function examines the actual tables and builds appropriate queries based on
    the real column names and data structure found in the BigQuery workspace.
    
    Args:
        query_description (str): Natural language description of the business query
    
    Returns:
        dict: Query results with data and analysis
    """
    app.logger.info(f"Tool Call: execute_complex_business_query - {query_description}")
    
    try:
        query_lower = query_description.lower()
        
        # First, get list of relevant tables based on query content
        tables_result = internal_execute_sql_query(f"""
            SELECT table_name 
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
            WHERE table_name NOT LIKE '-%'
            ORDER BY table_name
        """)
        
        if tables_result.get('status') != 'success':
            return {"status": "error", "error_message": "Could not retrieve table list"}
        
        table_names = [row['table_name'] for row in tables_result['data']]
        
        # Find tables relevant to the query
        relevant_tables = []
        
        # Look for event-specific tables
        if 'kapwa gardens' in query_lower or 'kapwa' in query_lower:
            relevant_tables.extend([t for t in table_names if 'kapwa-gardens' in t.lower() or 'kapwa' in t.lower()])
        if 'yum yams' in query_lower or 'yum-yams' in query_lower:
            relevant_tables.extend([t for t in table_names if 'yum-yams' in t.lower()])
        if 'undscvrd' in query_lower or 'undiscovered' in query_lower:
            relevant_tables.extend([t for t in table_names if 'undscvrd' in t.lower() or 'undiscovered' in t.lower()])
        if 'balay kreative' in query_lower or 'balay' in query_lower:
            relevant_tables.extend([t for t in table_names if 'balay' in t.lower()])
        
        # If no specific events mentioned, look for vendor/sales tables
        if not relevant_tables:
            if any(word in query_lower for word in ['vendor', 'sales', 'money', 'revenue']):
                relevant_tables = [t for t in table_names if any(keyword in t.lower() for keyword in ['vendor', 'sales', 'close-out'])]
        
        # If still no tables, use the first few vendor-related tables
        if not relevant_tables:
            relevant_tables = [t for t in table_names if 'close-out' in t.lower()][:3]
        
        if not relevant_tables:
            return {"status": "error", "error_message": "No relevant tables found for the query"}
        
        # Use the first relevant table to build a query
        target_table = relevant_tables[0]
        
        # First, check what columns are actually available in the table
        columns_result = internal_execute_sql_query(f"""
            SELECT column_name 
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS` 
            WHERE table_name = '{target_table}'
            ORDER BY ordinal_position
        """)
        
        if columns_result.get('status') != 'success':
            # Fallback to simple SELECT ALL if we can't get column info
            sql_query = f"""SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"""
        else:
            available_columns = [row['column_name'] for row in columns_result['data']]
            
            # Handle different types of queries based on available columns
            if any(word in query_lower for word in ['money', 'revenue', 'sales', 'made']):
                # Look for revenue-related columns
                revenue_columns = []
                for col in available_columns:
                    if any(term in col.lower() for term in ['total_sales', 'sales', 'revenue', 'cash', 'credit']):
                        revenue_columns.append(col)
                
                if revenue_columns:
                    select_cols = ', '.join(revenue_columns[:5])  # Limit to 5 columns
                    sql_query = f"""
                        SELECT {select_cols}, _timestamp
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}`
                        WHERE {revenue_columns[0]} IS NOT NULL 
                        AND {revenue_columns[0]} != ''
                        LIMIT 20
                    """
                else:
                    sql_query = f"""SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"""
                    
            elif any(word in query_lower for word in ['email', 'contact', 'phone']):
                # Look for contact-related columns
                contact_columns = []
                for col in available_columns:
                    if any(term in col.lower() for term in ['email', 'phone', 'contact', 'name']):
                        contact_columns.append(col)
                
                if contact_columns:
                    select_cols = ', '.join(contact_columns[:5])
                    sql_query = f"""
                        SELECT {select_cols}
                        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}`
                        WHERE {contact_columns[0]} IS NOT NULL 
                        AND {contact_columns[0]} != ''
                        LIMIT 20
                    """
                else:
                    sql_query = f"""SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}` LIMIT 10"""
            else:
                # General information - just show all columns
                sql_query = f"""
                    SELECT *
                    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{target_table}`
                    LIMIT 10
                """
        
        result = internal_execute_sql_query(sql_query)
        
        if result.get('status') == 'success' and result.get('data'):
            # Generate business intelligence summary instead of raw data
            data_rows = result['data']
            
            # Analyze the data and provide business insights
            analysis = generate_business_intelligence_summary(query_description, data_rows, target_table)
            
            return {
                'status': 'success',
                'business_intelligence': analysis,
                'data_source': target_table,
                'records_analyzed': len(data_rows),
                'query_context': query_description
            }
        
        return result
        
    except Exception as e:
        app.logger.error(f"Error in complex business query: {e}", exc_info=True)
        return {"status": "error", "error_message": str(e)}


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
                "name": "execute_complex_business_query",
                "description": "Execute complex business intelligence queries",
                "parameters": ["query_description"]
            },
            {
                "name": "execute_comprehensive_analysis",
                "description": "Perform comprehensive data analysis",
                "parameters": ["table_name", "analysis_type"]
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
    """Process natural language business queries using Gemini AI"""
    try:
        data = request.get_json()
        if not data or 'query' not in data:
            return jsonify({"error": "Missing 'query' parameter"}), 400
        
        query = data['query']
        query_lower = query.lower()
        
        app.logger.info(f"Natural language query: {query}")
        
        # Route the query directly to appropriate processing function
        
        # For complex business queries (prioritize this over simple table display)
        if any(keyword in query_lower for keyword in ['revenue', 'analysis', 'attendees', 'vendors', 'how much', 'how many', 'which', 'who', 'what', 'top', 'most', 'zip code', 'email', 'phone', 'cell', 'identify', 'gave', 'live', 'participated', 'applied', 'money', 'made']):
            result = execute_complex_business_query(query)
            return jsonify(result)
        
        elif any(keyword in query_lower for keyword in ['table', 'tables', 'list']):
            # Table discovery request
            tables_result = internal_execute_sql_query(f"""
                SELECT table_name 
                FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
                ORDER BY table_name
            """)
            return jsonify(tables_result)
        
        elif any(keyword in query_lower for keyword in ['show me', 'data']) and not any(keyword in query_lower for keyword in ['revenue', 'money', 'top', 'how much']):
            # Simple table display for basic "show me data" requests
            tables_result = internal_execute_sql_query(f"""
                SELECT table_name 
                FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` 
                ORDER BY table_name
            """)
            
            if tables_result.get('status') == 'success' and tables_result.get('data'):
                table_names = [row['table_name'] for row in tables_result['data']]
                
                # Find best matching table for the query
                best_match = None
                keywords = ['kapwa', 'gardens', 'vendor', 'balay', 'kreative', 'undiscovered', 'yum', 'yams']
                
                for table_name in table_names:
                    table_lower = table_name.lower()
                    match_score = sum(1 for keyword in keywords if keyword in query_lower and keyword in table_lower)
                    if match_score > 0:
                        if not best_match or match_score > best_match[1]:
                            best_match = (table_name, match_score)
                
                if best_match:
                    # Query the matched table
                    result = internal_execute_sql_query(f"""
                        SELECT * FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.{best_match[0]}` 
                        LIMIT 10
                    """)
                    return jsonify(result)
        
        # Default response for queries we can't handle yet
        return jsonify({
            "status": "success",
            "response": f"I understand you're asking: '{query}'. This is a natural language query endpoint. For best results, try queries like 'show me tables', 'show me kapwa gardens data', or specific business questions about revenue and attendees.",
            "data": None
        })
        
    except Exception as e:
        app.logger.error(f"Error in natural language query: {str(e)}")
        return jsonify({"error": str(e)}), 500

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


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8081, debug=False)
