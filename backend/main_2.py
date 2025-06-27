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


def execute_complex_business_query(query_description: str) -> dict:
    """Executes complex business intelligence queries with multi-table joins, date ranges, and demographic filters.
    
    This function handles sophisticated queries like:
    - Vendor revenue analysis across events and date ranges
    - Attendee demographics and geographic analysis
    - Cross-event participation tracking
    - Financial performance metrics
    - Identity-based filtering and analysis
    
    Args:
        query_description (str): Natural language description of the business query
    
    Returns:
        dict: Query results with data and analysis
    """
    app.logger.info(f"Tool Call: execute_complex_business_query - {query_description}")
    
    try:
        # Parse query type and extract key components
        query_lower = query_description.lower()
        
        # Vendor-related queries
        if any(keyword in query_lower for keyword in ['vendor', 'vendors', 'seller', 'sellers']):
            return _handle_vendor_queries(query_description, query_lower)
        
        # Attendee/donor-related queries  
        elif any(keyword in query_lower for keyword in ['attendee', 'attendees', 'donor', 'donors', 'participant']):
            return _handle_attendee_queries(query_description, query_lower)
        
        # Revenue/financial queries
        elif any(keyword in query_lower for keyword in ['revenue', 'money', 'income', 'sales', 'profit', 'financial']):
            return _handle_financial_queries(query_description, query_lower)
        
        # Geographic queries
        elif any(keyword in query_lower for keyword in ['zip code', 'city', 'location', 'geographic', 'sf', 'daly city']):
            return _handle_geographic_queries(query_description, query_lower)
        
        # Event-specific queries
        elif any(keyword in query_lower for keyword in ['event', 'kapwa gardens', 'balay kreative', 'undscvrd', 'yum yams']):
            return _handle_event_queries(query_description, query_lower)
        
        # Default comprehensive analysis
        else:
            return _execute_general_analysis(query_description)
            
    except Exception as e:
        app.logger.error(f"Error in complex business query: {e}", exc_info=True)
        return {"status": "error", "error_message": str(e)}


def _get_queryable_tables() -> list:
    """Get list of queryable tables (all tables are VIEW type, excluding dash-prefixed names)."""
    try:
        tables_query = """
        SELECT table_name 
        FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` 
        WHERE table_name NOT LIKE '-%'
        ORDER BY table_name
        """
        result = internal_execute_sql_query(tables_query)
        if result.get('status') == 'success' and result.get('data'):
            return [row['table_name'] for row in result['data']]
        return []
    except Exception as e:
        app.logger.error(f"Error getting queryable tables: {e}")
        return []

def _extract_demographics(query_description: str, query_lower: str) -> dict:
    """Extract demographic identifiers from natural language queries."""
    demographics = {}
    
    # Identity-based patterns
    identity_patterns = {
        'middle eastern': ['middle eastern', 'middle-eastern', 'middle east'],
        'asian': ['asian', 'asian american', 'asian-american'],
        'latino': ['latino', 'latina', 'hispanic', 'latinx'],
        'black': ['black', 'african american', 'african-american'],
        'white': ['white', 'caucasian'],
        'native': ['native', 'native american', 'indigenous'],
        'pacific islander': ['pacific islander', 'hawaiian', 'pacific']
    }
    
    for identity, patterns in identity_patterns.items():
        if any(pattern in query_lower for pattern in patterns):
            demographics['identity'] = identity
            break
    
    # Gender patterns
    gender_patterns = {
        'female': ['female', 'woman', 'women'],
        'male': ['male', 'man', 'men'],
        'non-binary': ['non-binary', 'nonbinary', 'non binary']
    }
    
    for gender, patterns in gender_patterns.items():
        if any(pattern in query_lower for pattern in patterns):
            demographics['gender'] = gender
            break
    
    return demographics

def _extract_date_range(query_description: str) -> dict:
    """Extract date range from query descriptions."""
    import re
    
    # Look for year ranges like "2020-2023", "from 2021 to 2024"
    year_pattern = r'(\d{4})\s*[-to]\s*(\d{4})'
    match = re.search(year_pattern, query_description)
    
    if match:
        start_year, end_year = match.groups()
        return {
            'start_year': start_year,
            'end_year': end_year,
            'where_clause': f"AND EXTRACT(YEAR FROM PARSE_DATETIME('%Y-%m-%d', Order_Date)) BETWEEN {start_year} AND {end_year}"
        }
    
    # Look for single years
    single_year = re.search(r'in (\d{4})', query_description)
    if single_year:
        year = single_year.group(1)
        return {
            'year': year,
            'where_clause': f"AND EXTRACT(YEAR FROM PARSE_DATETIME('%Y-%m-%d', Order_Date)) = {year}"
        }
    
    return {}

def _extract_event_patterns(query_description: str, query_lower: str) -> dict:
    """Extract event names and patterns from queries."""
    events = []
    frequency = None
    
    # Event name patterns
    if 'kapwa gardens' in query_lower:
        events.append('Kapwa Gardens')
    if 'undscvrd' in query_lower or 'undiscovered' in query_lower:
        events.append('UNDSCVRD')
    if 'balay kreative' in query_lower:
        events.append('Balay Kreative')
    if 'yum yams' in query_lower:
        events.append('Yum Yams')
    
    # Frequency patterns
    if 'more than' in query_lower and ('time' in query_lower or 'x' in query_lower):
        import re
        freq_match = re.search(r'more than (\d+)', query_lower)
        if freq_match:
            frequency = int(freq_match.group(1))
    
    return {'events': events, 'frequency': frequency}

def _build_demographics_where_clause(demographics: dict) -> str:
    """Build WHERE clause from demographics data."""
    clauses = []
    
    if 'identity' in demographics:
        identity = demographics['identity']
        clauses.append(f"(LOWER(Identity) LIKE '%{identity.lower()}%' OR LOWER(Ethnicity) LIKE '%{identity.lower()}%')")
    
    if 'gender' in demographics:
        gender = demographics['gender']
        clauses.append(f"LOWER(Gender) LIKE '%{gender.lower()}%'")
    
    return ' AND '.join(clauses)

def _extract_income_threshold(query_description: str, query_lower: str) -> dict:
    """Extract income/sales thresholds from queries."""
    import re
    
    threshold = {}
    
    # Look for dollar amounts
    dollar_matches = re.findall(r'\$?(\d+(?:,\d{3})*(?:\.\d{2})?)', query_description)
    if dollar_matches:
        amount = float(dollar_matches[0].replace(',', ''))
        
        if 'more than' in query_lower or 'greater than' in query_lower or 'above' in query_lower:
            threshold['operator'] = '>'
            threshold['amount'] = amount
        elif 'less than' in query_lower or 'under' in query_lower or 'below' in query_lower:
            threshold['operator'] = '<'
            threshold['amount'] = amount
        elif 'at least' in query_lower or 'minimum' in query_lower:
            threshold['operator'] = '>='
            threshold['amount'] = amount
    
    return threshold

def _extract_contact_fields(query_lower: str) -> list:
    """Identify what contact information is being requested."""
    fields = []
    
    if 'email' in query_lower:
        fields.extend(['Email', 'email', 'email_address', 'Email_Address'])
    
    if any(term in query_lower for term in ['phone', 'cell', 'mobile', 'number']):
        fields.extend(['Phone', 'phone', 'cell_phone', 'Cell_Phone', 'Mobile', 'mobile', 'Phone_Number', 'phone_number'])
    
    return fields

def _extract_event_patterns(query_description: str, query_lower: str) -> dict:
    """Enhanced event pattern detection."""
    events = {}
    
    # Event name patterns
    event_patterns = {
        'kapwa gardens': ['kapwa gardens', 'kapwa-gardens', 'kapwa'],
        'balay kreative': ['balay kreative', 'balay-kreative', 'balay'],
        'undscvrd': ['undscvrd', 'undiscovered', 'undiscvrd'],
        'yum yams': ['yum yams', 'yum-yams', 'yumyams']
    }
    
    for event_name, patterns in event_patterns.items():
        if any(pattern in query_lower for pattern in patterns):
            if 'events' not in events:
                events['events'] = []
            events['events'].append(event_name)
    
    # Multi-event detection
    if len(events.get('events', [])) > 1:
        events['multi_event'] = True
    
    # Event frequency detection
    frequency_patterns = re.findall(r'more than (\d+)x|(\d+)\+ times|attended.*(\d+)', query_lower)
    if frequency_patterns:
        events['frequency_threshold'] = max([int(match[0] or match[1] or match[2]) for match in frequency_patterns if any(match)])
    
    return events

def _build_demographics_where_clause(demographics: dict) -> str:
    """Build SQL WHERE clause for demographic filtering."""
    clauses = []
    
    if 'identity' in demographics:
        identity = demographics['identity']
        # Try multiple potential field names for identity
        identity_clause = f"""
        (LOWER(COALESCE(Ethnicity, Race, Identity, Demographics, '')) LIKE '%{identity.lower()}%'
         OR LOWER(COALESCE(Race_Ethnicity, Demographic_Info, Background, '')) LIKE '%{identity.lower()}%')
        """
        clauses.append(identity_clause)
    
    if 'gender' in demographics:
        gender = demographics['gender']
        gender_clause = f"""
        (LOWER(COALESCE(Gender, gender, Sex, '')) LIKE '%{gender.lower()}%')
        """
        clauses.append(gender_clause)
    
    return ' AND ' + ' AND '.join(clauses) if clauses else ''

def _handle_vendor_queries(query_description: str, query_lower: str) -> dict:
    """Handle vendor-specific business intelligence queries with enhanced demographics and contact extraction."""
    
    # Get available queryable tables first to avoid hitting views
    queryable_tables = _get_queryable_tables()
    if not queryable_tables:
        return {"status": "error", "error_message": "Could not retrieve queryable tables"}
    
    # Filter tables for vendor-related data
    vendor_tables = [t for t in queryable_tables if any(keyword in t.lower() for keyword in ['vendor', 'close-out', 'sales'])]
    available_tables = _get_queryable_tables()
    if not available_tables:
        return {"status": "error", "error_message": "No queryable tables available"}
    
    # Use first relevant table for vendor queries (avoiding wildcards)
    vendor_table = None
    for table in available_tables:
        if any(keyword in table.lower() for keyword in ['vendor', 'attendees', 'orders', 'sales']):
            vendor_table = f"`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.{table}`"
            break
    
    if not vendor_table:
        vendor_table = f"`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.{available_tables[0]}`"
    
    # Extract date ranges and demographics
    date_range = _extract_date_range(query_description)
    demographics = _extract_demographics(query_description, query_lower)
    income_threshold = _extract_income_threshold(query_description, query_lower)
    
    # Revenue queries for vendors
    if 'money' in query_lower or 'revenue' in query_lower or 'sales' in query_lower:
        if 'event' in query_lower and ('date' in query_lower or any(year in query_description for year in ['2020', '2021', '2022', '2023', '2024'])):
            # "How much money was made by vendors at this event on this date"
            sql_query = f"""
            SELECT 
                Event_Name,
                Event_Date,
                SUM(CAST(Lineitem_price AS FLOAT64)) as total_vendor_revenue,
                COUNT(DISTINCT Order_ID) as total_orders,
                COUNT(*) as total_items
            FROM {vendor_table}
            WHERE Event_Date IS NOT NULL 
            AND Lineitem_price IS NOT NULL 
            AND Lineitem_price != ''
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Event_Name, Event_Date
            ORDER BY total_vendor_revenue DESC
            LIMIT 20
            """
            
        elif 'top' in query_lower and ('vendor' in query_lower or 'sellers' in query_lower):
            # "Who are the top 5 vendors from this event from date to date"  
            sql_query = f"""
            SELECT 
                Vendor_Name,
                Event_Name,
                SUM(CAST(Lineitem_price AS FLOAT64)) as vendor_revenue,
                COUNT(DISTINCT Order_ID) as orders_count,
                AVG(CAST(Lineitem_price AS FLOAT64)) as avg_order_value
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Vendor_Name IS NOT NULL
            AND Lineitem_price IS NOT NULL 
            AND Lineitem_price != ''
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Vendor_Name, Event_Name
            ORDER BY vendor_revenue DESC
            LIMIT 10
            """
            
        else:
            # General vendor revenue analysis
            sql_query = f"""
            SELECT 
                Event_Name,
                COUNT(DISTINCT Vendor_Name) as unique_vendors,
                SUM(CAST(Lineitem_price AS FLOAT64)) as total_revenue,
                AVG(CAST(Lineitem_price AS FLOAT64)) as avg_revenue_per_item
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Lineitem_price IS NOT NULL 
            AND Lineitem_price != ''
            GROUP BY Event_Name
            ORDER BY total_revenue DESC
            LIMIT 15
            """
    
    # Geographic vendor queries
    elif 'zip code' in query_lower or 'zip' in query_lower:
        sql_query = f"""
        SELECT 
            Billing_Zip_Code as vendor_zip,
            COUNT(DISTINCT Vendor_Name) as vendor_count,
            COUNT(*) as total_orders,
            SUM(CAST(Lineitem_price AS FLOAT64)) as total_revenue
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Billing_Zip_Code IS NOT NULL
        AND Billing_Zip_Code != ''
        {date_range['where_clause'] if date_range else ''}
        GROUP BY Billing_Zip_Code
        ORDER BY vendor_count DESC
        LIMIT 20
        """
    
    # Enhanced contact queries with demographics and income filtering
    elif any(contact_term in query_lower for contact_term in ['email', 'phone', 'cell', 'contact']):
        contact_fields = _extract_contact_fields(query_lower)
        demographics_clause = _build_demographics_where_clause(demographics)
        
        # Income threshold filtering for vendors
        income_clause = ""
        if income_threshold:
            income_clause = f" AND CAST(COALESCE(Lineitem_price, '0') AS FLOAT64) {income_threshold['operator']} {income_threshold['amount']}"
        
        # Build contact field selection
        contact_select = []
        if any('email' in field.lower() for field in contact_fields):
            contact_select.extend(['Email', 'email', 'Email_Address'])
        if any('phone' in field.lower() or 'cell' in field.lower() for field in contact_fields):
            contact_select.extend(['Phone', 'Cell_Phone', 'Mobile', 'Phone_Number'])
        
        if not contact_select:
            contact_select = ['Email']  # Default to email
        
        # Remove duplicates and create COALESCE for each field type
        email_fields = [f for f in contact_select if 'email' in f.lower()]
        phone_fields = [f for f in contact_select if any(term in f.lower() for term in ['phone', 'cell', 'mobile'])]
        
        select_clause = "Vendor_Name"
        if email_fields:
            select_clause += f", COALESCE({', '.join(email_fields)}) as contact_email"
        if phone_fields:
            select_clause += f", COALESCE({', '.join(phone_fields)}) as contact_phone"
        
        # Simplified GROUP BY for contact queries
        group_fields = ['Vendor_Name', 'Event_Name']
        if email_fields:
            group_fields.append('Email')
        if phone_fields:
            group_fields.append('Phone')
        
        sql_query = f"""
        SELECT DISTINCT
            {select_clause},
            Event_Name,
            SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_sales,
            COUNT(*) as transaction_count
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Vendor_Name IS NOT NULL
        {demographics_clause}
        {income_clause}
        {date_range['where_clause'] if date_range else ''}
        AND (Email IS NOT NULL OR Phone IS NOT NULL OR Cell_Phone IS NOT NULL)
        GROUP BY {', '.join(group_fields)}
        ORDER BY total_sales DESC
        LIMIT 50
        """
    
    # Multi-event participation queries
    elif 'kapwa gardens' in query_lower and 'undscvrd' in query_lower:
        sql_query = f"""
        WITH vendor_events AS (
            SELECT DISTINCT
                Vendor_Name,
                Event_Name,
                CASE 
                    WHEN LOWER(Event_Name) LIKE '%kapwa%' THEN 'Kapwa Gardens'
                    WHEN LOWER(Event_Name) LIKE '%undscvrd%' THEN 'UNDSCVRD'
                    ELSE 'Other'
                END as event_category,
                SUM(CAST(Lineitem_price AS FLOAT64)) as vendor_revenue
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Vendor_Name IS NOT NULL
            AND Lineitem_price IS NOT NULL
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Vendor_Name, Event_Name
        )
        SELECT 
            Vendor_Name,
            COUNT(DISTINCT event_category) as event_types_participated,
            SUM(vendor_revenue) as total_revenue,
            STRING_AGG(DISTINCT Event_Name, ', ') as events_list
        FROM vendor_events
        WHERE event_category IN ('Kapwa Gardens', 'UNDSCVRD')
        GROUP BY Vendor_Name
        HAVING COUNT(DISTINCT event_category) = 2
        AND SUM(vendor_revenue) >= 500
        ORDER BY total_revenue DESC
        """
    
    else:
        # Default vendor analysis
        sql_query = f"""
        SELECT 
            Vendor_Name,
            COUNT(DISTINCT Event_Name) as events_participated,
            SUM(CAST(Lineitem_price AS FLOAT64)) as total_revenue,
            COUNT(*) as total_items_sold
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Vendor_Name IS NOT NULL
        GROUP BY Vendor_Name
        ORDER BY total_revenue DESC
        LIMIT 20
        """
    
    return internal_execute_sql_query(sql_query)


def _handle_attendee_queries(query_description: str, query_lower: str) -> dict:
    """Handle attendee/donor-specific business intelligence queries with enhanced analytics."""
    
    date_range = _extract_date_range(query_description)
    demographics = _extract_demographics(query_description, query_lower)
    income_threshold = _extract_income_threshold(query_description, query_lower)
    event_patterns = _extract_event_patterns(query_description, query_lower)
    
    # Geographic attendee queries
    if 'zip code' in query_lower or 'city' in query_lower:
        if 'sf' in query_lower or 'san francisco' in query_lower:
            sql_query = f"""
            SELECT 
                Billing_City,
                Billing_Zip_Code,
                COUNT(*) as attendee_count,
                COUNT(DISTINCT Email) as unique_attendees
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE (LOWER(Billing_City) LIKE '%san francisco%' 
                OR LOWER(Billing_City) LIKE '%sf%'
                OR Billing_Zip_Code IN ('94102', '94103', '94104', '94105', '94107', '94108', '94109', '94110', '94111', '94112', '94114', '94115', '94116', '94117', '94118', '94121', '94122', '94123', '94124', '94127', '94129', '94130', '94131', '94132', '94133', '94134', '94158'))
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Billing_City, Billing_Zip_Code
            ORDER BY attendee_count DESC
            """
            
        elif 'daly city' in query_lower:
            sql_query = f"""
            SELECT 
                Billing_City,
                Billing_Zip_Code,
                COUNT(*) as attendee_count,
                COUNT(DISTINCT Email) as unique_attendees,
                STRING_AGG(DISTINCT Event_Name, ', ') as events_attended
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE (LOWER(Billing_City) LIKE '%daly city%'
                OR Billing_Zip_Code IN ('94014', '94015', '94016', '94017'))
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Billing_City, Billing_Zip_Code
            ORDER BY attendee_count DESC
            """
            
        else:
            # General geographic analysis
            sql_query = f"""
            SELECT 
                Billing_City as attendee_city,
                COUNT(*) as total_attendees,
                COUNT(DISTINCT Email) as unique_attendees,
                COUNT(DISTINCT Event_Name) as events_attended
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Billing_City IS NOT NULL
            AND Billing_City != ''
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Billing_City
            ORDER BY total_attendees DESC
            LIMIT 20
            """
    
    # Enhanced donation/giving queries with thresholds
    elif 'gave' in query_lower or 'donation' in query_lower or 'donor' in query_lower:
        demographics_clause = _build_demographics_where_clause(demographics)
        
        # Handle donation threshold queries like "gave more than $1"
        donation_clause = ""
        if income_threshold:
            donation_clause = f" AND CAST(COALESCE(Lineitem_price, '0') AS FLOAT64) {income_threshold['operator']} {income_threshold['amount']}"
        
        sql_query = f"""
        SELECT 
            Event_Name,
            Billing_City,
            COUNT(DISTINCT Email) as unique_donors,
            SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_donations,
            AVG(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as avg_donation,
            COUNT(*) as total_transactions
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Email IS NOT NULL
        {demographics_clause}
        {donation_clause}
        {date_range['where_clause'] if date_range else ''}
        GROUP BY Event_Name, Billing_City
        ORDER BY total_donations DESC
        LIMIT 50
        """
    
    # Grant application correlation queries
    elif 'grant' in query_lower or 'application' in query_lower:
        event_clause = ""
        if event_patterns.get('events'):
            event_names = "', '".join(event_patterns['events'])
            event_conditions = " OR ".join([f"LOWER(Event_Name) LIKE '%{name.lower()}%'" for name in event_patterns['events']])
            event_clause = f" AND ({event_conditions})"
        
        frequency_clause = ""
        if event_patterns.get('frequency_threshold'):
            frequency_clause = f" HAVING event_count > {event_patterns['frequency_threshold']}"
        
        demographics_clause = _build_demographics_where_clause(demographics)
        
        sql_query = f"""
        WITH grant_applicants AS (
            SELECT DISTINCT Email, Name, City, Demographics
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE (LOWER(Event_Name) LIKE '%grant%' 
                OR LOWER(Event_Name) LIKE '%application%'
                OR LOWER(Event_Name) LIKE '%balay%')
            {demographics_clause}
        ),
        event_attendance AS (
            SELECT 
                Email,
                COUNT(DISTINCT Event_Name) as event_count,
                STRING_AGG(DISTINCT Event_Name, ', ') as events_attended,
                MAX(Event_Date) as last_attendance
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Email IN (SELECT Email FROM grant_applicants)
            {event_clause}
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Email
            {frequency_clause}
        )
        SELECT 
            g.Email,
            g.Name,
            g.City,
            g.Demographics,
            e.event_count,
            e.events_attended,
            e.last_attendance
        FROM grant_applicants g
        LEFT JOIN event_attendance e ON g.Email = e.Email
        ORDER BY e.event_count DESC
        LIMIT 100
        """
    
    # Multi-event attendance queries
    elif any(event in query_lower for event in ['balay kreative', 'undscvrd', 'kapwa gardens']) and 'attended' in query_lower:
        demographics_clause = _build_demographics_where_clause(demographics)
        
        # Handle multi-event participation
        if len(event_patterns.get('events', [])) >= 2:
            events_list = [event.replace(' ', '%') for event in event_patterns['events']]
            
            sql_query = f"""
            WITH attendee_events AS (
                SELECT 
                    Email,
                    Name,
                    Billing_City,
                    Event_Name,
                    CASE 
                        WHEN LOWER(Event_Name) LIKE '%balay%' THEN 'Balay Kreative'
                        WHEN LOWER(Event_Name) LIKE '%undscvrd%' THEN 'UNDSCVRD'
                        WHEN LOWER(Event_Name) LIKE '%kapwa%' THEN 'Kapwa Gardens'
                        ELSE 'Other'
                    END as event_category
                FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
                WHERE Email IS NOT NULL
                {demographics_clause}
                {date_range['where_clause'] if date_range else ''}
            )
            SELECT 
                Email,
                Name,
                Billing_City,
                COUNT(DISTINCT event_category) as event_types_attended,
                STRING_AGG(DISTINCT Event_Name, ', ') as all_events,
                COUNT(DISTINCT Event_Name) as total_events_attended
            FROM attendee_events
            WHERE event_category IN ('Balay Kreative', 'UNDSCVRD', 'Kapwa Gardens')
            GROUP BY Email, Name, Billing_City
            HAVING COUNT(DISTINCT event_category) >= 2
            ORDER BY event_types_attended DESC, total_events_attended DESC
            LIMIT 100
            """
        else:
            # Single event attendance
            event_filter = event_patterns['events'][0] if event_patterns.get('events') else 'balay'
            
            sql_query = f"""
            SELECT 
                Email,
                Name,
                Billing_City,
                COUNT(DISTINCT Event_Name) as events_attended,
                STRING_AGG(DISTINCT Event_Name, ', ') as event_list,
                MAX(Event_Date) as last_attendance
            FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
            WHERE Email IS NOT NULL
            AND LOWER(Event_Name) LIKE '%{event_filter.lower()}%'
            {demographics_clause}
            {date_range['where_clause'] if date_range else ''}
            GROUP BY Email, Name, Billing_City
            ORDER BY events_attended DESC
            LIMIT 100
            """
    
    # Contact extraction for attendees
    elif any(contact_term in query_lower for contact_term in ['email', 'contact']):
        demographics_clause = _build_demographics_where_clause(demographics)
        city_filter = ""
        
        # City-specific filtering
        if 'daly city' in query_lower:
            city_filter = " AND (LOWER(Billing_City) LIKE '%daly city%' OR Billing_Zip_Code IN ('94014', '94015', '94016', '94017'))"
        elif any(city in query_lower for city in ['sf', 'san francisco']):
            city_filter = " AND (LOWER(Billing_City) LIKE '%san francisco%' OR LOWER(Billing_City) LIKE '%sf%')"
        
        sql_query = f"""
        SELECT DISTINCT
            Email,
            Name,
            Billing_City,
            Billing_Zip_Code,
            COUNT(DISTINCT Event_Name) as events_attended,
            STRING_AGG(DISTINCT Event_Name, ', ') as event_list
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Email IS NOT NULL
        AND Email LIKE '%@%'
        {demographics_clause}
        {city_filter}
        {date_range['where_clause'] if date_range else ''}
        GROUP BY Email, Name, Billing_City, Billing_Zip_Code
        ORDER BY events_attended DESC
        LIMIT 100
        """
    
    # Default attendee count and demographics
    else:
        demographics_clause = _build_demographics_where_clause(demographics)
        
        sql_query = f"""
        SELECT 
            Event_Name,
            COUNT(DISTINCT Email) as unique_attendees,
            COUNT(*) as total_registrations,
            SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_revenue,
            AVG(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as avg_transaction_value
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Email IS NOT NULL
        {demographics_clause}
        {date_range['where_clause'] if date_range else ''}
        GROUP BY Event_Name
        ORDER BY unique_attendees DESC
        LIMIT 50
        """
    
    return internal_execute_sql_query(sql_query)


def _handle_financial_queries(query_description: str, query_lower: str) -> dict:
    """Handle financial and revenue analysis queries."""
    date_range = _extract_date_range(query_description)
    
    sql_query = f"""
    SELECT 
        Event_Name,
        SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_revenue,
        COUNT(DISTINCT Email) as unique_customers,
        COUNT(*) as total_transactions,
        AVG(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as avg_transaction_value
    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
    WHERE Lineitem_price IS NOT NULL
    AND Lineitem_price != ''
    {date_range.get('where_clause', '')}
    GROUP BY Event_Name
    ORDER BY total_revenue DESC
    LIMIT 20
    """
    
    return internal_execute_sql_query(sql_query)


def _handle_geographic_queries(query_description: str, query_lower: str) -> dict:
    """Handle geographic and location-based queries."""
    date_range = _extract_date_range(query_description)
    
    sql_query = f"""
    SELECT 
        Billing_City,
        Billing_State,
        Billing_Zip_Code,
        COUNT(DISTINCT Email) as unique_residents,
        COUNT(*) as total_transactions,
        SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_spent
    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
    WHERE Billing_City IS NOT NULL
    AND Billing_City != ''
    {date_range.get('where_clause', '')}
    GROUP BY Billing_City, Billing_State, Billing_Zip_Code
    ORDER BY unique_residents DESC
    LIMIT 50
    """
    
    return internal_execute_sql_query(sql_query)


def _handle_event_queries(query_description: str, query_lower: str) -> dict:
    """Handle event-specific analysis queries."""
    date_range = _extract_date_range(query_description)
    event_patterns = _extract_event_patterns(query_description, query_lower)
    
    # Multi-event attendance queries
    if len(event_patterns.get('events', [])) >= 2:
        events = event_patterns['events']
        sql_query = f"""
        SELECT DISTINCT
            Email,
            First_Name,
            Last_Name,
            COUNT(DISTINCT Event_Name) as events_attended
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE Event_Name IN ('{"', '".join(events)}')
        {date_range.get('where_clause', '')}
        GROUP BY Email, First_Name, Last_Name
        HAVING events_attended >= 2
        ORDER BY events_attended DESC
        LIMIT 100
        """
    else:
        # Single event analysis
        sql_query = f"""
        SELECT 
            Event_Name,
            COUNT(DISTINCT Email) as unique_attendees,
            COUNT(*) as total_transactions,
            SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_revenue
        FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
        WHERE 1=1
        {date_range.get('where_clause', '')}
        GROUP BY Event_Name
        ORDER BY unique_attendees DESC
        LIMIT 50
        """
    
    return internal_execute_sql_query(sql_query)


def _execute_general_analysis(query_description: str, query_lower: str) -> dict:
    """Execute general data analysis queries."""
    sql_query = f"""
    SELECT 
        Event_Name,
        COUNT(DISTINCT Email) as unique_attendees,
        COUNT(*) as total_orders,
        SUM(CAST(COALESCE(Lineitem_price, '0') AS FLOAT64)) as total_revenue
    FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.*`
    WHERE Event_Name IS NOT NULL
    GROUP BY Event_Name
    ORDER BY total_revenue DESC
    LIMIT 20
    """
    
    return internal_execute_sql_query(sql_query)
