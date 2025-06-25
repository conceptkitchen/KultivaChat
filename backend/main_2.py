import os
import re  # For fallback logic - moved to top
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
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
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get(
    'GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')

# Add configuration variables for project and workspace IDs
GOOGLE_PROJECT_ID = os.environ.get('GOOGLE_PROJECT_ID', 'kbc-use4-839-261b')
KBC_WORKSPACE_ID = os.environ.get('KBC_WORKSPACE_ID', 'WORKSPACE_21894820')

# Log configuration status for debugging
app.logger.info("=== Configuration Check ===")
app.logger.info(f"KBC_API_URL: {'SET' if KBC_API_URL else 'MISSING'}")
app.logger.info(f"KBC_STORAGE_TOKEN: {'SET' if KBC_STORAGE_TOKEN else 'MISSING'}")
app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'MISSING'}")
app.logger.info(f"KBC_WORKSPACE_SCHEMA: {'SET' if KBC_WORKSPACE_SCHEMA else 'MISSING'}")
app.logger.info(f"GEMINI_API_KEY: {'SET' if GEMINI_API_KEY else 'MISSING'}")
app.logger.info(f"GOOGLE_PROJECT_ID: {GOOGLE_PROJECT_ID}")
app.logger.info(f"KBC_WORKSPACE_ID: {KBC_WORKSPACE_ID}")

# --- Define System Instruction Constant ---
SYSTEM_INSTRUCTION_PROMPT = f"""You are an expert Keboola Data Analyst Assistant, adept at understanding natural language requests for data. Your primary goal is to help users understand and retrieve insights from their data stored within a Keboola project. This project utilizes Keboola Storage (organized into 'buckets' containing 'tables') for source data, and crucially, a Google BigQuery data warehouse (project ID: `{GOOGLE_PROJECT_ID}`, dataset/workspace schema: `{KBC_WORKSPACE_ID}`) for querying transformed and analysis-ready data.

**MANDATORY EXECUTION RULE: For ANY request mentioning table data, you MUST use internal_execute_sql_query ONLY. Do NOT use list_keboola_buckets or list_tables_in_keboola_bucket. Use BigQuery tables directly.**

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

- **Complex Analytical Questions and Reporting** (e.g., "How much money was made by vendors at Yum Yams event?", "Top 5 vendors from an event between two dates?", "Attendees from specific Zip Codes who donated more than $X?", "Which vendors who identify as 'X' made more than 'Y' sales from 2020-2023?", "How many attendees live in SF and Daly City?"):
    1.  **Deconstruct the Request:** Identify key entities (e.g., 'vendors', 'attendees', 'donors', 'events' like 'Yum Yams', 'Kapwa Gardens', 'UNDSCVRD', 'Balay Kreative grants'), metrics (e.g., 'money made', 'counts', 'sales'), filters (e.g., dates, identity, location, monetary thresholds like 'more than $500', zip codes), and desired output (e.g., total sum, list of names/emails, top N ranking).
    2.  **Table Discovery & Schema Review (Iterative Process):**
        a.  Use `execute_sql_query` with `SELECT table_name FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES\`;` to list all tables in the BigQuery workspace.
        b.  From this list, identify 1-3 candidate tables that likely contain the required information. Use keywords from the user's query and common naming patterns. Examples:
            * For 'vendors', 'sales', 'money made': Look for tables like `DIM_VENDORS`, `FACT_SALES`, `EVENT_TRANSACTIONS`, `OUT_VENDOR_PERFORMANCE`.
            * For 'attendees', 'donors', 'zip code', 'city', 'emails': Look for `DIM_ATTENDEES`, `CRM_CONTACTS`, `DONATIONS_MASTER`, `OUT_USER_PROFILES`.
            * For 'events', 'dates', specific event names like 'Yum Yams', 'Kapwa Gardens', 'UNDSCVRD': Look for `DIM_EVENTS`, `EVENT_SCHEDULE`, or tables named after events e.g., `FACT_ORDERS_KAPWA_GARDENS`.
            * For 'identity' (e.g., demographic data): This might be in vendor or attendee profile tables.
            * For 'Balay Kreative grants' or applicants: Look for tables like `GRANT_APPLICATIONS`, `BALAY_APPLICANTS`.
        c.  Briefly inform the user of the primary table(s) you're investigating (e.g., "To find out about vendor sales at Yum Yams, I'll look into tables like `FACT_VENDOR_EVENT_SALES` and `DIM_EVENTS`.").
        d.  For these selected candidate tables, **you MUST retrieve their schemas** to identify correct column names and types. Use `execute_sql_query` with `SELECT table_name, column_name, data_type FROM \`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.COLUMNS\` WHERE table_name IN ('TABLE1_CANDIDATE', 'TABLE2_CANDIDATE');`.
    3.  **SQL Formulation Strategy (using the retrieved schemas):**
        a.  **JOINs:** Determine necessary JOINs between the selected tables using common key columns (e.g., `event_id`, `vendor_id`, `user_id`, `attendee_id`, `application_id`). Use `INNER JOIN` by default, unless `LEFT JOIN` is needed.
        b.  **Filtering (WHERE clause):** Construct precise `WHERE` clauses.
            * For text like event names, vendor names, identity categories: `WHERE event_name = 'Yum Yams'` or `WHERE demographic_category = 'X'`. Use `LIKE '%keyword%'` if partial matching is appropriate.
            * For dates/date ranges: `WHERE event_date = 'YYYY-MM-DD'` or `WHERE event_date BETWEEN 'YYYY-MM-DD' AND 'YYYY-MM-DD'`. Convert phrases like "from 201X to 202X" to `BETWEEN '201X-01-01' AND '202X-12-31'`.
            * For numerical thresholds: `WHERE sales_amount > 500` or `WHERE donations_total < 100`.
            * **For filtering by city names when the table only has a `zip_code` column (and no direct `city` column or internal city-zip mapping table is found):**
                1.  Use the `get_zip_codes_for_city` tool to get a list of zip codes for the target city. Provide a state code like 'CA' if it can be inferred or is commonly associated (e.g., for "San Francisco" or "Daly City").
                2.  If the tool returns a list of zip codes successfully, use them in your SQL: `WHERE zip_code IN ('zip1', 'zip2', ...)`
            * For other zip codes/cities (if city column exists): `WHERE zip_code = '94107'` or `LOWER(city) = 'san francisco'`.
            * For multiple conditions (e.g., participated in X AND Y events): This might involve subqueries, multiple JOINs, or `GROUP BY` and `HAVING COUNT(DISTINCT event_type) = 2`.
        c.  **Aggregations (GROUP BY):** For "how much," "how many," "total," "average," use `SUM(metric_column)`, `COUNT(DISTINCT id_column)`, `AVG(value_column)` often combined with `GROUP BY` categorical_columns.
        d.  **Ranking/Ordering (ORDER BY, LIMIT):** For "top N," "most," "least," use `ORDER BY metric_column DESC` (or `ASC`) and `LIMIT N`.
        e.  **Selecting Output Columns:** Select the columns the user asked for (e.g., names, emails, phone numbers, amounts).
    4.  **Execute Query:** Use `internal_execute_sql_query` with the fully constructed, complex SQL.
    5.  **Refinement (If Necessary for Complex Queries):** If your initial complex query attempt results in an error or clearly misses the mark (e.g., a crucial column was misidentified despite schema check, or `get_zip_codes_for_city` failed), you may briefly state what you tried and ask a *very specific* clarifying question related to the SQL construction or missing information. This is an exception to the "NEVER ask for confirmation" rule, strictly for iterative solving of complex analytical tasks *after* an initial automated attempt.

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
        2.  Call `execute_sql_query`.
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

keboola_storage_client = None
try:
    if KBC_API_URL and KBC_STORAGE_TOKEN:
        app.logger.info(
            f"Attempting to initialize Keboola Storage Client with URL: {KBC_API_URL}"
        )
        with timeout_context(30):  # 30 second timeout
            keboola_storage_client = KeboolaStorageClient(KBC_API_URL,
                                                          KBC_STORAGE_TOKEN)
        app.logger.info("Successfully initialized Keboola Storage Client.")
    else:
        app.logger.error(
            "CRITICAL (Keboola Client): KBC_API_URL or KBC_STORAGE_TOKEN not set."
        )
except TimeoutError:
    app.logger.error("Keboola Storage Client initialization timed out - continuing without it")
except Exception as e:
    app.logger.error(f"Error initializing Keboola Storage Client: {e}",
                     exc_info=True)

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
    app.logger.info(
        f"Tool Call: internal_execute_sql_query with query: {sql_query}")
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=60)
        rows_list = []
        for row in results:
            row_dict = {}
            for key, value in dict(row).items():
                if hasattr(value, '__class__') and 'Decimal' in str(
                        type(value)):
                    row_dict[key] = float(value)
                else:
                    row_dict[key] = value
            rows_list.append(row_dict)
        app.logger.info(
            f"Tool Call: internal_execute_sql_query executed, returned {len(rows_list)} rows."
        )
        
        # Store results globally for fallback extraction
        global last_sql_results
        last_sql_results = rows_list
        
        result_payload = {"status": "success", "data": rows_list}
        return result_payload
    except Exception as e:
        app.logger.error(
            f"Tool Call: Error executing BigQuery query for internal_execute_sql_query: {e}",
            exc_info=True)
        return {
            "status": "error",
            "error_message": f"Error executing BigQuery query: {str(e)}"
        }


def list_keboola_buckets() -> dict:
    """Lists all available top-level data categories (buckets) in the Keboola Storage project.
    Use this as a first step to understand the overall data landscape.

    Returns:
        dict: A dictionary containing 'status' and either 'data' (list of bucket objects) or 'error_message',
              and 'display_type', 'display_title' for successful calls.
    """
    if not keboola_storage_client:
        msg = "Keboola Storage client not initialized. Please check your Keboola API credentials."
        app.logger.error(f"Tool call list_keboola_buckets: {msg}")
        return {"status": "error", "error_message": msg}

    app.logger.info("Tool Call: list_keboola_buckets")
    try:
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = []
        for b in buckets_data or []:
            bucket_info.append({
                "id": b.get("id"),
                "name": b.get("name"),
                "stage": b.get("stage"),
                "description": b.get("description", "")
            })
        app.logger.info(
            f"Tool Call: list_keboola_buckets returned {len(bucket_info)} buckets."
        )
        return {
            "status": "success",
            "data": bucket_info,
            "display_type": "table",
            "display_title": "Keboola Storage Buckets"
        }
    except Exception as e:
        app.logger.error(f"Tool Call: Error listing Keboola buckets: {e}",
                         exc_info=True)
        return {
            "status": "error",
            "error_message": f"Error listing buckets: {str(e)}"
        }


def list_tables_in_keboola_bucket(bucket_id: str) -> dict:
    """Lists all specific datasets (tables) and their row counts within a chosen Keboola Storage bucket.

    Args:
        bucket_id (str): The ID of the Keboola Storage bucket (e.g., 'in.c-mydata' or 'out.c-transformeddata').

    Returns:
        dict: A dictionary containing 'status' and either 'data' (list of table objects) or 'error_message',
              and 'display_type', 'display_title' for successful calls.
    """
    if not keboola_storage_client:
        msg = "Keboola Storage client not initialized. Please check your Keboola API credentials."
        app.logger.error(f"Tool call list_tables_in_keboola_bucket: {msg}")
        return {"status": "error", "error_message": msg}

    app.logger.info(
        f"Tool Call: list_tables_in_keboola_bucket with bucket_id: {bucket_id}"
    )
    try:
        tables_data = keboola_storage_client.buckets.list_tables(
            bucket_id=bucket_id)
        table_info = []
        for t in tables_data or []:
            table_info.append({
                "id": t.get("id"),
                "name": t.get("name"),
                "rowsCount": t.get("rowsCount", 0),
                "primaryKey": t.get("primaryKey", [])
            })
        app.logger.info(
            f"Tool Call: list_tables_in_keboola_bucket returned {len(table_info)} tables."
        )
        return {
            "status": "success",
            "data": table_info,
            "display_type": "table",
            "display_title": f"Tables in Keboola Bucket: {bucket_id}"
        }
    except Exception as e:
        app.logger.error(
            f"Tool Call: Error listing tables in bucket {bucket_id}: {e}",
            exc_info=True)
        return {
            "status":
            "error",
            "error_message":
            f"Error listing tables in bucket {bucket_id}: {str(e)}"
        }


def get_keboola_table_detail(table_id: str) -> dict:
    """Retrieves the detailed schema (column names, data types) and metadata for a specific Keboola Storage table.

    Args:
        table_id (str): The full ID of the Keboola Storage table (e.g., 'in.c-mybucket.mytable').

    Returns:
        dict: A dictionary containing 'status' and either 'data' (table detail object wrapped in a list) or 'error_message',
              and 'display_type', 'display_title' for successful calls.
    """
    if not keboola_storage_client:
        msg = "Keboola Storage client not initialized. Please check your Keboola API credentials."
        app.logger.error(f"Tool call get_keboola_table_detail: {msg}")
        return {"status": "error", "error_message": msg}

    app.logger.info(
        f"Tool Call: get_keboola_table_detail with table_id: {table_id}")
    try:
        table_detail_response = keboola_storage_client.tables.detail(table_id)
        app.logger.info(
            f"Table detail response type: {type(table_detail_response)}, content snippet: {str(table_detail_response)[:200]}"
        )

        if not isinstance(table_detail_response, dict):
            app.logger.error(
                f"API returned unexpected response type: {type(table_detail_response)}. Full response: {table_detail_response}"
            )
            return {
                "status":
                "error",
                "error_message":
                f"API returned unexpected response type for table details: {type(table_detail_response)}"
            }

        columns_info = []
        if 'definition' in table_detail_response and isinstance(
                table_detail_response['definition'].get('columns'), list):
            app.logger.info(
                "Parsing columns from table_detail.definition.columns")
            for col_def in table_detail_response['definition']['columns']:
                columns_info.append({
                    "name":
                    col_def.get("name"),
                    "type":
                    col_def.get("baseType", col_def.get("type", "unknown"))
                })
        elif table_detail_response.get("columns") and isinstance(
                table_detail_response.get("columns"),
                list) and table_detail_response.get("attributes"):
            app.logger.info(
                "Parsing columns by matching names with attributes list.")
            column_names = table_detail_response.get("columns", [])
            attributes = {
                attr.get("name"): attr
                for attr in table_detail_response.get("attributes", [])
            }
            for name in column_names:
                attr = attributes.get(name, {})
                columns_info.append({
                    "name": name,
                    "type": attr.get("type", "unknown")
                })
        elif table_detail_response.get("columns") and isinstance(
                table_detail_response.get("columns"), list):
            app.logger.warning(
                f"Only column names found for table {table_id}, type information might be missing."
            )
            for col_name in table_detail_response.get("columns", []):
                columns_info.append({"name": col_name, "type": "unknown"})

        detail_info_data = {
            "id": table_detail_response.get("id"),
            "name": table_detail_response.get("name"),
            "rowsCount": table_detail_response.get("rowsCount", 0),
            "columns": columns_info,
            "primaryKey": table_detail_response.get("primaryKey", [])
        }

        app.logger.info(
            f"Tool Call: get_keboola_table_detail returned details for table {table_id}."
        )
        return {
            "status": "success",
            "data": [detail_info_data],
            "display_type": "table_detail",
            "display_title": f"Schema for Keboola Table: {table_id}"
        }
    except Exception as e:
        app.logger.error(
            f"Tool Call: Error getting table detail for {table_id}: {e}",
            exc_info=True)
        return {
            "status": "error",
            "error_message":
            f"Error getting table detail for {table_id}: {str(e)}"
        }


def get_current_time() -> dict:
    """Returns the current date, time, and timezone.
    Returns:
        dict: A dictionary containing the current time string with a key 'current_time' and 'status'.
    """
    app.logger.info("Tool Call: get_current_time")
    current_time_str = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    return {"status": "success", "current_time": current_time_str}


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


def execute_comprehensive_analysis(table_name: str, analysis_type: str = "overview") -> dict:
    """Performs comprehensive data analysis on a specific table with advanced insights.
    
    Args:
        table_name (str): The full table name (e.g., 'Balay-Kreative---attendees---all-orders')
        analysis_type (str): Type of analysis - 'overview', 'trends', 'aggregations', 'detailed'
    
    Returns:
        dict: Comprehensive analysis results with multiple data perspectives
    """
    app.logger.info(f"Tool Call: execute_comprehensive_analysis for {table_name}, type: {analysis_type}")
    
    try:
        full_table_name = f"`{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.{table_name}`"
        
        if analysis_type == "overview":
            # Get table structure and sample data
            queries = [
                f"SELECT COUNT(*) as total_rows FROM {full_table_name}",
                f"SELECT * FROM {full_table_name} LIMIT 50",
                f"SELECT column_name, data_type FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_SCHEMA}.INFORMATION_SCHEMA.COLUMNS` WHERE table_name = '{table_name}'"
            ]
        elif analysis_type == "trends":
            # Time-based analysis
            queries = [
                f"SELECT Event_Date, COUNT(*) as count FROM {full_table_name} GROUP BY Event_Date ORDER BY Event_Date",
                f"SELECT Event_Name, COUNT(*) as attendees FROM {full_table_name} GROUP BY Event_Name ORDER BY attendees DESC LIMIT 20"
            ]
        elif analysis_type == "aggregations":
            # Financial and statistical analysis
            queries = [
                f"SELECT SUM(CAST(Lineitem_price AS FLOAT64)) as total_revenue, AVG(CAST(Lineitem_price AS FLOAT64)) as avg_price FROM {full_table_name} WHERE Lineitem_price IS NOT NULL AND Lineitem_price != ''",
                f"SELECT Event_Name, SUM(CAST(Lineitem_price AS FLOAT64)) as event_revenue FROM {full_table_name} WHERE Lineitem_price IS NOT NULL AND Lineitem_price != '' GROUP BY Event_Name ORDER BY event_revenue DESC"
            ]
        elif analysis_type == "detailed":
            # Comprehensive dataset
            queries = [f"SELECT * FROM {full_table_name} LIMIT 500"]
        
        results = []
        for query in queries:
            result = internal_execute_sql_query(query)
            if result.get('status') == 'success':
                results.append({
                    'query': query,
                    'data': result.get('data', []),
                    'row_count': len(result.get('data', []))
                })
        
        return {
            "status": "success",
            "analysis_type": analysis_type,
            "table_name": table_name,
            "results": results
        }
        
    except Exception as e:
        app.logger.error(f"Error in comprehensive analysis: {e}", exc_info=True)
        return {"status": "error", "error_message": str(e)}


gemini_tool_functions_list = [
    internal_execute_sql_query,
    execute_comprehensive_analysis,
    get_zip_codes_for_city,
    get_current_time
]

# --- Initialize Gemini Client (using genai.Client and GenerateContentConfig) ---
gemini_sdk_client = None
gemini_generation_config_with_tools = None

if GEMINI_API_KEY and GEMINI_SDK_AVAILABLE:
    try:
        app.logger.info("Initializing google.genai.Client with API key...")
        gemini_sdk_client = google_genai_for_client.Client(
            api_key=GEMINI_API_KEY)
        app.logger.info("Successfully initialized google.genai.Client.")

        app.logger.info(
            f"Defining tools for Gemini: {[f.__name__ for f in gemini_tool_functions_list]}"
        )
        gemini_generation_config_with_tools = google_genai_types.GenerateContentConfig(
            tools=gemini_tool_functions_list, )
        app.logger.info(
            "Gemini GenerateContentConfig with tools created successfully.")

    except Exception as e:
        app.logger.error(
            f"Error initializing Gemini client or GenerateContentConfig: {e}",
            exc_info=True)
        gemini_sdk_client = None
        gemini_generation_config_with_tools = None
else:
    if not GEMINI_API_KEY:
        app.logger.error("CRITICAL (Gemini): GEMINI_API_KEY not set.")
    if not GEMINI_SDK_AVAILABLE:
        app.logger.error(
            "CRITICAL (Gemini): SDK 'google.genai' not available.")


# --- API Endpoints ---
@app.route('/')
def hello():
    return "Hello from your custom Keboola API Gateway (using genai.Client)!"

@app.route('/api/health')
def health():
    return jsonify({"status": "healthy", "backend": "running"})

# --- Frontend Compatibility Endpoints ---
@app.route('/api/user', methods=['GET'])
def get_user():
    # Simple mock user for frontend compatibility
    return jsonify({
        "id": "user-1",
        "username": "user",
        "email": "user@kultivate.ai",
        "firstName": "Kultivate",
        "lastName": "User"
    })

@app.route('/api/conversations', methods=['GET'])
def get_conversations():
    # Fetch actual conversations from database using SQL execution tool
    try:
        # Connect to the same database system used by the Node.js server
        import subprocess
        import json
        import os
        
        database_url = os.getenv('DATABASE_URL')
        if not database_url:
            app.logger.error("DATABASE_URL not found")
            return jsonify([])
        
        query = "SELECT id, title, created_at, updated_at FROM conversations WHERE user_id = 1 ORDER BY updated_at DESC"
        
        # Use psql to execute the query
        result = subprocess.run([
            'psql', database_url, '-c', query, '-t', '--csv'
        ], capture_output=True, text=True)
        
        app.logger.info(f"Get conversations - return code: {result.returncode}")
        app.logger.info(f"Get conversations - stdout: {result.stdout}")
        app.logger.info(f"Get conversations - stderr: {result.stderr}")
        
        conversations = []
        if result.returncode == 0 and result.stdout.strip():
            lines = result.stdout.strip().split('\n')
            for line in lines:
                if line.strip():
                    parts = line.split(',')
                    if len(parts) >= 4:
                        conversations.append({
                            "id": parts[0].strip(),
                            "title": parts[1].strip(),
                            "createdAt": parts[2].strip() + "Z" if parts[2].strip() else None,
                            "updatedAt": parts[3].strip() + "Z" if parts[3].strip() else None
                        })
        
        app.logger.info(f"Retrieved {len(conversations)} conversations from database")
        return jsonify(conversations)
        
    except Exception as e:
        app.logger.error(f"Error fetching conversations: {e}")
        return jsonify([])

@app.route('/api/conversations', methods=['POST'])
def create_conversation():
    # Create a new conversation in the database using SQL execution
    try:
        import subprocess
        import uuid
        import os
        from datetime import datetime
        
        data = request.get_json() or {}
        title = data.get('title', 'New Conversation')
        conv_id = str(uuid.uuid4())
        now = datetime.utcnow().isoformat()
        
        database_url = os.getenv('DATABASE_URL')
        if not database_url:
            return jsonify({"error": "Database connection failed"}), 500
        
        # SQL query to insert new conversation - escape single quotes properly
        escaped_title = title.replace("'", "''")
        query = f"INSERT INTO conversations (id, user_id, title, created_at, updated_at) VALUES ('{conv_id}', 1, '{escaped_title}', '{now}', '{now}') RETURNING id, title, created_at, updated_at;"
        
        app.logger.info(f"Executing SQL: {query}")
        
        # Execute using psql
        result = subprocess.run([
            'psql', database_url, '-c', query, '-t', '--csv'
        ], capture_output=True, text=True)
        
        app.logger.info(f"psql return code: {result.returncode}")
        app.logger.info(f"psql stdout: {result.stdout}")
        app.logger.info(f"psql stderr: {result.stderr}")
        
        if result.returncode == 0 and result.stdout.strip():
            lines = result.stdout.strip().split('\n')
            # Take first line which contains the data, ignore INSERT statement
            data_line = lines[0] if lines else ""
            parts = data_line.split(',')
            if len(parts) >= 4:
                conversation = {
                    "id": parts[0].strip(),
                    "title": parts[1].strip(),
                    "createdAt": parts[2].strip() + "Z",
                    "updatedAt": parts[3].strip() + "Z"
                }
                app.logger.info(f"Created new conversation: {conv_id}")
                return jsonify(conversation)
        
        app.logger.error(f"Failed to create conversation - return code: {result.returncode}, stderr: {result.stderr}")
        return jsonify({"error": "Failed to create conversation"}), 500
        
    except Exception as e:
        app.logger.error(f"Error creating conversation: {e}")
        return jsonify({"error": "Failed to create conversation"}), 500

@app.route('/api/conversations/<conversation_id>', methods=['GET'])
def get_conversation(conversation_id):
    # Fetch specific conversation with messages from database
    try:
        import subprocess
        import os
        
        database_url = os.getenv('DATABASE_URL')
        if not database_url:
            return jsonify({"error": "Database connection failed"}), 500
        
        # First get conversation details
        conv_query = f"SELECT id, title, created_at, updated_at FROM conversations WHERE id = '{conversation_id}' AND user_id = 1"
        conv_result = subprocess.run([
            'psql', database_url, '-c', conv_query, '-t', '--csv'
        ], capture_output=True, text=True)
        
        if conv_result.returncode != 0 or not conv_result.stdout.strip():
            return jsonify({"error": "Conversation not found"}), 404
        
        conv_parts = conv_result.stdout.strip().split(',')
        if len(conv_parts) < 4:
            return jsonify({"error": "Conversation not found"}), 404
        
        # Get messages for this conversation
        msg_query = f"SELECT id, role, content, displays, timestamp FROM messages WHERE conversation_id = '{conversation_id}' ORDER BY timestamp ASC"
        msg_result = subprocess.run([
            'psql', database_url, '-c', msg_query, '-t', '--csv'
        ], capture_output=True, text=True)
        
        messages = []
        if msg_result.returncode == 0 and msg_result.stdout.strip():
            lines = msg_result.stdout.strip().split('\n')
            for line in lines:
                if line.strip():
                    # Handle CSV with proper quoting for complex content
                    parts = []
                    current_part = ""
                    in_quotes = False
                    for char in line:
                        if char == '"' and not in_quotes:
                            in_quotes = True
                        elif char == '"' and in_quotes:
                            in_quotes = False
                        elif char == ',' and not in_quotes:
                            parts.append(current_part.strip('"'))
                            current_part = ""
                            continue
                        current_part += char
                    parts.append(current_part.strip('"'))
                    
                    if len(parts) >= 5:
                        try:
                            displays = json.loads(parts[3]) if parts[3] and parts[3] != '\\N' else []
                        except:
                            displays = []
                        
                        messages.append({
                            "id": parts[0].strip(),
                            "role": parts[1].strip(),
                            "content": parts[2].strip(),
                            "displays": displays,
                            "timestamp": parts[4].strip() + "Z" if parts[4].strip() != '\\N' else None
                        })
        
        conversation = {
            "id": conv_parts[0].strip(),
            "title": conv_parts[1].strip(),
            "createdAt": conv_parts[2].strip() + "Z" if conv_parts[2].strip() != '\\N' else None,
            "updatedAt": conv_parts[3].strip() + "Z" if conv_parts[3].strip() != '\\N' else None,
            "messages": messages
        }
        
        return jsonify(conversation)
        
    except Exception as e:
        app.logger.error(f"Error fetching conversation {conversation_id}: {e}")
        return jsonify({"error": "Failed to fetch conversation"}), 500

@app.route('/api/conversations/<conversation_id>/messages', methods=['GET'])
def get_conversation_messages(conversation_id):
    # Return empty messages for new conversations
    return jsonify([])

@app.route('/api/conversations/<conversation_id>/messages', methods=['POST'])
def send_message_to_conversation(conversation_id):
    # This endpoint handles the actual chat functionality with database persistence
    try:
        import psycopg2
        import uuid
        import os
        from datetime import datetime
        
        # Get the request data
        request_data = request.get_json()
        if not request_data or 'message' not in request_data:
            return jsonify({"error": "Missing 'message' in JSON payload."}), 400

        user_content = request_data['message']
        
        # Get existing conversation and messages from database
        database_url = os.getenv('DATABASE_URL')
        if not database_url:
            return jsonify({"error": "Database connection failed"}), 500
            
        conn = psycopg2.connect(database_url)
        cur = conn.cursor()
        
        # Verify conversation exists
        cur.execute("SELECT id FROM conversations WHERE id = %s AND user_id = 1", (conversation_id,))
        if not cur.fetchone():
            cur.close()
            conn.close()
            return jsonify({"error": "Conversation not found"}), 404
        
        # Get conversation history from database
        cur.execute("""
            SELECT role, content FROM messages 
            WHERE conversation_id = %s 
            ORDER BY timestamp ASC
        """, (conversation_id,))
        
        conversation_history = []
        for row in cur.fetchall():
            conversation_history.append({
                'role': row[0],
                'content': row[1]
            })
        
        # Convert to the format expected by the existing chat handler
        chat_request = {
            'message': user_content,
            'conversation_history': conversation_history
        }
        
        # Call the existing chat functionality
        if not gemini_sdk_client or not gemini_generation_config_with_tools:
            cur.close()
            conn.close()
            return jsonify({"error": "Chat service not available"}), 500
        
        # Use existing chat logic but return in frontend-expected format
        user_message_text = chat_request['message']
        conversation_history = chat_request.get('conversation_history', [])
        
        app.logger.info(f"Received chat message for conversation {conversation_id}: {user_message_text}")
        
        # Create chat session (simplified version of existing logic)
        full_history = []
        if GEMINI_SDK_AVAILABLE:
            full_history = [
                google_genai_types.Content(
                    role="user",
                    parts=[google_genai_types.Part(text=SYSTEM_INSTRUCTION_PROMPT)]
                ),
                google_genai_types.Content(
                    role="model", 
                    parts=[google_genai_types.Part(text="Understood. I am ready to assist you with your Keboola project data. How can I help you?")]
                )
            ]
        
        chat_session = gemini_sdk_client.chats.create(
            model='gemini-2.0-flash',
            config=gemini_generation_config_with_tools,
            history=full_history
        )
        
        response = chat_session.send_message(user_message_text)
        
        # Extract response text
        final_answer = ""
        try:
            if hasattr(response, 'text') and response.text:
                final_answer = response.text
            else:
                final_answer = "I found your data tables and they're displayed below."
        except (ValueError, AttributeError):
            final_answer = "I found your data tables and they're displayed below."
        
        # Extract any displays from tool responses
        displays = []
        if GEMINI_SDK_AVAILABLE:
            try:
                retrieved_history = chat_session.get_history()
                for message_content in reversed(retrieved_history):
                    for part in message_content.parts:
                        if hasattr(part, 'function_response') and part.function_response:
                            func_result = part.function_response.response
                            if isinstance(func_result, dict):
                                if func_result.get('display_type') == 'table' and func_result.get('data'):
                                    displays.append({
                                        'type': 'table',
                                        'title': func_result.get('display_title', 'Query Results'),
                                        'content': func_result['data']
                                    })
                            break
            except Exception as e:
                app.logger.error(f"Error extracting displays: {e}")
        
        # Save messages to database and return in frontend format
        user_msg_id = str(uuid.uuid4())
        assistant_msg_id = str(uuid.uuid4())
        now = datetime.utcnow().isoformat()
        
        # Save user message to database
        escaped_user_content = user_content.replace("'", "''")
        user_msg_query = f"INSERT INTO messages (id, conversation_id, role, content, timestamp) VALUES ('{user_msg_id}', '{conversation_id}', 'user', '{escaped_user_content}', '{now}')"
        subprocess.run(['psql', database_url, '-c', user_msg_query], capture_output=True)
        
        # Save assistant message to database
        escaped_final_answer = final_answer.replace("'", "''")
        displays_json = json.dumps(displays).replace("'", "''")
        assistant_msg_query = f"INSERT INTO messages (id, conversation_id, role, content, displays, timestamp) VALUES ('{assistant_msg_id}', '{conversation_id}', 'assistant', '{escaped_final_answer}', '{displays_json}', '{now}')"
        subprocess.run(['psql', database_url, '-c', assistant_msg_query], capture_output=True)
        
        # Update conversation updated_at timestamp
        update_conv_query = f"UPDATE conversations SET updated_at = '{now}' WHERE id = '{conversation_id}'"
        subprocess.run(['psql', database_url, '-c', update_conv_query], capture_output=True)
        
        # Update conversation title if it's the first user message
        if len(conversation_history) == 0:  # This was the first message
            title = user_content[:30] + "..." if len(user_content) > 30 else user_content
            escaped_title = title.replace("'", "''")
            title_query = f"UPDATE conversations SET title = '{escaped_title}' WHERE id = '{conversation_id}'"
            subprocess.run(['psql', database_url, '-c', title_query], capture_output=True)
        
        # Only create display for explicit table requests, not casual mentions
        app.logger.info(f"Pre-emergency check: displays={len(displays)}, user_message='{user_message_text}', final_answer preview='{final_answer[:50] if final_answer else 'None'}'")
        explicit_table_requests = [
            "show me my tables", "list tables", "what tables do i have", 
            "show me my data tables", "display tables", "available tables"
        ]
        if not displays and any(phrase in user_message_text.lower() for phrase in explicit_table_requests):
            app.logger.info("Triggering emergency display creation for explicit table request...")
            try:
                direct_result = internal_execute_sql_query("SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name")
                app.logger.info(f"Direct query result: {direct_result}")
                if direct_result.get('status') == 'success' and direct_result.get('data'):
                    displays = [{
                        "type": "table",
                        "title": "Available Data Tables",
                        "content": direct_result['data']
                    }]
                    app.logger.info(f"Emergency display creation successful with {len(direct_result['data'])} rows")
                    final_answer = "Here are your available data tables:"
            except Exception as e:
                app.logger.error(f"Emergency display creation failed: {e}")
        
        app.logger.info(f"Final response - displays count: {len(displays)}, displays: {displays}")
        
        # Return only the assistant response - frontend handles user message display
        return jsonify({
            "reply": final_answer,
            "displays": displays
        })
        
    except Exception as e:
        app.logger.error(f"Error in conversation message handler: {e}", exc_info=True)
        return jsonify({"error": "Failed to process message"}), 500

@app.route('/api/logout', methods=['POST'])
def logout():
    # Simple logout endpoint for frontend compatibility
    return jsonify({"status": "success"})


@app.route('/api/list_buckets', methods=['GET'])
def list_keboola_buckets_endpoint_direct():
    result = list_keboola_buckets()
    if result.get("status") == "error":
        return jsonify(result), 500
    return jsonify(result.get("data"))


@app.route('/api/buckets/<path:bucket_id>/tables', methods=['GET'])
def list_tables_in_bucket_endpoint_direct(bucket_id):
    result = list_tables_in_keboola_bucket(bucket_id=bucket_id)
    if result.get("status") == "error":
        return jsonify(result), 500
    return jsonify(result.get("data"))


@app.route('/api/query_data', methods=['POST'])
def query_data_endpoint():
    request_data = request.get_json()
    if not request_data or 'sql_query' not in request_data:
        return jsonify({"error": "Missing 'sql_query' in JSON payload."}), 400
    sql_query = request_data['sql_query']
    result = internal_execute_sql_query(sql_query)
    if isinstance(result,
                  dict) and result.get("status") != "success" and result.get(
                      "status") != "success_truncated":
        return jsonify(result), 500
    return jsonify(result)

@app.route('/api/execute_sql', methods=['POST'])
def execute_sql_direct():
    """Direct SQL execution for API consumers"""
    try:
        data = request.json
        sql_query = data.get('sql_query')
        credentials = data.get('credentials', {})
        
        if not sql_query:
            return jsonify({'status': 'error', 'error_message': 'SQL query required'})
        
        # Update environment with provided credentials if available
        if credentials:
            for key, value in credentials.items():
                if value:  # Only set non-empty values
                    os.environ[key] = str(value)
        
        # Execute SQL using existing function
        result = internal_execute_sql_query(sql_query)
        
        return jsonify(result)
        
    except Exception as e:
        return jsonify({
            'status': 'error',
            'error_message': str(e)
        })


# --- CHAT ENDPOINT using genai.Client and Automatic Function Calling ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini_client_style():
    if not gemini_sdk_client or not gemini_generation_config_with_tools:
        app.logger.error(
            "/api/chat called but Gemini client or tool config is not initialized."
        )
        return jsonify({
            "error":
            "Gemini client/config not initialized. Check server logs."
        }), 500

    try:
        user_message_data = request.get_json()
        if not user_message_data or 'message' not in user_message_data:
            return jsonify({"error":
                            "Missing 'message' in JSON payload."}), 400

        user_message_text = user_message_data['message']
        conversation_history = user_message_data.get('conversation_history',
                                                     [])
        app.logger.info(
            f"Received user message for Gemini (genai.Client): {user_message_text}"
        )
        app.logger.info(
            f"Conversation history length: {len(conversation_history)}")

        # Check for auto-execution opportunity before sending to AI
        auto_result = auto_execute_table_query(user_message_text, conversation_history)
        if auto_result:
            app.logger.info(f"Auto-executed query for table: {auto_result['table_used']}")
            
            # Format response like AI would
            displays = []
            if auto_result['result'].get('data'):
                displays.append({
                    'type': 'table',
                    'title': 'SQL Query Results',
                    'content': auto_result['result']['data']
                })
            
            reply_text = f"Here's the data from `{auto_result['table_used']}` table:"
            
            return jsonify({
                'reply': reply_text,
                'displays': displays
            })

        # Check if user is asking for more data, different table, or previous results
        more_data_keywords = ['30 more', '30 records', 'more records', 'show more', 'load more']
        show_results_keywords = ['show me the query results', 'show query results', 'display the results', 'show the results', 'display results']
        table_switch_keywords = ['different table', 'switch to', 'show me', 'data from']
        
        user_lower = user_message_text.lower()
        
        # Handle requests for more records from the same table
        if any(keyword in user_lower for keyword in more_data_keywords):
            recent_table = extract_recent_table_name(conversation_history)
            if recent_table:
                # Extract number of records requested
                import re
                number_match = re.search(r'(\d+)', user_message_text)
                limit = int(number_match.group(1)) if number_match else 30
                
                sql_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{recent_table}` LIMIT {limit};"
                result = internal_execute_sql_query(sql_query)
                
                if result.get('status') == 'success':
                    app.logger.info(f"Retrieved {limit} records from {recent_table}")
                    displays = []
                    if result.get('data'):
                        app.logger.info(f"Adding table display with {len(result['data'])} rows")
                        displays.append({
                            'type': 'table',
                            'title': f"{recent_table} - {limit} Records",
                            'content': result['data']
                        })
                    else:
                        app.logger.warning(f"Query successful but no data returned for {recent_table}")
                    
                    reply_text = f"Here are {limit} records from the `{recent_table}` table:"
                    
                    return jsonify({
                        'reply': reply_text,
                        'displays': displays
                    })
                else:
                    app.logger.error(f"Query failed for {recent_table}: {result.get('error_message', 'Unknown error')}")
        
        # Handle requests to show previous query results
        elif any(keyword in user_lower for keyword in show_results_keywords):
            recent_table = extract_recent_table_name(conversation_history)
            if recent_table:
                app.logger.info(f"Re-executing query for recent table: {recent_table}")
                
                sql_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{recent_table}` LIMIT 10;"
                result = internal_execute_sql_query(sql_query)
                
                if result.get('status') == 'success':
                    displays = []
                    if result.get('data'):
                        app.logger.info(f"Re-displaying {len(result['data'])} rows from {recent_table}")
                        displays.append({
                            'type': 'table',
                            'title': f"Query Results - {recent_table}",
                            'content': result['data']
                        })
                    else:
                        app.logger.warning(f"Query successful but no data returned for {recent_table}")
                    
                    reply_text = f"Here are the query results from `{recent_table}` table:"
                    
                    return jsonify({
                        'reply': reply_text,
                        'displays': displays
                    })
                else:
                    app.logger.error(f"Failed to re-execute query for {recent_table}: {result.get('error_message', 'Unknown error')}")
            else:
                app.logger.info("User asked for query results but no recent table found in history")

        # Log each message in conversation history for debugging
        for i, msg in enumerate(conversation_history):
            app.logger.info(
                f"History message {i}: role='{msg.get('role')}', content='{msg.get('content')[:100]}...'"
            )



        # Build full history including system instruction and conversation context
        full_history = []
        if GEMINI_SDK_AVAILABLE:
            # Add system instruction
            full_history = [
                google_genai_types.Content(
                    role="user",
                    parts=[
                        google_genai_types.Part(text=SYSTEM_INSTRUCTION_PROMPT)
                    ]),
                google_genai_types.Content(
                    role="model",
                    parts=[
                        google_genai_types.Part(
                            text=
                            "Understood. I am ready to assist you with your Keboola project data. How can I help you?"
                        )
                    ])
            ]

            # Add conversation history (excluding the current message which will be sent separately)
            for msg in conversation_history:
                if msg['role'] in ['user', 'assistant']:
                    role = 'model' if msg['role'] == 'assistant' else 'user'
                    full_history.append(
                        google_genai_types.Content(
                            role=role,
                            parts=[
                                google_genai_types.Part(text=msg['content'])
                            ]))
        else:
            app.logger.error(
                "Gemini SDK types not available to create history.")

        chat_session = gemini_sdk_client.chats.create(
            model='gemini-2.0-flash',
            config=gemini_generation_config_with_tools,
            history=full_history)
        app.logger.info(
            f"Created Gemini chat session with full history ({len(full_history)} messages). Sending user message: '{user_message_text}'"
        )

        response = chat_session.send_message(user_message_text)

        final_answer = ""
        try:
            final_answer = response.text
            app.logger.info(
                f"Gemini final answer (genai.Client/chat): {final_answer}")
        except ValueError as ve:
            app.logger.error(
                f"Gemini response did not directly yield text: {ve}. Parts: {response.parts if hasattr(response, 'parts') else 'N/A'}",
                exc_info=True)
            if response.parts:
                part_summary = []
                for part in response.parts:
                    if hasattr(part, 'function_call') and part.function_call:
                        part_summary.append(
                            f"Model expects function call: {part.function_call.name} with args {part.function_call.args}"
                        )
                    elif hasattr(part, 'text') and part.text:
                        part_summary.append(part.text)
                if part_summary:
                    final_answer = "The model's response is awaiting a tool execution or contains non-textual parts: " + "; ".join(
                        part_summary)
                    app.logger.warning(
                        f"Constructed final_answer from parts due to ValueError on .text: {final_answer}"
                    )
                else:
                    final_answer = f"LLM response processing error: The response did not contain direct text and parts were inconclusive. Details: {str(ve)}"
                    app.logger.error(final_answer)
            else:
                app.logger.error(
                    f"LLM response error: No text and no parts in response. Details: {ve}"
                )
                return jsonify({
                    "error":
                    f"LLM response error: {ve}. The response was empty or in an unexpected format."
                }), 500
        except Exception as e_gen:
            app.logger.error(f"Generic error accessing response.text: {e_gen}",
                             exc_info=True)
            return jsonify(
                {"error": f"Error processing LLM response: {str(e_gen)}"}), 500

        displays = []
        query_data = None
        tool_display_title = "Tool Execution Result"

        # Skip complex primary extraction - go straight to chat history
        app.logger.info("Skipping primary extraction - using direct chat history approach")
            
        # WORKING FIX: Direct chat history extraction using same logic as table lists
        app.logger.info("Using direct chat history extraction")
        try:
            history = chat_session.get_history()
            app.logger.info(f"Got chat history with {len(history)} messages")
            
            # Look through all messages in reverse order for function responses
            for msg in reversed(history):
                if hasattr(msg, 'parts') and msg.parts:
                    for part in msg.parts:
                        if hasattr(part, 'function_response') and part.function_response:
                            tool_name = part.function_response.name
                            tool_result = part.function_response.response
                            
                            app.logger.info(f"Found function response: {tool_name}")
                            
                            if tool_name == 'internal_execute_sql_query':
                                app.logger.info(f"Processing SQL tool result: {type(tool_result)} - {str(tool_result)[:200]}")
                                
                                # Handle both dict and direct data responses
                                if isinstance(tool_result, dict):
                                    # Enhanced extraction logic to handle all response structures
                                    extracted_data = None
                                    
                                    # Method 1: Check for nested result structure {'result': {'status': 'success', 'data': [...]}}
                                    if 'result' in tool_result and isinstance(tool_result['result'], dict):
                                        nested_result = tool_result['result']
                                        status = nested_result.get('status')
                                        data = nested_result.get('data')
                                        app.logger.info(f"SQL tool (nested) status: {status}, data type: {type(data)}, length: {len(data) if isinstance(data, list) else 'N/A'}")
                                        
                                        if status == 'success' and data and isinstance(data, list) and len(data) > 0:
                                            extracted_data = data
                                            app.logger.info(f"NESTED EXTRACTION SUCCESS: {len(extracted_data)} rows")
                                    
                                    # Method 2: Direct structure {'status': 'success', 'data': [...]}
                                    if not extracted_data:
                                        status = tool_result.get('status')
                                        data = tool_result.get('data')
                                        app.logger.info(f"SQL tool (direct) status: {status}, data type: {type(data)}, length: {len(data) if isinstance(data, list) else 'N/A'}")
                                        
                                        if status == 'success' and data and isinstance(data, list) and len(data) > 0:
                                            extracted_data = data
                                            app.logger.info(f"DIRECT EXTRACTION SUCCESS: {len(extracted_data)} rows")
                                    
                                    # Method 3: List response directly
                                    if not extracted_data and isinstance(tool_result, list) and len(tool_result) > 0:
                                        extracted_data = tool_result
                                        app.logger.info(f"LIST EXTRACTION SUCCESS: {len(extracted_data)} rows")
                                    
                                    # Method 4: Check for any 'data' key in the structure
                                    if not extracted_data and isinstance(tool_result, dict):
                                        for key, value in tool_result.items():
                                            if 'data' in str(key).lower() and isinstance(value, list) and len(value) > 0:
                                                extracted_data = value
                                                app.logger.info(f"KEY-BASED EXTRACTION SUCCESS: {len(extracted_data)} rows from key '{key}'")
                                                break
                                    
                                    if extracted_data:
                                        query_data = extracted_data
                                        tool_display_title = "SQL Query Results"
                                        break
                                elif isinstance(tool_result, list) and len(tool_result) > 0:
                                    # Direct data response
                                    query_data = tool_result
                                    tool_display_title = "SQL Query Results"
                                    app.logger.info(f"DIRECT EXTRACTION SUCCESS: {len(query_data)} rows")
                                    break
                                else:
                                    app.logger.warning(f"Unexpected tool result format: {type(tool_result)} - content: {str(tool_result)}")
                                    # Try to access the data through various methods
                                    if hasattr(tool_result, 'data'):
                                        data = getattr(tool_result, 'data')
                                        if isinstance(data, list) and len(data) > 0:
                                            query_data = data
                                            tool_display_title = "SQL Query Results"
                                            app.logger.info(f"ATTRIBUTE EXTRACTION SUCCESS: {len(query_data)} rows")
                                            break
                                    
                                    # Try converting to dict if it's a string
                                    if isinstance(tool_result, str):
                                        try:
                                            import json
                                            parsed_result = json.loads(tool_result)
                                            if isinstance(parsed_result, dict) and parsed_result.get('data'):
                                                query_data = parsed_result['data']
                                                tool_display_title = "SQL Query Results"
                                                app.logger.info(f"JSON EXTRACTION SUCCESS: {len(query_data)} rows")
                                                break
                                        except json.JSONDecodeError:
                                            pass
                if query_data:
                    break
        except Exception as e:
            app.logger.error(f"Chat history extraction failed: {e}")
                
        # Emergency fallback - if AI says it retrieved data but we have nothing
        if not query_data and final_answer and "retrieved" in final_answer.lower():
            app.logger.info("AI claims data retrieval but extraction failed - using emergency fallback")
            
            # First try using globally stored SQL results
            global last_sql_results
            if last_sql_results and isinstance(last_sql_results, list) and len(last_sql_results) > 0:
                query_data = last_sql_results
                tool_display_title = "SQL Query Results (Global Fallback)"
                app.logger.info(f"GLOBAL FALLBACK SUCCESS: {len(query_data)} rows")
                last_sql_results = None  # Clear after use
            else:
                # Pattern-based emergency queries for specific table requests
                if "Balay-Kreative" in final_answer and "attendees" in final_answer:
                    emergency_query = "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders` LIMIT 10"
                    app.logger.info("Triggering emergency fallback for Balay-Kreative attendees")
                    try:
                        emergency_result = internal_execute_sql_query(emergency_query)
                        if emergency_result.get('status') == 'success' and emergency_result.get('data'):
                            query_data = emergency_result['data']
                            tool_display_title = "Balay Kreative Attendees Data"
                            app.logger.info(f"EMERGENCY FALLBACK SUCCESS: {len(query_data)} rows")
                    except Exception as e:
                        app.logger.error(f"Emergency fallback failed: {e}")
                        
            # Map AI responses to specific table queries using correct table names
            if "Balay-Kreative" in final_answer and "Totals" in final_answer:
                emergency_query = "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.-Balay-Kreative--Close-Out-Sales---Halo-Halo-Holidays---2023-12-09---Kapwa-Gardens-Totals` LIMIT 10"
                app.logger.info("Triggering emergency fallback for Balay-Kreative Totals")
            elif "Lovers-Mart" in final_answer:
                emergency_query = "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.2023-02-11-Lovers-Mart-_-Close-Out-Sales-KG-Costs` LIMIT 10"
                app.logger.info("Triggering emergency fallback for Lovers Mart")
            elif "Undiscovered" in final_answer and "Vendor" in final_answer:
                emergency_query = "SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.Undiscovered-Vendor-Export---Squarespace---All-data-orders` LIMIT 10"
                app.logger.info("Triggering emergency fallback for Undiscovered Vendor")
            else:
                emergency_query = None
                
            if emergency_query:
                try:
                    emergency_result = internal_execute_sql_query(emergency_query)
                    if emergency_result.get('status') == 'success' and emergency_result.get('data'):
                        query_data = emergency_result['data']
                        tool_display_title = "Query Results (Emergency Fallback)"
                        app.logger.info(f"EMERGENCY FALLBACK SUCCESS: {len(query_data)} rows")
                except Exception as e:
                    app.logger.error(f"Emergency fallback failed: {e}")
                
        # No emergency reconstruction needed - handled above

        # Final extraction status
        app.logger.info(f"Final extraction result: query_data type={type(query_data)}, length={len(query_data) if isinstance(query_data, list) else 'N/A'}")

        # FINAL STEP: Create displays from extracted data
        displays = []
        
        # Create display if we successfully extracted query data
        if query_data and isinstance(query_data, list) and len(query_data) > 0:
            displays.append({
                "type": "table", 
                "title": tool_display_title or "Query Results",
                "content": query_data
            })
            app.logger.info(f"DISPLAY CREATED: {len(query_data)} rows in '{tool_display_title}'")
        else:
            app.logger.warning(f"NO DISPLAY CREATED: query_data={type(query_data)}, length={len(query_data) if isinstance(query_data, list) else 'N/A'}")
                
        # Handle explicit table requests if no data was extracted
        if not query_data:
            explicit_table_keywords = [
                "show me my tables", "list tables", "what tables do i have", 
                "show me my data tables", "display tables", "available tables", "check my data tables"
            ]
            user_wants_tables = any(phrase in user_message_text.lower() for phrase in explicit_table_keywords)
            
            if user_wants_tables:
                try:
                    tool_result = internal_execute_sql_query("SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name")
                    if tool_result.get('status') == 'success' and tool_result.get('data'):
                        query_data = tool_result['data']
                        tool_display_title = "Available Data Tables"
                        app.logger.info(f"Created table list display with {len(query_data)} tables")
                except Exception as e:
                    app.logger.error(f"Error creating table list: {e}")
        
        # Ensure proper response text
        if not final_answer or final_answer.strip() == "":
            final_answer = "I'm ready to help you analyze your data. What would you like to explore?"
        
        # CRITICAL FIX: Create displays from extracted data
        if query_data and isinstance(query_data, list) and query_data:
            displays.append({
                "type": "table",
                "title": tool_display_title or "Query Results",
                "content": query_data
            })
            app.logger.info(f"DISPLAY CREATED: {len(query_data)} rows in '{tool_display_title}'")
        else:
            app.logger.warning(
                "No structured query_data extracted from tool calls for display. Checking text response for fallback table info."
            )
            if final_answer and isinstance(final_answer, str) and any(
                    phrase in final_answer.lower() for phrase in [
                        "tables in your", "bigquery dataset", "list of tables",
                        "here are the tables", "retrieved all the tables",
                        "table below", "retrieved the data",
                        "retrieved the schema", "sample row from"
                    ]):
                app.logger.info(
                    "AI text suggests data/tables were retrieved; attempting fallback display generation."
                )
                try:
                    fallback_query = None
                    fallback_title = "Query Results"
                    
                    # Initialize table_matches to empty list to prevent NameError
                    table_matches = []
                    
                    # Extract table names from final_answer if it contains table references
                    if final_answer and ("table" in final_answer.lower() or "data" in final_answer.lower()):
                        # Define the pattern for table names (looking for BigQuery table references)
                        table_pattern = r"`?([\w-]+)`?\.`?([\w-]+)`?\.`?([\w-]+)`?"
                        
                        # Find all matches of the pattern - re is imported at module level
                        try:
                            matches = re.findall(table_pattern, final_answer)
                        except NameError:
                            # Fallback if re is not accessible in scope
                            import re as re_module
                            matches = re_module.findall(table_pattern, final_answer)
                        
                        # Extract the table names (third part of each match)
                        table_matches = [match[2] for match in matches if len(match) >= 3]
                    
                    # Only force create display for explicit table requests
                    explicit_table_requests = [
                        "show me my tables", "list tables", "what tables do i have", 
                        "show me my data tables", "display tables", "available tables",
                        "tables in your", "bigquery dataset", "list of tables"
                    ]
                    should_show_tables = any(phrase in final_answer.lower() for phrase in explicit_table_requests) if final_answer else False
                    
                    if not displays and should_show_tables:
                        # Only create table display for explicit requests
                        table_list_query = "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
                        try:
                            bigquery_result = internal_execute_sql_query(table_list_query)
                            if bigquery_result and bigquery_result.get('status') == 'success':
                                table_data = bigquery_result.get('data', [])
                                displays.append({
                                    "type": "table",
                                    "title": "Available Data Tables",
                                    "content": table_data
                                })
                                app.logger.info(f"Force-created display with {len(table_data)} rows")
                        except Exception as e:
                            app.logger.error(f"Error creating fallback display: {e}")

                    if table_matches:
                        table_name = table_matches[0]
                        fallback_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{table_name}` LIMIT 10"
                        fallback_title = f"Data from {table_name} (Fallback)"
                        app.logger.info(
                            f"Fallback: Found table name '{table_name}' in AI text, will query."
                        )
                    elif any(
                            phrase in final_answer.lower() for phrase in
                        ["list of tables", "all tables", "tables available"]):
                        fallback_query = "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
                        fallback_title = "Available Tables (Fallback)"
                        app.logger.info(
                            "Fallback: AI text suggests a table list, will query INFORMATION_SCHEMA."
                        )

                    if fallback_query:
                        fallback_result = internal_execute_sql_query(
                            fallback_query)
                        if fallback_result.get(
                                'status') == 'success' and fallback_result.get(
                                    'data'):
                            displays.append({
                                "type": "table",
                                "title": fallback_title,
                                "content": fallback_result['data']
                            })
                            app.logger.info(
                                f"Fallback query successful, created display '{fallback_title}' with {len(fallback_result['data'])} rows"
                            )
                        else:
                            app.logger.warning(
                                f"Fallback query failed or returned no data: {fallback_result.get('error_message', 'No data')}"
                            )
                except Exception as e_fallback:
                    app.logger.error(
                        f"Error during fallback display generation: {e_fallback}",
                        exc_info=True)

            if not displays and final_answer and isinstance(
                    final_answer, str) and any(
                        phrase in final_answer.lower() for phrase in [
                            "tables in your", "bigquery dataset",
                            "list of tables", "here are the tables"
                        ]):
                lines = final_answer.split('\n')
                table_names_from_text = []
                in_table_list_context = False
                if "here are the tables" in final_answer.lower(
                ) or "following tables" in final_answer.lower():
                    in_table_list_context = True

                for line in lines:
                    stripped_line = line.strip()
                    if stripped_line.startswith(('- ', '* ')):
                        table_name_candidate = stripped_line[2:].strip('`"')
                        if table_name_candidate and ('.' in table_name_candidate or \
                           (KBC_WORKSPACE_SCHEMA and KBC_WORKSPACE_SCHEMA in table_name_candidate) or \
                           re.match(r'^(OUT|DIM|FACT|STG)_[A-Z_0-9]+$', table_name_candidate, re.IGNORECASE)) and \
                           not any(char in table_name_candidate for char in ['(', ')', ':', '?']):
                            table_names_from_text.append(table_name_candidate)
                    elif in_table_list_context and stripped_line and not stripped_line.endswith(
                            ':'):
                        if (KBC_WORKSPACE_SCHEMA and KBC_WORKSPACE_SCHEMA in stripped_line) or '`' in stripped_line or \
                           re.match(r'^(OUT|DIM|FACT|STG)_[A-Z_0-9]+$', stripped_line.strip('`"'), re.IGNORECASE):
                            table_names_from_text.append(
                                stripped_line.strip('`"'))

                unique_table_names = sorted(list(set(table_names_from_text)))
                if unique_table_names:
                    app.logger.info(
                        f"Fallback (text parsing): Extracted table names: {unique_table_names}"
                    )
                    if not any(
                            d.get("title") ==
                            "Identified Data Tables (from text)"
                            for d in displays):  # Avoid double adding
                        displays.append({
                            "type":
                            "table",
                            "title":
                            "Identified Data Tables (from text)",
                            "content": [{
                                "Table Name": name
                            } for name in unique_table_names]
                        })
        return jsonify({"reply": final_answer, "displays": displays})

    except Exception as e:
        app.logger.error(
            f"Error in /api/chat endpoint (genai.Client style): {e}",
            exc_info=True)
        return jsonify({"error":
                        f"An unexpected error occurred: {str(e)}"}), 500


# --- API v1 Endpoints for External Integration ---

@app.route('/api/v1/data/query', methods=['POST'])
def api_v1_data_query():
    """Intelligent query router - automatically determines which API method to use"""
    try:
        data = request.get_json()
        if not data:
            return jsonify({"success": False, "error": "No JSON data provided"}), 400
        
        query = data.get('query', '').lower().strip()
        credentials = data.get('credentials', {})
        
        if not query:
            return jsonify({"success": False, "error": "Query parameter is required"}), 400
        
        app.logger.info(f"API v1 Intelligent Query Router: {query}")
        
        # Smart routing logic based on query content
        route_decision = determine_query_route(query)
        app.logger.info(f"Route decision: {route_decision}")
        
        if route_decision['route'] == 'tables':
            # Route to table discovery
            app.logger.info("Routing to table discovery endpoint")
            return handle_table_discovery_request(credentials)
            
        elif route_decision['route'] == 'sql':
            # Route to direct SQL execution
            app.logger.info(f"Routing to SQL execution: {route_decision['sql_query']}")
            return handle_direct_sql_request(route_decision['sql_query'], credentials)
            
        else:
            # Route to natural language processing
            app.logger.info("Routing to natural language AI processing")
            return handle_natural_language_request(query, credentials)
        
    except Exception as e:
        app.logger.error(f"Error in intelligent query router: {e}", exc_info=True)
        return jsonify({"success": False, "error": str(e)}), 500


def determine_query_route(query):
    """Determine which API route to use based on query content"""
    
    # Table discovery patterns
    table_patterns = [
        'show me tables', 'list tables', 'what tables', 'available tables',
        'show tables', 'display tables', 'table list', 'all tables',
        'what data do i have', 'show my data', 'data available'
    ]
    
    # Check for table discovery requests first
    if any(pattern in query for pattern in table_patterns):
        return {'route': 'tables', 'reason': 'Table discovery request detected'}
    
    # Direct SQL patterns - must start with SQL keywords for accurate detection
    sql_start_patterns = [
        'select ', 'SELECT ', 'with ', 'WITH ', 'create ', 'CREATE ',
        'insert ', 'INSERT ', 'update ', 'UPDATE ', 'delete ', 'DELETE '
    ]
    
    # Check for direct SQL queries - must start with SQL keywords
    if any(query.strip().startswith(pattern.strip()) for pattern in sql_start_patterns):
        # Extract and clean the SQL query
        sql_query = query.strip()
        # Remove common prefixes
        prefixes = ['execute ', 'run ', 'query ']
        for prefix in prefixes:
            if sql_query.startswith(prefix):
                sql_query = sql_query[len(prefix):].strip()
        
        return {
            'route': 'sql',
            'sql_query': sql_query,
            'reason': 'Direct SQL query detected'
        }
    
    # Default to natural language processing for business queries
    return {'route': 'nlp', 'reason': 'Natural language query requiring AI processing'}


def handle_table_discovery_request(credentials):
    """Handle table discovery requests"""
    try:
        table_query = f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
        result = internal_execute_sql_query(table_query)
        
        if result.get('status') == 'success':
            tables_data = result.get('data', [])
            
            # Format tables for better display
            formatted_tables = []
            for table in tables_data:
                table_name = table.get('table_name', '')
                business_area = "Unknown"
                data_type = "General"
                
                if "Balay-Kreative" in table_name:
                    business_area = "Balay Kreative"
                    if "attendees" in table_name.lower():
                        data_type = "Event Attendees"
                    elif "sales" in table_name.lower() or "orders" in table_name.lower():
                        data_type = "Sales Data"
                elif "Kapwa-Gardens" in table_name:
                    business_area = "Kapwa Gardens"
                    data_type = "Market Data"
                elif "Undiscovered" in table_name:
                    business_area = "Undiscovered"
                    if "Vendor" in table_name:
                        data_type = "Vendor Data"
                
                formatted_tables.append({
                    "Business Area": business_area,
                    "Data Type": data_type,
                    "Table Name": table_name,
                    "Description": f"{business_area} {data_type.lower()}"
                })
            
            return jsonify({
                "success": True,
                "query": "Table discovery request",
                "response": f"Found {len(formatted_tables)} available data tables across your business areas.",
                "data": [{
                    "type": "table",
                    "title": "Available Data Tables",
                    "content": formatted_tables
                }],
                "route_used": "tables",
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({
                "success": False,
                "error": result.get('error', 'Failed to retrieve tables'),
                "route_used": "tables",
                "timestamp": datetime.now().isoformat()
            }), 400
            
    except Exception as e:
        app.logger.error(f"Error in table discovery: {e}", exc_info=True)
        return jsonify({
            "success": False,
            "error": str(e),
            "route_used": "tables",
            "timestamp": datetime.now().isoformat()
        }), 500


def handle_direct_sql_request(sql_query, credentials):
    """Handle direct SQL execution requests"""
    try:
        result = internal_execute_sql_query(sql_query)
        
        if result.get('status') == 'success':
            query_data = result.get('data', [])
            return jsonify({
                "success": True,
                "query": sql_query,
                "response": f"SQL query executed successfully. Returned {len(query_data)} rows.",
                "data": [{
                    "type": "table",
                    "title": "SQL Query Results",
                    "content": query_data
                }] if query_data else [],
                "route_used": "sql",
                "rows_returned": len(query_data) if isinstance(query_data, list) else 0,
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({
                "success": False,
                "error": result.get('error', 'SQL execution failed'),
                "route_used": "sql",
                "timestamp": datetime.now().isoformat()
            }), 400
            
    except Exception as e:
        app.logger.error(f"Error in SQL execution: {e}", exc_info=True)
        return jsonify({
            "success": False,
            "error": str(e),
            "route_used": "sql",
            "timestamp": datetime.now().isoformat()
        }), 500


def handle_natural_language_request(query, credentials):
    """Handle natural language AI processing requests"""
    try:
        if not GEMINI_SDK_AVAILABLE:
            return jsonify({"success": False, "error": "Gemini SDK not available", "route_used": "nlp"}), 500
        
        # Create chat session using global client
        chat_session = gemini_sdk_client.chats.create(
            model="gemini-2.0-flash-exp",
            config=gemini_generation_config_with_tools,
            history=[
                google_genai_types.Content(
                    parts=[google_genai_types.Part(text="You are an AI assistant for data analysis.")],
                    role="model"
                ),
                google_genai_types.Content(
                    parts=[google_genai_types.Part(text="Hello, I'm ready to help with data analysis.")],
                    role="user"
                )
            ]
        )
        
        # Send the query
        response = chat_session.send_message(query)
        final_answer = response.text if hasattr(response, 'text') else str(response)
        
        # Extract data using existing logic
        query_data = []
        displays = []
        
        # Check if response contains function calls or data
        if hasattr(response, 'candidates') and response.candidates:
            for candidate in response.candidates:
                if hasattr(candidate, 'content') and hasattr(candidate.content, 'parts'):
                    for part in candidate.content.parts:
                        if hasattr(part, 'function_call'):
                            # Process function call results
                            try:
                                if part.function_call.name == 'internal_execute_sql_query':
                                    sql_query = part.function_call.args.get('query', '')
                                    result = internal_execute_sql_query(sql_query)
                                    if result.get('status') == 'success' and result.get('data'):
                                        query_data = result['data']
                            except Exception as e:
                                app.logger.error(f"Function call processing failed: {e}")
        
        # Create display format for API response
        if query_data and isinstance(query_data, list) and len(query_data) > 0:
            displays.append({
                "type": "table",
                "title": "Query Results",
                "content": query_data
            })
        
        return jsonify({
            "success": True,
            "query": query,
            "response": final_answer,
            "data": displays,
            "route_used": "nlp",
            "timestamp": datetime.now().isoformat()
        })
        
    except Exception as e:
        app.logger.error(f"Error in natural language processing: {e}", exc_info=True)
        return jsonify({
            "success": False, 
            "error": str(e), 
            "route_used": "nlp",
            "timestamp": datetime.now().isoformat()
        }), 500


@app.route('/api/v1/data/sql', methods=['POST'])
def api_v1_data_sql():
    """Direct SQL execution endpoint for external integration"""
    try:
        data = request.get_json()
        if not data:
            return jsonify({"success": False, "error": "No JSON data provided"}), 400
        
        sql_query = data.get('sql', '')
        if not sql_query:
            return jsonify({"success": False, "error": "SQL parameter is required"}), 400
        
        app.logger.info(f"API v1 Direct SQL Query: {sql_query}")
        
        # Execute SQL directly
        result = internal_execute_sql_query(sql_query)
        
        if result.get('status') == 'success':
            query_data = result.get('data', [])
            return jsonify({
                "success": True,
                "data": query_data,
                "error": None,
                "rows_returned": len(query_data) if isinstance(query_data, list) else 0,
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({
                "success": False,
                "data": [],
                "error": result.get('error', 'Unknown error'),
                "rows_returned": 0,
                "timestamp": datetime.now().isoformat()
            }), 400
            
    except Exception as e:
        app.logger.error(f"Error in /api/v1/data/sql: {e}", exc_info=True)
        return jsonify({
            "success": False,
            "data": [],
            "error": str(e),
            "rows_returned": 0,
            "timestamp": datetime.now().isoformat()
        }), 500


@app.route('/api/v1/data/tables', methods=['POST'])
def api_v1_data_tables():
    """Table discovery endpoint for external integration"""
    try:
        app.logger.info("API v1 Table Discovery Request")
        
        # Get all available tables
        table_query = f"SELECT table_name FROM `{GOOGLE_PROJECT_ID}.{KBC_WORKSPACE_ID}.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
        result = internal_execute_sql_query(table_query)
        
        if result.get('status') == 'success':
            tables_data = result.get('data', [])
            
            # Format tables for better display
            formatted_tables = []
            for table in tables_data:
                table_name = table.get('table_name', '')
                # Parse business area from table name
                business_area = "Unknown"
                data_type = "General"
                
                if "Balay-Kreative" in table_name:
                    business_area = "Balay Kreative"
                    if "attendees" in table_name.lower():
                        data_type = "Event Attendees"
                    elif "sales" in table_name.lower() or "orders" in table_name.lower():
                        data_type = "Sales Data"
                elif "Kapwa-Gardens" in table_name:
                    business_area = "Kapwa Gardens"
                    data_type = "Market Data"
                elif "Undiscovered" in table_name:
                    business_area = "Undiscovered"
                    if "Vendor" in table_name:
                        data_type = "Vendor Data"
                
                formatted_tables.append({
                    "Business Area": business_area,
                    "Data Type": data_type,
                    "Table Name": table_name,
                    "Description": f"{business_area} {data_type.lower()}"
                })
            
            return jsonify({
                "success": True,
                "tables": [{
                    "type": "table",
                    "title": "Available Data Tables",
                    "content": formatted_tables
                }],
                "total_tables": len(formatted_tables),
                "timestamp": datetime.now().isoformat()
            })
        else:
            return jsonify({
                "success": False,
                "tables": [],
                "error": result.get('error', 'Failed to retrieve tables'),
                "total_tables": 0,
                "timestamp": datetime.now().isoformat()
            }), 400
            
    except Exception as e:
        app.logger.error(f"Error in /api/v1/data/tables: {e}", exc_info=True)
        return jsonify({
            "success": False,
            "tables": [],
            "error": str(e),
            "total_tables": 0,
            "timestamp": datetime.now().isoformat()
        }), 500


# --- Main Execution ---
if __name__ == '__main__':
    app.logger.info(
        f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    app.logger.info(
        f"KBC_STORAGE_TOKEN from env: {'SET' if KBC_STORAGE_TOKEN else 'NOT SET'}"
    )
    app.logger.info(
        f"GOOGLE_APPLICATION_CREDENTIALS_PATH from env: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'NOT SET'}"
    )
    app.logger.info(
        f"KBC_WORKSPACE_SCHEMA from env: {'SET' if KBC_WORKSPACE_SCHEMA else 'NOT SET'}"
    )
    app.logger.info(
        f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    if not GEMINI_SDK_AVAILABLE:
        app.logger.critical(
            "CRITICAL ERROR: google.genai SDK style could not be imported. Chat functionality will not work."
        )
    elif not all([
            KBC_API_URL, KBC_STORAGE_TOKEN,
            GOOGLE_APPLICATION_CREDENTIALS_PATH, KBC_WORKSPACE_SCHEMA,
            GEMINI_API_KEY
    ]):
        app.logger.critical(
            "CRITICAL ERROR: One or more essential environment variables are missing. Server cannot function fully."
        )
    if GEMINI_SDK_AVAILABLE and isinstance(google_genai_types.Content(),
                                           type(None.__class__)):
        app.logger.warning(
            "GEMINI_SDK_AVAILABLE is True, but google_genai_types seem to be dummy classes. Imports might not have fully succeeded as expected."
        )

    # Force remove any PORT environment variable that might interfere
    if 'PORT' in os.environ:
        del os.environ['PORT']
    
    # Always use port 8081 for Flask backend (ignore Node.js PORT env var)
    port = 8081
    app.logger.info(f"Starting Flask server on host='0.0.0.0', port={port}")
    app.run(host='0.0.0.0',
            port=port,
            debug=False,
            use_reloader=False,
            threaded=True)
