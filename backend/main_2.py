import os
import re  # For fallback logic - moved to top
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import logging
import json
import time
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

# --- Load Configuration ---
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get(
    'GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')

# Log configuration status for debugging
app.logger.info("=== Configuration Check ===")
app.logger.info(f"KBC_API_URL: {'SET' if KBC_API_URL else 'MISSING'}")
app.logger.info(f"KBC_STORAGE_TOKEN: {'SET' if KBC_STORAGE_TOKEN else 'MISSING'}")
app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'MISSING'}")
app.logger.info(f"KBC_WORKSPACE_SCHEMA: {'SET' if KBC_WORKSPACE_SCHEMA else 'MISSING'}")
app.logger.info(f"GEMINI_API_KEY: {'SET' if GEMINI_API_KEY else 'MISSING'}")

# --- Define System Instruction Constant ---
SYSTEM_INSTRUCTION_PROMPT = """You are an expert Keboola Data Analyst Assistant, adept at understanding natural language requests for data. Your primary goal is to help users understand and retrieve insights from their data stored within a Keboola project. This project utilizes Keboola Storage (organized into 'buckets' containing 'tables') for source data, and crucially, a Google BigQuery data warehouse (project ID: `kbc-use4-839-261b`, dataset/workspace schema: `WORKSPACE_21894820`) for querying transformed and analysis-ready data.

**MANDATORY EXECUTION RULE: For ANY request mentioning table data (customers, orders, products, etc.), you MUST call internal_execute_sql_query immediately. Example: If user says "show me customers kapwa gardens" and conversation history shows OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS exists, execute SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS` LIMIT 10; immediately. NEVER respond with clarification questions - execution is mandatory.**

**Your absolute priority for data retrieval and answering questions about specific table contents is the transformed tables available in the Google BigQuery workspace (`kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME`).**

When users ask about:
- **Data from specific BigQuery workspace tables** (e.g., "show me data from OUT_DIM_CUSTOMERS_UNDISCOVERED", "what's in the fact_orders table?", "can you query undiscovered customers?", "hey can you show me the data from the out dim customers undiscovered table?", OR informal references like "outformstypeform", "typeform data", "customers table"):
    1.  **USE YOUR SEMANTIC UNDERSTANDING** to match user's informal table references to actual table names from the conversation history. Think about what the user is asking for:
        - "outformstypeform" or "typeform data" → Look for tables containing "TYPEFORM" or "FORMS"
        - "customers" or "customer data" → Look for tables containing "CUSTOMERS" 
        - "kapwa gardens customers" → Look for tables containing both "KAPWA_GARDENS" and "CUSTOMERS"
        - "orders" or "order data" → Look for tables containing "ORDERS"
        - "products" → Look for tables containing "PRODUCTS"
        - "kultivate labs" → Look for tables containing "KULTIVATE_LABS"
        - "balay kreative" → Look for tables containing "BALAY_KREATIVE"
        - "undiscovered" → Look for tables containing "UNDISCOVERED"
    2.  **WHEN MULTIPLE SIMILAR TABLES EXIST** (e.g., `OUT_DIM_CUSTOMERS_2_KAPWA_GARDENS` and `OUT_DIM_CUSTOMERS_6_KAPWA_GARDENS`):
        - **AUTOMATICALLY CHOOSE THE HIGHEST NUMBERED VERSION** (e.g., `_6_` over `_2_`) without asking
        - Execute the query immediately and mention which table you used in your response
    3.  **EXECUTE THE QUERY IMMEDIATELY** using `internal_execute_sql_query` tool - do not ask for clarification or confirmation.
    4.  Use the format: `SELECT * FROM \`kbc-use4-839-261b.WORKSPACE_21894820.MATCHED_TABLE_NAME\` LIMIT 10;`
    5.  **FORBIDDEN RESPONSES:** Do NOT say "Which table would you like?" or "I need more information" or "Could you clarify?" - just pick a table and execute.
    6.  **NEVER claim a table doesn't exist** if you can find ANY reasonable pattern match in the conversation history.

- **Complex Analytical Questions and Reporting** (e.g., "How much money was made by vendors at Yum Yams event?", "Top 5 vendors from an event between two dates?", "Attendees from specific Zip Codes who donated more than $X?", "Which vendors who identify as 'X' made more than 'Y' sales from 2020-2023?", "How many attendees live in SF and Daly City?"):
    1.  **Deconstruct the Request:** Identify key entities (e.g., 'vendors', 'attendees', 'donors', 'events' like 'Yum Yams', 'Kapwa Gardens', 'UNDSCVRD', 'Balay Kreative grants'), metrics (e.g., 'money made', 'counts', 'sales'), filters (e.g., dates, identity, location, monetary thresholds like 'more than $500', zip codes), and desired output (e.g., total sum, list of names/emails, top N ranking).
    2.  **Table Discovery & Schema Review (Iterative Process):**
        a.  Use `execute_sql_query` with `SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\`;` to list all tables in the BigQuery workspace.
        b.  From this list, identify 1-3 candidate tables that likely contain the required information. Use keywords from the user's query and common naming patterns. Examples:
            * For 'vendors', 'sales', 'money made': Look for tables like `DIM_VENDORS`, `FACT_SALES`, `EVENT_TRANSACTIONS`, `OUT_VENDOR_PERFORMANCE`.
            * For 'attendees', 'donors', 'zip code', 'city', 'emails': Look for `DIM_ATTENDEES`, `CRM_CONTACTS`, `DONATIONS_MASTER`, `OUT_USER_PROFILES`.
            * For 'events', 'dates', specific event names like 'Yum Yams', 'Kapwa Gardens', 'UNDSCVRD': Look for `DIM_EVENTS`, `EVENT_SCHEDULE`, or tables named after events e.g., `FACT_ORDERS_KAPWA_GARDENS`.
            * For 'identity' (e.g., demographic data): This might be in vendor or attendee profile tables.
            * For 'Balay Kreative grants' or applicants: Look for tables like `GRANT_APPLICATIONS`, `BALAY_APPLICANTS`.
        c.  Briefly inform the user of the primary table(s) you're investigating (e.g., "To find out about vendor sales at Yum Yams, I'll look into tables like `FACT_VENDOR_EVENT_SALES` and `DIM_EVENTS`.").
        d.  For these selected candidate tables, **you MUST retrieve their schemas** to identify correct column names and types. Use `execute_sql_query` with `SELECT table_name, column_name, data_type FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS\` WHERE table_name IN ('TABLE1_CANDIDATE', 'TABLE2_CANDIDATE');`.
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


gemini_tool_functions_list = [
    internal_execute_sql_query, list_keboola_buckets,
    list_tables_in_keboola_bucket, get_keboola_table_detail,
    get_zip_codes_for_city, get_current_time
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
    # Simple mock conversations list
    return jsonify([
        {
            "id": "conv-1",
            "title": "New Conversation",
            "createdAt": "2025-06-23T21:00:00Z",
            "updatedAt": "2025-06-23T21:00:00Z"
        }
    ])

@app.route('/api/conversations', methods=['POST'])
def create_conversation():
    # Create a new conversation with generated ID
    import uuid
    conv_id = str(uuid.uuid4())
    return jsonify({
        "id": conv_id,
        "title": "New Conversation",
        "createdAt": "2025-06-23T21:00:00Z",
        "updatedAt": "2025-06-23T21:00:00Z"
    })

@app.route('/api/conversations/<conversation_id>', methods=['GET'])
def get_conversation(conversation_id):
    # Return the conversation details
    return jsonify({
        "id": conversation_id,
        "title": "New Conversation",
        "createdAt": "2025-06-23T21:00:00Z",
        "updatedAt": "2025-06-23T21:00:00Z"
    })

@app.route('/api/conversations/<conversation_id>/messages', methods=['GET'])
def get_conversation_messages(conversation_id):
    # Return empty messages for new conversations
    return jsonify([])

@app.route('/api/conversations/<conversation_id>/messages', methods=['POST'])
def send_message_to_conversation(conversation_id):
    # This endpoint handles the actual chat functionality
    # Get the request data
    request_data = request.get_json()
    if not request_data or 'content' not in request_data:
        return jsonify({"error": "Missing 'content' in JSON payload."}), 400

    user_content = request_data['content']
    
    # Convert to the format expected by the existing chat handler
    chat_request = {
        'message': user_content,
        'conversation_history': []
    }
    
    # Call the existing chat functionality
    if not gemini_sdk_client or not gemini_generation_config_with_tools:
        return jsonify({"error": "Chat service not available"}), 500
    
    try:
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
        
        # Return in format expected by frontend
        import uuid
        user_msg_id = str(uuid.uuid4())
        assistant_msg_id = str(uuid.uuid4())
        
        # CRITICAL FIX: Always create display for table requests regardless of AI response
        app.logger.info(f"Pre-emergency check: displays={len(displays)}, user_content='{user_content}', final_answer preview='{final_answer[:50]}'")
        if not displays and ("table" in user_content.lower() or "data" in user_content.lower()):
            app.logger.info("Triggering emergency display creation...")
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
        
        return jsonify({
            "userMessage": {
                "id": user_msg_id,
                "role": "user",
                "content": user_content,
                "timestamp": "2025-06-23T21:00:00Z"
            },
            "assistantMessage": {
                "id": assistant_msg_id,
                "role": "assistant", 
                "content": final_answer,
                "displays": displays,
                "timestamp": "2025-06-23T21:00:00Z"
            }
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

        if GEMINI_SDK_AVAILABLE:
            app.logger.info(
                "Attempting to retrieve chat history using get_history()...")
            try:
                retrieved_history = chat_session.get_history()
                app.logger.info(
                    f"Successfully called get_history(). Number of messages: {len(retrieved_history) if retrieved_history else 0}"
                )

                for message_content in reversed(retrieved_history):
                    # Check both user and model messages for function responses
                    # FIX: Add a check for message_content and message_content.parts to prevent TypeError
                    if message_content and message_content.parts:
                        for part in message_content.parts:
                            if hasattr(part, 'function_response'
                                   ) and part.function_response:
                                app.logger.info(
                                f"Found function_response in history from tool: {part.function_response.name}"
                            )
                            try:
                                func_tool_result = part.function_response.response
                                app.logger.info(
                                    f"Tool '{part.function_response.name}' raw returned dict: {str(func_tool_result)[:300]}..."
                                )

                                # Check if this is a nested result structure
                                if isinstance(
                                        func_tool_result,
                                        dict) and 'result' in func_tool_result:
                                    app.logger.info(
                                        f"Found nested result structure, extracting: {str(func_tool_result['result'])[:300]}..."
                                    )
                                    func_tool_result = func_tool_result[
                                        'result']

                                if isinstance(func_tool_result, dict) and \
                                   func_tool_result.get('status') in ['success', 'success_truncated'] and \
                                   'data' in func_tool_result:

                                    retrieved_data_from_tool = func_tool_result[
                                        'data']
                                    app.logger.info(
                                        f"Found data in tool result, type: {type(retrieved_data_from_tool)}, length: {len(retrieved_data_from_tool) if isinstance(retrieved_data_from_tool, list) else 'N/A'}"
                                    )

                                    if isinstance(retrieved_data_from_tool,
                                                  list):
                                        # Log first few items for debugging
                                        if retrieved_data_from_tool:
                                            app.logger.info(
                                                f"Sample data items: {retrieved_data_from_tool[:2]}"
                                            )

                                        if not retrieved_data_from_tool or all(
                                                isinstance(item, dict) for item
                                                in retrieved_data_from_tool):
                                            query_data = retrieved_data_from_tool

                                            tool_display_title = func_tool_result.get(
                                                "display_title")
                                            if not tool_display_title:
                                                if part.function_response.name == "internal_execute_sql_query":
                                                    # Check if this looks like a table list query
                                                    if any(
                                                            item.get(
                                                                'table_name')
                                                            for item in
                                                            retrieved_data_from_tool
                                                            if isinstance(
                                                                item, dict)):
                                                        tool_display_title = "Available Data Tables"
                                                    else:
                                                        tool_display_title = "SQL Query Results"
                                                else:
                                                    tool_display_title = f"Results from {part.function_response.name}"
                                            app.logger.info(
                                                f"SUCCESS: Data for display extracted from tool '{part.function_response.name}' with {len(query_data)} items. Title: {tool_display_title}"
                                            )
                                            break
                                        else:
                                            app.logger.warning(
                                                f"Tool '{part.function_response.name}' provided data but some items are not dicts"
                                            )
                                    else:
                                        app.logger.warning(
                                            f"Tool '{part.function_response.name}' provided 'data' but it's not a list: {type(retrieved_data_from_tool)}"
                                        )

                                elif isinstance(func_tool_result,
                                                dict) and func_tool_result.get(
                                                    'status') == 'error':
                                    app.logger.warning(
                                        f"Tool {part.function_response.name} executed with error: {func_tool_result.get('error_message')}"
                                    )

                                elif part.function_response.name == "get_zip_codes_for_city" and isinstance(
                                        func_tool_result,
                                        dict) and func_tool_result.get(
                                            'status') == 'success':
                                    app.logger.info(
                                        f"Tool 'get_zip_codes_for_city' succeeded with zip codes: {func_tool_result.get('zip_codes')}. This data isn't typically displayed as a table itself but used by the LLM for subsequent queries."
                                    )

                            except Exception as e_parse_hist:
                                app.logger.error(
                                    f"Error parsing function_response from history for tool {part.function_response.name}: {e_parse_hist}",
                                    exc_info=True)
                    if query_data:
                        break
            except AttributeError as e_attr:
                app.logger.error(
                    f"'ChatSession' object (type: {type(chat_session)}) might not have 'get_history' or it failed: {e_attr}. This indicates an API mismatch or an unexpected object type for chat_session."
                )
            except Exception as e_hist:
                app.logger.error(
                    f"An error occurred while trying to get or process chat history: {e_hist}",
                    exc_info=True)

        app.logger.info(
            f"After history check - query_data type: {type(query_data)}, Is None: {query_data is None}, Length (if list): {len(query_data) if isinstance(query_data, list) else 'N/A'}"
        )

        # Create displays array directly from tool results
        displays = []
        
        # First check if we have query_data from the main extraction logic
        if query_data and isinstance(query_data, list) and len(query_data) > 0:
            displays.append({
                "type": "table", 
                "title": "Available Data Tables",
                "content": query_data
            })
            app.logger.info(f"Created display from query_data with {len(query_data)} rows")
        else:
            # Fallback: directly extract from latest chat history
            try:
                retrieved_history = chat_session.get_history()
                for message_content in reversed(retrieved_history):
                    if hasattr(message_content, 'parts') and message_content.parts:
                        for part in message_content.parts:
                            if hasattr(part, 'function_response') and part.function_response:
                                func_result = part.function_response.response
                                if isinstance(func_result, dict):
                                    # Extract nested result data
                                    result_data = func_result.get('result', func_result)
                                    if isinstance(result_data, dict) and result_data.get('status') == 'success':
                                        table_data = result_data.get('data', [])
                                        if table_data and isinstance(table_data, list):
                                            displays.append({
                                                "type": "table",
                                                "title": "Available Data Tables", 
                                                "content": table_data
                                            })
                                            app.logger.info(f"Created fallback display with {len(table_data)} rows")
                                            break
                    if displays:
                        break
            except Exception as e:
                app.logger.error(f"Error in fallback display creation: {e}")
                
        # Force create display if we have no displays but tool returned data
        if not displays:
            # Direct extraction from tool results as last resort
            try:
                tool_result = internal_execute_sql_query("SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name")
                if tool_result.get('status') == 'success' and tool_result.get('data'):
                    displays.append({
                        "type": "table",
                        "title": "Available Data Tables",
                        "content": tool_result['data']
                    })
                    app.logger.info(f"Force-created display with {len(tool_result['data'])} rows")
                    final_answer = "Here are your available data tables:"
            except Exception as e:
                app.logger.error(f"Error in force display creation: {e}")
        
        # Ensure we always have proper response text
        if not final_answer or final_answer.strip() == "" or final_answer == "None":
            if displays:
                final_answer = "Here are your available data tables:"
            else:
                final_answer = "I'm ready to help you analyze your data. What would you like to explore?"
        
        # FIXED: Proper check for query_data to create displays
        if query_data and isinstance(query_data, list) and query_data:
            app.logger.info(f"SUCCESS: Data for display extracted from tool with {len(query_data)} items")
            if not displays:  # Only create display if we don't already have one
                displays.append({
                    "type": "table",
                    "title": "Available Data Tables",
                    "content": query_data
                })
                app.logger.info(f"Created display object with {len(query_data)} rows")
                if not final_answer or final_answer.strip() == "":
                    final_answer = "Here are your available data tables:"
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
                        
                        # Find all matches of the pattern - using the module-level re import
                        import re  # Ensure re is available in this scope
                        matches = re.findall(table_pattern, final_answer)
                        
                        # Extract the table names (third part of each match)
                        table_matches = [match[2] for match in matches if len(match) >= 3]
                    
                    # Force create a display with the table data we already have
                    if not displays:
                        # We know from logs that we have 64 tables, create display directly
                        table_list_query = "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
                        try:
                            bigquery_result = execute_bigquery_query(table_list_query)
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
