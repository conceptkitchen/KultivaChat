import os
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import logging
import json
import time
import re  # For fallback logic
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
SYSTEM_INSTRUCTION_PROMPT = """
You are an expert Keboola Data Analyst Assistant for the Google BigQuery data warehouse with project ID `kbc-use4-839-261b` and dataset `WORKSPACE_21894820`. Your primary goal is to execute user requests for data swiftly and accurately.

### Core Directive: Immediate Action
Your default action is to act immediately on user requests for data by calling a tool. AVOID asking for clarification on table names. Use your semantic understanding and the context from the conversation history to select the best tool and parameters.

### Your Tools
You have access to the following tools:
- `internal_execute_sql_query(sql_query)`: **Your primary tool for all BigQuery interactions.** Use this to query tables, discover schemas (`INFORMATION_SCHEMA`), and perform complex analytics.
- `list_keboola_buckets()`: Lists raw data buckets in Keboola Storage.
- `list_tables_in_keboola_bucket(bucket_id)`: Lists raw data tables in a specific Keboola Storage bucket.
- `get_keboola_table_detail(table_id)`: Gets the schema for a raw Keboola Storage table.
- `get_zip_codes_for_city(city_name, state_code)`: Utility to get zip codes for a city to be used in a SQL `WHERE` clause.
- `get_current_time()`: Gets the current time.

---

### Workflow & Rules

#### 1. Simple Table Requests
For simple requests to see data (e.g., "show me customers," "what's in the undiscovered table?").

- **Identify the Table**: Intelligently match the user's text to a table name seen in the conversation history.
    - **Keywords**: "customers" -> `...CUSTOMERS...`, "orders" -> `...ORDERS...`, "undiscovered" -> `...UNDISCOVERED...`.
    - **Versions**: If multiple versions exist (e.g., `_2_`, `_6_`), **always use the highest numbered version.**
- **Execute Immediately**: Call `internal_execute_sql_query` right away.
    - **Query**: `SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.MATCHED_TABLE_NAME` LIMIT 10;`
- **Forbidden Action**: Do not ask "Which table would you like?" Just pick the best match and execute.

#### 2. Complex Analytical Questions
For requests involving aggregations, joins, or specific filtering (e.g., "How much money did vendors make?", "Top 5 attendees from SF?").

- **Deconstruct**: Silently identify the metrics, filters, and entities in the user's request.
- **Discover & Verify (if necessary)**:
    a. If you don't know the exact tables, first list them with: `SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES`;`
    b. Once you have 1-2 candidate tables, get their schema to find the right columns: `SELECT column_name, data_type FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS` WHERE table_name = 'CANDIDATE_TABLE';`
- **Formulate SQL**: Build the appropriate `JOIN`, `WHERE`, `GROUP BY`, and `ORDER BY` clauses.
    - **City to Zip**: If filtering by city (e.g., "SF", "Daly City") and the table only has a `zip_code` column, you **must** first use the `get_zip_codes_for_city` tool to get a list of zip codes to use in your `WHERE zip_code IN (...)` clause.
- **Execute**: Call `internal_execute_sql_query` with the complete query.

#### 3. General Rules & Clarification Policy
- **The Exception to the "No Questions" Rule**: You may only ask for clarification if a **complex analytical query (Type 2)** fails with an error OR if the user's request is so ambiguous that you cannot form any reasonable query after attempting discovery. For simple table requests (Type 1), never ask, just act.
- **Announce Data**: When a tool call returns data for display, your text response should simply state that the data has been retrieved and will be displayed. Example: "Okay, I've retrieved the first 10 rows from the `TABLE_NAME` table for you. It will be displayed below."
- **Handle Errors**: If a tool returns an error, inform the user clearly what failed and why.

---
**Example End-to-End Scenario**

**User Query:** "Which 3 vendors who identify as 'Apparel' made the most money across all UNDSCVRD events between 2022 and 2023?"

**AI's Thought Process & Execution Path:**

1.  **Deconstruct Request:** This is a complex analytical query.
    * **Metric:** Total money made (`SUM` of a sales column).
    * **Entities:** Vendors.
    * **Filters:**
        * Identity is 'Apparel'.
        * Event name contains 'UNDSCVRD'.
        * Date is between '2022-01-01' and '2023-12-31'.
    * **Ranking:** Top 3 (`ORDER BY ... DESC LIMIT 3`).

2.  **Table & Schema Discovery:** I need to find tables with vendor info, sales, event details, and dates.
    * **Tool Call 1 (Find Tables):** `internal_execute_sql_query(sql_query="SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\`;")`
    * *(Self-Correction/Analysis): From the results, `OUT_DIM_VENDORS_MASTER` and `OUT_FACT_EVENT_SALES` look promising.*
    * **Tool Call 2 (Get Schemas):** `internal_execute_sql_query(sql_query="SELECT column_name, data_type FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS\` WHERE table_name IN ('OUT_DIM_VENDORS_MASTER', 'OUT_FACT_EVENT_SALES');")`
    * *(Self-Correction/Analysis): The schemas confirm the tables can be joined on `vendor_id`. `OUT_DIM_VENDORS_MASTER` has `vendor_email` and `identity_category`. `OUT_FACT_EVENT_SALES` has `event_name`, `transaction_date`, and `sales_total`.*

3.  **Final SQL Formulation & Execution:** Now I have all the pieces to build the query.
    * **Tool Call 3 (Final Query):**
        ```sql
        internal_execute_sql_query(sql_query="
        SELECT
            V.vendor_email,
            SUM(S.sales_total) AS total_revenue
        FROM
            \`kbc-use4-839-261b.WORKSPACE_21894820.OUT_FACT_EVENT_SALES\` AS S
        JOIN
            \`kbc-use4-839-261b.WORKSPACE_21894820.OUT_DIM_VENDORS_MASTER\` AS V ON S.vendor_id = V.vendor_id
        WHERE
            V.identity_category = 'Apparel'
            AND S.event_name LIKE '%UNDSCVRD%'
            AND S.transaction_date BETWEEN '2022-01-01' AND '2023-12-31'
        GROUP BY
            V.vendor_email
        ORDER BY
            total_revenue DESC
        LIMIT 3;
        ")
        ```
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


def query_database_tool(query: str) -> dict:
    """
    Queries a database for data and returns it in a structured format for tables.
    Use this when the user asks to see data, tables, or reports.
    """
    print(f"--- Tool: query_database_tool called with query: {query} ---")
    # Mock data for demonstration. Replace with actual BigQuery/DB calls.
    if "orders" in query.lower():
        table_data = {
            "headers": ["Order ID", "Product", "Amount", "Status"],
            "rows": [
                ["1001", "Laptop", 1200, "Shipped"],
                ["1002", "Mouse", 25, "Processing"],
                ["1003", "Keyboard", 75, "Shipped"],
            ]
        }
        # The return format is critical for the frontend
        return {"status": "success", "type": "table", "data": table_data}
    else:
        return {"status": "error", "message": "I can only provide data for 'orders'."}


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
    get_zip_codes_for_city, get_current_time, query_database_tool
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

                # Look for the most recent function response with data
                for message_content in reversed(retrieved_history):
                    if query_data:  # Already found data, break
                        break
                        
                    for part in message_content.parts:
                        if hasattr(part, 'function_response') and part.function_response:
                            app.logger.info(
                                f"Found function_response in history from tool: {part.function_response.name}"
                            )
                            try:
                                func_tool_result = part.function_response.response
                                app.logger.info(
                                    f"Tool '{part.function_response.name}' raw returned dict: {str(func_tool_result)[:300]}..."
                                )

                                # Handle nested result structure
                                if isinstance(func_tool_result, dict) and 'result' in func_tool_result:
                                    app.logger.info(
                                        f"Found nested result structure, extracting: {str(func_tool_result['result'])[:300]}..."
                                    )
                                    func_tool_result = func_tool_result['result']

                                # Check for successful tool execution with data
                                if isinstance(func_tool_result, dict) and \
                                   func_tool_result.get('status') in ['success', 'success_truncated'] and \
                                   'data' in func_tool_result:

                                    retrieved_data_from_tool = func_tool_result['data']
                                    app.logger.info(
                                        f"Found data in tool result, type: {type(retrieved_data_from_tool)}, length: {len(retrieved_data_from_tool) if isinstance(retrieved_data_from_tool, list) else 'N/A'}"
                                    )

                                    # Ensure data is a list of dictionaries for table display
                                    if isinstance(retrieved_data_from_tool, list) and \
                                       (not retrieved_data_from_tool or all(isinstance(item, dict) for item in retrieved_data_from_tool)):
                                        
                                        query_data = retrieved_data_from_tool
                                        
                                        # Set appropriate display title
                                        tool_display_title = func_tool_result.get("display_title")
                                        if not tool_display_title:
                                            if part.function_response.name == "internal_execute_sql_query":
                                                if any(item.get('table_name') for item in retrieved_data_from_tool if isinstance(item, dict)):
                                                    tool_display_title = "Available Data Tables"
                                                else:
                                                    tool_display_title = "SQL Query Results"
                                            else:
                                                tool_display_title = f"Results from {part.function_response.name}"
                                        
                                        app.logger.info(
                                            f"SUCCESS: Data for display extracted from tool '{part.function_response.name}' with {len(query_data)} items. Title: {tool_display_title}"
                                        )
                                        break  # Found data, stop looking
                                        
                                elif isinstance(func_tool_result, dict) and func_tool_result.get('status') == 'error':
                                    app.logger.warning(
                                        f"Tool {part.function_response.name} executed with error: {func_tool_result.get('error_message')}"
                                    )

                            except Exception as e_parse_hist:
                                app.logger.error(
                                    f"Error parsing function_response from history for tool {part.function_response.name}: {e_parse_hist}",
                                    exc_info=True)
                                    
            except AttributeError as e_attr:
                app.logger.error(
                    f"'ChatSession' object might not have 'get_history' or it failed: {e_attr}"
                )
            except Exception as e_hist:
                app.logger.error(
                    f"An error occurred while trying to get or process chat history: {e_hist}",
                    exc_info=True)

        app.logger.info(
            f"After history check - query_data type: {type(query_data)}, Is None: {query_data is None}, Length (if list): {len(query_data) if isinstance(query_data, list) else 'N/A'}"
        )

        # Always try to create table display if we have valid data
        if query_data is not None and isinstance(query_data, list):
            displays.append({
                "type": "table",
                "title": tool_display_title,
                "content": query_data
            })
            app.logger.info(
                f"Created table display for '{tool_display_title}' with {len(query_data)} rows."
            )
        else:
            app.logger.warning(
                "No structured query_data extracted from tool calls for display. Attempting fallback strategies."
            )
            
            # Enhanced fallback: try to extract and re-execute if we can identify table operations
            if final_answer and isinstance(final_answer, str):
                # Look for table references in the AI response
                import re
                table_pattern = r'\b(OUT_[A-Z_0-9]+|DIM_[A-Z_0-9]+|FACT_[A-Z_0-9]+|STG_[A-Z_0-9]+)\b'
                table_matches = re.findall(table_pattern, final_answer, re.IGNORECASE)
                
                # Check if response suggests tables were retrieved or listed
                table_keywords = [
                    "tables in your", "bigquery workspace", "list of tables", 
                    "here are the tables", "retrieved all the tables",
                    "table below", "retrieved the data", "query results",
                    "sample row from", "displaying", "will be displayed"
                ]
                
                has_table_context = any(phrase in final_answer.lower() for phrase in table_keywords)
                
                if has_table_context or table_matches:
                    app.logger.info(f"Fallback: AI response suggests table data. Found tables: {table_matches}")
                    
                    try:
                        if table_matches:
                            # Use the first table found for data display
                            table_name = table_matches[0]
                            fallback_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{table_name}` LIMIT 10"
                            fallback_title = f"Data from {table_name}"
                            app.logger.info(f"Fallback: Executing query for table '{table_name}'")
                        else:
                            # Default to listing tables
                            fallback_query = "SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES` ORDER BY table_name"
                            fallback_title = "Available Tables"
                            app.logger.info("Fallback: Listing available tables")
                        
                        fallback_result = internal_execute_sql_query(fallback_query)
                        if fallback_result.get('status') in ['success', 'success_truncated'] and fallback_result.get('data'):
                            displays.append({
                                "type": "table",
                                "title": fallback_title,
                                "content": fallback_result['data']
                            })
                            app.logger.info(f"Fallback successful: Created display '{fallback_title}' with {len(fallback_result['data'])} rows")
                        else:
                            app.logger.warning(f"Fallback query failed: {fallback_result.get('error_message', 'No data returned')}")
                            
                    except Exception as e_fallback:
                        app.logger.error(f"Error during fallback display generation: {e_fallback}", exc_info=True)
            
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
