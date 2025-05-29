import os
from flask import Flask, jsonify, request

# Using the import style from the documentation you provided and confirmed works for you.
# If these imports fail, the script will fail, indicating an environment/install issue
# with the specific 'google-generativeai' or 'google-cloud-aiplatform' package
# that provides 'from google import genai'.
from google import genai
from google.genai import types as google_genai_types

from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import logging
import json
import time 
import re

# --- Initialize Flask App ---
app = Flask(__name__)
logging.basicConfig(level=logging.INFO)
app.logger.setLevel(logging.INFO)

# --- Load Configuration ---
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')

# --- Initialize Keboola and BigQuery Clients ---
keboola_storage_client = None
try:
    if KBC_API_URL and KBC_STORAGE_TOKEN:
        app.logger.info(f"Initializing Keboola Storage Client with URL: {KBC_API_URL}")
        keboola_storage_client = KeboolaStorageClient(KBC_API_URL, KBC_STORAGE_TOKEN)
        app.logger.info("Successfully initialized Keboola Storage Client.")
    else: app.logger.error("CRITICAL (Keboola Client): KBC_API_URL or KBC_STORAGE_TOKEN not set.")
except Exception as e: app.logger.error(f"Error initializing Keboola Storage Client: {e}", exc_info=True)

bigquery_client = None
try:
    if GOOGLE_APPLICATION_CREDENTIALS_PATH:
        app.logger.info(f"Initializing Google BigQuery Client using credentials: {GOOGLE_APPLICATION_CREDENTIALS_PATH}")
        bigquery_client = bigquery.Client.from_service_account_json(GOOGLE_APPLICATION_CREDENTIALS_PATH)
        app.logger.info(f"Successfully initialized Google BigQuery Client. Project: {bigquery_client.project}")
    else: app.logger.error("CRITICAL (BigQuery Client): GOOGLE_APPLICATION_CREDENTIALS path not set.")
except Exception as e: app.logger.error(f"Error initializing Google BigQuery Client: {e}", exc_info=True)

# --- Tool Functions (for Automatic Function Calling) ---
def list_keboola_buckets() -> dict:
    """Lists all available top-level data categories (buckets) in the Keboola Storage project. This helps understand the overall data landscape. The data from this tool is suitable for table display.
    Returns: A dictionary with 'status': 'success' and 'data': [{'id', 'name', 'stage'}], or 'status': 'error' and 'error_message'.
    """
    app.logger.info("Tool Call: list_keboola_buckets")
    if not keboola_storage_client: return {"status": "error", "error_message": "Keboola client not initialized."}
    try:
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = [{"id": b.get("id"), "name": b.get("name"), "stage": b.get("stage")} for b in buckets_data or []]
        app.logger.info(f"Tool Call: list_keboola_buckets found {len(bucket_info)} buckets.")
        return {"status": "success", "data": bucket_info, "display_type": "table", "display_title": "Keboola Storage Buckets"}
    except Exception as e:
        app.logger.error(f"Tool Call: Error listing Keboola buckets: {e}", exc_info=True)
        return {"status": "error", "error_message": str(e)}

def list_tables_in_keboola_bucket(bucket_id: str) -> dict:
    """Lists all tables within a specified Keboola Storage bucket, providing their names and row counts. Use this after identifying a relevant bucket. The data is suitable for table display.
    Args:
        bucket_id (str): The ID of the Keboola Storage bucket (e.g., 'in.c-mybucket').
    Returns: A dictionary with 'status': 'success' and 'data': [{'id', 'name', 'rowsCount'}], or 'status': 'error' and 'error_message'.
    """
    app.logger.info(f"Tool Call: list_tables_in_keboola_bucket for bucket_id: {bucket_id}")
    if not keboola_storage_client: return {"status": "error", "error_message": "Keboola client not initialized."}
    try:
        tables_data = keboola_storage_client.buckets.list_tables(bucket_id=bucket_id)
        table_info = [{"id": t.get("id"), "name": t.get("name"), "rowsCount": t.get("rowsCount")} for t in tables_data or []]
        app.logger.info(f"Tool Call: Found {len(table_info)} tables in bucket {bucket_id}.")
        return {"status": "success", "data": table_info, "display_type": "table", "display_title": f"Tables in Bucket: {bucket_id}"}
    except Exception as e:
        app.logger.error(f"Tool Call: Error listing tables for bucket {bucket_id}: {e}", exc_info=True)
        return {"status": "error", "error_message": f"Could not list tables for bucket {bucket_id}: {str(e)}"}

def get_keboola_table_detail(table_id: str) -> dict:
    """Retrieves detailed schema information for a specific Keboola Storage table (columns and their data types). Use this before writing an SQL query. The column information is suitable for table display.
    Args:
        table_id (str): Full Keboola Storage table ID (e.g., 'in.c-mybucket.mytable').
    Returns: A dictionary with 'status': 'success' and 'data': {'id', 'name', 'columns':[{'name','type'}], 'rowsCount'}, or 'status': 'error' and 'error_message'.
    """
    app.logger.info(f"Tool Call: get_keboola_table_detail for table_id: {table_id}")
    if not keboola_storage_client: return {"status": "error", "error_message": "Keboola client not initialized."}
    try:
        detail = keboola_storage_client.tables.detail(table_id=table_id)
        columns_info = []
        if detail and 'columns' in detail:
            for col_name in detail['columns']:
                col_type = "string"; 
                if 'columnMetadata' in detail and col_name in detail['columnMetadata']:
                    for meta_item in detail['columnMetadata'][col_name]:
                        if meta_item.get('key') == 'KBC.datatype.type': col_type = meta_item.get('value', 'string'); break
                elif detail.get("primaryKey") and col_name in detail.get("primaryKey"): col_type = "string" 
                columns_info.append({"name": col_name, "type": col_type})
        table_detail_info = {"id": detail.get("id"), "name": detail.get("name"), "columns": columns_info, "rowsCount": detail.get("rowsCount")}
        app.logger.info(f"Tool Call: Retrieved details for table {table_id}.")
        return {"status": "success", "data": table_detail_info, "display_type": "table_detail", "display_title": f"Schema for Table: {detail.get('name', table_id)}"}
    except Exception as e:
        app.logger.error(f"Tool Call: Error getting table detail for {table_id}: {e}", exc_info=True)
        return {"status": "error", "error_message": f"Could not get table detail for {table_id}: {str(e)}"}

def execute_sql_query(sql_query: str) -> dict:
    """Executes a BigQuery SQL query against the Keboola project's data warehouse (dataset: WORKSPACE_21894820, project: kbc-use4-839-261b). The data is suitable for table display.
    Args:
        sql_query (str): The BigQuery SQL SELECT query. Example: 'SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.YOUR_TABLE_NAME` LIMIT 10'.
    Returns: A dictionary with 'status' and either 'data' (list of rows), 'message' (for truncation), or 'error_message'.
    """
    if not bigquery_client:
        msg = "BigQuery client not initialized."
        app.logger.error(f"Tool call execute_sql_query: {msg}")
        return {"status": "error", "error_message": msg}
    app.logger.info(f"Tool Call: execute_sql_query with query: {sql_query}")
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=60) 
        rows_list = [dict(row) for row in results]
        app.logger.info(f"Tool Call: query executed, returned {len(rows_list)} rows.")
        display_title = "Query Results"
        match = re.search(r"FROM\s+`[^`]+\.[^`]+\.([^`]+)`", sql_query, re.IGNORECASE)
        if match: display_title = f"Data from: {match.group(1)}"

        result_payload = {"status": "success", "data": rows_list, "display_type": "table", "display_title": display_title}
        if len(json.dumps(rows_list)) > 3800: 
            app.logger.warning(f"Query result is very large. Truncating for LLM response and display.")
            return {"status": "success_truncated", "message": f"Query returned {len(rows_list)} rows. Displaying first 3.", "data": rows_list[:3], "display_type": "table", "display_title": display_title + " (Sample)"}
        return result_payload
    except Exception as e:
        app.logger.error(f"Tool Call: Error executing BigQuery query: {e}", exc_info=True)
        return {"status": "error", "error_message": f"Error executing BigQuery query: {str(e)}"}

def get_current_time() -> dict:
    """Returns the current date, time, and timezone.
    Returns: A dictionary with 'status':'success' and 'current_time'.
    """
    app.logger.info("Tool Call: get_current_time")
    current_time_str = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    return {"status": "success", "current_time": current_time_str}

gemini_tool_functions_list = [
    list_keboola_buckets,
    list_tables_in_keboola_bucket,
    get_keboola_table_detail,
    execute_sql_query,
    get_current_time
]

# --- Initialize Gemini Client using genai.Client and GenerateContentConfig ---
gemini_client_instance = None 
gemini_chat_config_with_tools = None 

if GEMINI_API_KEY: # Check if 'from google import genai' worked
    try:
        app.logger.info("Initializing google.genai.Client with API key...")
        gemini_client_instance = genai.Client(api_key=GEMINI_API_KEY) # Uses 'genai' from 'from google import genai'
        app.logger.info("Successfully initialized google.genai.Client.")

        app.logger.info(f"Defining tools for Gemini: {[f.__name__ for f in gemini_tool_functions_list]}")

        # Create GenerateContentConfig with Python function objects as tools
        # This relies on "Automatic Function Calling"
        gemini_chat_config_with_tools = google_genai_types.GenerateContentConfig(
            tools=gemini_tool_functions_list
        )
        app.logger.info("Gemini GenerateContentConfig with tools created successfully.")

    except AttributeError as ae: # Catch if genai.types.Tool or .GenerateContentConfig are missing
        app.logger.error(f"AttributeError during Gemini setup with google.genai.Client (e.g., google.genai.types.GenerateContentConfig or .Tool might be missing/different): {ae}. This SDK style might have issues with your installed version.", exc_info=True)
        gemini_client_instance = None; gemini_chat_config_with_tools = None
    except Exception as e:
        app.logger.error(f"Error initializing Gemini client (genai.Client pattern) or its config: {e}", exc_info=True)
        gemini_client_instance = None; gemini_chat_config_with_tools = None
elif not GEMINI_API_KEY: 
    app.logger.error("CRITICAL (Gemini): GEMINI_API_KEY not set.")
elif not GEMINI_SDK_CLIENT_STYLE_AVAILABLE: 
    app.logger.error("CRITICAL (Gemini): SDK style 'from google import genai' was not available or failed import. Chat functionality will not work.")


# --- API Endpoints ---
@app.route('/')
def hello(): return "Hello from your custom Keboola API Gateway (genai.Client pattern)!"

# (Direct test endpoints remain useful)
@app.route('/api/test/list_buckets', methods=['GET'])
def test_list_keboola_buckets_flask_endpoint():
    result = list_keboola_buckets(); return jsonify(result.get("buckets") if result.get("status")=="success" else result), 200 if result.get("status")=="success" else 500
@app.route('/api/test/tables_in_bucket/<path:bucket_id>', methods=['GET'])
def test_list_tables_in_bucket_flask_endpoint(bucket_id):
    result = list_tables_in_keboola_bucket(bucket_id); return jsonify(result.get("tables") if result.get("status")=="success" else result), 200 if result.get("status")=="success" else 500
@app.route('/api/test/table_detail/<path:table_id>', methods=['GET'])
def test_get_keboola_table_detail_flask_endpoint(table_id):
    result = get_keboola_table_detail(table_id); return jsonify(result.get("table_detail") if result.get("status")=="success" else result), 200 if result.get("status")=="success" else 500
@app.route('/api/test/query_data', methods=['POST'])
def test_query_data_flask_endpoint():
    request_data = request.get_json(); sql_query = request_data.get('sql_query')
    if not sql_query: return jsonify({"error": "Missing 'sql_query'."}), 400
    result = execute_sql_query(sql_query)
    return jsonify(result.get("data") if result.get("status") in ["success", "success_truncated"] else result), 200 if result.get("status") in ["success", "success_truncated"] else 500

# --- CHAT ENDPOINT using genai.Client and Automatic Function Calling ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini_client_pattern():
    if not gemini_client_instance or not gemini_chat_config_with_tools: 
        app.logger.error("/api/chat called but Gemini client or tool config (genai.Client pattern) is not initialized.")
        return jsonify({"error": "Gemini (genai.Client pattern) not initialized. Check server startup logs."}), 500
    try:
        user_message_data = request.get_json()
        if not user_message_data or 'message' not in user_message_data:
            return jsonify({"error": "Missing 'message' in JSON payload."}), 400

        user_message_text = user_message_data['message']
        app.logger.info(f"Received user message for Gemini (genai.Client): {user_message_text}")

        # System prompt to guide the LLM's behavior and tool usage
        # This will be combined with the user message as part of the 'contents'
        agent_system_instruction = """You are an expert Keboola Data Analyst Assistant. Your primary goal is to help users understand and retrieve insights from their data stored within a Keboola project. This project utilizes Keboola Storage (organized into 'buckets' containing 'tables') and a Google BigQuery data warehouse (project ID: `kbc-use4-839-261b`, dataset/workspace schema: `WORKSPACE_21894820`) for querying data that has been loaded into the workspace.



You have the following tools at your disposal to achieve this:



1. `list_keboola_buckets`:

* **Description:** Lists all available top-level data categories (buckets) in the Keboola Storage project. Use this as a first step to understand the overall data landscape if the user's query is broad or if specific table locations are unknown.

* **Parameters:** None.

* **Returns:** A list of bucket objects, each containing 'id', 'name', and 'stage'.



2. `list_tables_in_keboola_bucket`:

* **Description:** Lists all specific datasets (tables) and their row counts within a chosen Keboola Storage bucket. Use this after identifying a relevant bucket to see what specific data tables it contains.

* **Parameters:**

* `bucket_id` (string, required): The ID of the Keboola Storage bucket (e.g., 'in.c-mydata' or 'out.c-transformeddata') from which to list tables.

* **Returns:** A list of table objects, each containing 'id', 'name', and 'rowsCount'.



3. `get_keboola_table_detail`:

* **Description:** Retrieves the detailed schema (column names, data types) and other metadata (like row count, primary key) for a specific Keboola Storage table. **Crucially, use this tool to understand a table's structure *before* attempting to write an SQL query for it.** This helps ensure your SQL query is accurate.

* **Parameters:**

* `table_id` (string, required): The full ID of the Keboola Storage table (e.g., 'in.c-mybucket.mytable' or 'out.c-transformeddata.final_output_table').

* **Returns:** An object containing 'id', 'name', 'columns' (a list of objects, each with 'name' and 'type'), 'rowsCount', and 'primaryKey'.



4. `execute_sql_query`:

* **Description:** Executes a BigQuery SQL SELECT query against the Keboola project's data warehouse (specifically dataset `WORKSPACE_21894820` within project `kbc-use4-839-261b`) and returns the results. Use this tool to answer specific questions about data content, perform calculations, filter data, or aggregate information once you have identified the correct table(s) and understand their schemas.

* **Parameters:**

* `sql_query` (string, required): The BigQuery SQL SELECT query to execute.

* **CRITICAL INSTRUCTIONS FOR SQL:**

* Table names in your SQL queries **MUST** be fully qualified: `` `kbc-use4-839-261b.WORKSPACE_21894820.ACTUAL_TABLE_NAME_FROM_WORKSPACE` ``. The `ACTUAL_TABLE_NAME_FROM_WORKSPACE` is the name of the table as it appears *after* being loaded into the BigQuery workspace via Keboola's Table Input Mapping (this might be different from its name in the Storage bucket if aliased).

* Use standard BigQuery SQL syntax.

* Quote column and table names with backticks (`) if they contain special characters or are reserved keywords.

* **Returns:** A list of dictionaries, where each dictionary represents a row from the query result, or an error if the query fails.



5. `get_current_time`:

* **Description:** Returns the current date, time, and timezone. Use only when explicitly asked for the time or date.

* **Parameters:** None.

* **Returns:** An object with 'current_time'.



**Your Task and Workflow:**



1. **Understand the User's Goal:** Carefully analyze the user's question to determine what information they are seeking.

2. **Explore (If Necessary):**

* If the user's query is vague, or if you don't know which tables contain the relevant data, **proactively use the metadata tools first.**

* Start with `list_keboola_buckets` to see available data categories.

* If a relevant bucket is identified (e.g., if the user mentions "Kapwa Gardens data" and you see a bucket like `out.c-squarespace-kapwa-gardens`), use `list_tables_in_keboola_bucket` with that bucket's ID to find relevant tables.

* Before querying a table with `execute_sql_query`, **always use `get_keboola_table_detail`** to understand its columns and data types. You can share key column names with the user to confirm relevance or to help them refine their question.

3. **Formulate SQL (When Ready):** Once you have identified the correct fully qualified table name(s) (e.g., `` `kbc-use4-839-261b.WORKSPACE_21894820.OUT_RESPONSE_ANSWERS_TYPEFORM` ``) and understand their schemas, construct a precise BigQuery SQL query to answer the user's question. This may involve `SELECT`, `WHERE`, `GROUP BY`, `SUM()`, `COUNT()`, `JOIN` (if you can infer relationships from table/column names or schema details), etc.

4.  Execute & Respond with Data for Display:
    * Call the appropriate tool (e.g., `execute_sql_query`, `list_keboola_buckets`, `list_tables_in_keboola_bucket`, `get_keboola_table_detail`).
    * The tool will return a dictionary including a "status" field, and if successful, a "data" field, a "display_type" field (e.g., "table", "table_detail"), and a "display_title" field.
    * **If the tool call is successful (status is "success" or "success_truncated") and returns a "data" payload suitable for visual display (especially from `execute_sql_query`, `list_keboola_buckets`, `list_tables_in_keboola_bucket`, or `get_keboola_table_detail` where the 'data' is tabular or a list):**
        * **Your primary textual response to the user MUST clearly and enthusiastically announce that the data has been retrieved and will be displayed as a table (or as appropriate).** You are setting the stage for the visual component.
        * **Examples of good textual lead-ins for displaying data:**
            * "Okay, I've retrieved the data you asked for! Here's a look at the [display_title, e.g., 'Kapwa Gardens Orders'] in a table:"
            * "I found [N] buckets. They will be displayed for you in a table below:"
            * "Here is the schema for table '[table_name]', which will be shown in a table format:"
            * "The SQL query results are ready and will be displayed as a table. Here's a brief summary: [brief summary if appropriate, e.g., 'The query returned 10 rows showing...']"
        * **Do NOT attempt to format the full table data as text within your own reply.** Simply announce that the data is ready for display and provide a brief context or summary. The backend system is responsible for taking the raw "data" from the tool and passing it to the UI for rendering in the `CanvasDisplay` component.
        * If the tool result indicates `status: "success_truncated"` (because the data was too large), clearly state this in your textual reply, for example: "I've retrieved a sample of the data as the full set was very large. It will be displayed for you in a table below:"
    * If the tool does not return data suitable for a table display (e.g., `get_current_time` just returns a time string), provide the information directly in your textual reply.


5. **Handle Tool Errors:** All tools will return a dictionary with a `"status"` field.

* If `status` is `"error"`, an `"error_message"` field will be present. Inform the user clearly about the error (e.g., "I tried to query the data, but encountered an error: [error_message]"). Do not try the exact same failing query again without modification or further information.

* If `status` is `"success_truncated"`, an additional `"message"` field will explain that the results were too large and a sample might be in the `"data"` field. Inform the user about this.

6. **Clarify Ambiguity:** If the user's request is unclear, if you cannot find relevant tables, or if you are unsure how to construct a query, **ask clarifying questions.** Do not guess if it might lead to incorrect or costly queries. For example, ask "Which specific dataset are you interested in for 'Kapwa Gardens'?" or "To count people, which column in the table XYZ represents a unique person?"



**Tone:** Be helpful, professional, data-savvy, and precise. Your goal is to act as an expert data analyst for their Keboola project.



**Example Scenario (Internal Thought Process):**

User: "How many people went to Undiscovered events versus Kapwa Garden events?"

Your thought process might be:

1. "Okay, I need to compare attendee counts for two event types: 'Undiscovered' and 'Kapwa Garden'."

2. "First, I need to find tables related to these event types and attendance."

3. Call `list_keboola_buckets`. (User sees results, maybe you find `out.c-undiscovered-events` and `out.c-kapwa-garden-events`).

4. Call `list_tables_in_keboola_bucket` for `out.c-undiscovered-events`. (Finds `ATTENDEE_LIST_UNDISCOVERED`).

5. Call `list_tables_in_keboola_bucket` for `out.c-kapwa-garden-events`. (Finds `ATTENDEE_LIST_KAPWA`).

6. Call `get_keboola_table_detail` for `out.c-undiscovered-events.ATTENDEE_LIST_UNDISCOVERED` to find the person identifier column.

7. Call `get_keboola_table_detail` for `out.c-kapwa-garden-events.ATTENDEE_LIST_KAPWA` for its person identifier.

8. Formulate SQL Query 1: `SELECT COUNT(DISTINCT person_id_column) AS undiscovered_attendees FROM \`kbc-use4-839-261b.WORKSPACE_21894820.ATTENDEE_LIST_UNDISCOVERED\`;`

9. Call `execute_sql_query` with Query 1. Get result (e.g., `{"undiscovered_attendees": 150}`).

10. Formulate SQL Query 2: `SELECT COUNT(DISTINCT person_id_column) AS kapwa_attendees FROM \`kbc-use4-839-261b.WORKSPACE_21894820.ATTENDEE_LIST_KAPWA\`;`

11. Call `execute_sql_query` with Query 2. Get result (e.g., `{"kapwa_attendees": 200}`).

12. Synthesize answer: "There were 150 attendees for Undiscovered events and 200 for Kapwa Garden events."



Strive to use the tools efficiently to answer the user's questions about their Keboola data."""
        # For genai.Client().models.generate_content(), contents can be a list of turns or just the current prompt.
        # To include a system instruction, you often make it the first "user" or "model" turn, or some models
        # support a 'system_instruction' parameter directly in GenerateContentRequest.
        # The 'google.genai.types.Content' and 'Part' are used here.

        # Constructing content with a system-like instruction and then the user message
        # This is one way to provide system context for client.models.generate_content
        # The 'Automatic Function Calling' example shows `contents="What's the temp in Boston?"` directly.
        # For more complex instructions, providing it as part of the prompt string is common if system_instruction isn't available.

        # The documentation for client.chats.create() is more suitable for multi-turn and managing history.
        chat_session = gemini_client_instance.chats.create(
            model='gemini-2.0-flash', 
            config=gemini_chat_config_with_tools,
            # System instruction can often be part of the initial history or a specific parameter
            # Let's try passing the instruction as the first part of the history.
            history=[
                google_genai_types.Content(role="user", parts=[google_genai_types.Part(text="System Pre-computation Context: \n" + agent_system_instruction)]), # System prompt first turn as user
                google_genai_types.Content(role="model", parts=[google_genai_types.Part(text="Okay, I understand my role and how to use the tools. How can I help you?")]) # Model ack
            ]
        )
        app.logger.info(f"Created Gemini chat session. Sending user message: '{user_message_text}'")

        response = chat_session.send_message(user_message_text)

        final_answer_text = ""
        tool_data_for_display = [] 
        try:
            final_answer_text = response.text 
            app.logger.info(f"Gemini final answer (genai.Client/chat auto): {final_answer_text}")

            # Extract structured data from the *last model turn* if it was a function response used by Gemini
            # The 'chat_session.history' will contain the sequence of user, model (with function call), and tool (with function response) turns.
            # The last "tool" role part in history for a relevant function would have our structured data.
            if chat_session.history:
                for turn_content in reversed(chat_session.history):
                    if turn_content.role == "tool": # Function results are added to history with role "tool" by the SDK
                        for part in turn_content.parts:
                            # The SDK wraps our Python function's dict return into part.function_response.response['content']
                            if hasattr(part, 'function_response') and part.function_response:
                                fr = part.function_response
                                if hasattr(fr, 'response') and isinstance(fr.response, dict):
                                    tool_output_content = fr.response.get('content')
                                    if isinstance(tool_output_content, dict) and tool_output_content.get("status") in ["success", "success_truncated"]:
                                        data = tool_output_content.get("data")
                                        display_type = tool_output_content.get("display_type")
                                        display_title = tool_output_content.get("display_title")
                                        if data and display_type:
                                            tool_data_for_display.append({
                                                "type": display_type,
                                                "title": display_title or f"Results from {fr.name}",
                                                "content": data
                                            })
                                            app.logger.info(f"Extracted data from tool '{fr.name}' (name in response: {fr.name}) for display array.")
                                            # Break if we found data from the most recent relevant tool call
                                            # This ensures we only get data from the last tool chain that produced the final answer
                                            break 
                        if tool_data_for_display: # If display data found for this turn, stop.
                            break
        except ValueError as ve: 
            app.logger.error(f"Gemini response did not directly yield text: {ve}. Full response object: {response}", exc_info=True)
            final_answer_text = "I had an issue processing that request fully. Could you try rephrasing?"
            if hasattr(response, 'parts') and response.parts: app.logger.warning(f"Model parts that might be relevant: {response.parts}")
        except Exception as e_gen:
            app.logger.error(f"Generic error processing Gemini response: {e_gen}", exc_info=True)
            final_answer_text = "Sorry, an unexpected error occurred while getting the response from the AI."

        response_payload = {"reply": final_answer_text}
        if tool_data_for_display:
            response_payload["displays"] = tool_data_for_display
        return jsonify(response_payload)

    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint (genai.Client pattern): {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500

# --- Main Execution ---
if __name__ == '__main__':
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    app.logger.info(f"KBC_STORAGE_TOKEN from env: {'SET' if KBC_STORAGE_TOKEN else 'NOT SET'}")
    app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS from env: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'NOT SET'}")
    app.logger.info(f"KBC_WORKSPACE_SCHEMA from env (BigQuery Dataset ID): {'SET' if KBC_WORKSPACE_SCHEMA else 'NOT SET'}")
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    app.run(host='0.0.0.0', port=8080, debug=True)