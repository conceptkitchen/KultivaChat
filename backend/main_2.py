import os
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery

# Using the import style from the documentation you provided
try:
    from google import genai as google_genai_for_client # Alias to avoid conflict with older genai import
    from google.genai import types as google_genai_types
    GEMINI_SDK_AVAILABLE = True
    print("Successfully imported 'google.genai' and 'google.genai.types'")
except ImportError as e:
    print(f"Failed to import 'google.genai' or 'google.genai.types': {e}. Make sure 'google-generativeai' is installed and accessible with this import style (it might be part of google-cloud-aiplatform or a specific version).")
    GEMINI_SDK_AVAILABLE = False
    # Define dummy classes if import fails, so Flask app can still load other routes
    class google_genai_types:
        class GenerateContentConfig: pass
        class Content: pass
        class Part: pass
    class google_genai_for_client:
        class Client: pass

# We still need HarmCategory and HarmBlockThreshold, try from google.generativeai.types
try:
    from google.generativeai.types import HarmCategory, HarmBlockThreshold
except ImportError:
    # Fallback if the primary SDK doesn't have them at this path either
    class HarmCategory: HARM_CATEGORY_HARASSMENT=None; HARM_CATEGORY_HATE_SPEECH=None; HARM_CATEGORY_SEXUALLY_EXPLICIT=None; HARM_CATEGORY_DANGEROUS_CONTENT=None # Add all relevant
    class HarmBlockThreshold: BLOCK_MEDIUM_AND_ABOVE=None; BLOCK_NONE=None # Add all relevant

import logging
import json
import time 

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
        app.logger.info(f"Attempting to initialize Keboola Storage Client with URL: {KBC_API_URL}")
        keboola_storage_client = KeboolaStorageClient(KBC_API_URL, KBC_STORAGE_TOKEN)
        app.logger.info("Successfully initialized Keboola Storage Client.")
    else: app.logger.error("CRITICAL (Keboola Client): KBC_API_URL or KBC_STORAGE_TOKEN not set.")
except Exception as e: app.logger.error(f"Error initializing Keboola Storage Client: {e}", exc_info=True)

bigquery_client = None
try:
    if GOOGLE_APPLICATION_CREDENTIALS_PATH:
        app.logger.info(f"Attempting to initialize Google BigQuery Client using credentials from: {GOOGLE_APPLICATION_CREDENTIALS_PATH}")
        bigquery_client = bigquery.Client.from_service_account_json(GOOGLE_APPLICATION_CREDENTIALS_PATH)
        app.logger.info(f"Successfully initialized Google BigQuery Client. Project: {bigquery_client.project}")
    else: app.logger.error("CRITICAL (BigQuery Client): GOOGLE_APPLICATION_CREDENTIALS path not set.")
except Exception as e: app.logger.error(f"Error initializing Google BigQuery Client: {e}", exc_info=True)

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
        app.logger.error(f"Tool call execute_sql_query: {msg}")
        return {"status": "error", "error_message": msg}
    app.logger.info(f"Tool Call: execute_sql_query with query: {sql_query}")
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=60) 
        rows_list = []
        for row in results:
            row_dict = {}
            for key, value in dict(row).items():
                # Convert Decimal objects to float to avoid JSON serialization issues
                if hasattr(value, '__class__') and 'Decimal' in str(type(value)):
                    row_dict[key] = float(value)
                else:
                    row_dict[key] = value
            rows_list.append(row_dict)
        app.logger.info(f"Tool Call: query executed, returned {len(rows_list)} rows.")
        result_payload = {"status": "success", "data": rows_list}
        return result_payload 
    except Exception as e:
        app.logger.error(f"Tool Call: Error executing BigQuery query: {e}", exc_info=True)
        return {"status": "error", "error_message": f"Error executing BigQuery query: {str(e)}"}

def get_current_time() -> dict:
    """Returns the current date, time, and timezone.
    Returns:
        dict: A dictionary containing the current time string with a key 'current_time' and 'status'.
    """
    app.logger.info("Tool Call: get_current_time")
    current_time_str = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    return {"status": "success", "current_time": current_time_str}

gemini_tool_functions_list = [internal_execute_sql_query, get_current_time]

# --- Initialize Gemini Client (using genai.Client and GenerateContentConfig) ---
gemini_sdk_client = None # Renamed to avoid confusion
gemini_generation_config_with_tools = None

if GEMINI_API_KEY and GEMINI_SDK_AVAILABLE:
    try:
        app.logger.info("Initializing google.genai.Client with API key...")
        gemini_sdk_client = google_genai_for_client.Client(api_key=GEMINI_API_KEY)
        app.logger.info("Successfully initialized google.genai.Client.")

        app.logger.info(f"Defining tools for Gemini: {[f.__name__ for f in gemini_tool_functions_list]}")
        # Create GenerateContentConfig with the Python function objects as tools
        gemini_generation_config_with_tools = google_genai_types.GenerateContentConfig(
            tools=gemini_tool_functions_list,
            # Optional: Add safety settings if needed directly in GenerateContentConfig
            # safety_settings=[...] 
            # Optional: Configure function calling mode if needed, e.g., "ANY"
            # tool_config=google_genai_types.ToolConfig(
            #    function_calling_config=google_genai_types.FunctionCallingConfig(mode="ANY")
            # )
        )
        app.logger.info("Gemini GenerateContentConfig with tools created successfully.")

    except Exception as e:
        app.logger.error(f"Error initializing Gemini client or GenerateContentConfig: {e}", exc_info=True)
        gemini_sdk_client = None
        gemini_generation_config_with_tools = None
else:
    if not GEMINI_API_KEY: app.logger.error("CRITICAL (Gemini): GEMINI_API_KEY not set.")
    if not GEMINI_SDK_AVAILABLE: app.logger.error("CRITICAL (Gemini): SDK 'google.genai' not available.")

# --- API Endpoints ---
@app.route('/')
def hello(): return "Hello from your custom Keboola API Gateway (using genai.Client)!"

# (Your existing /api/list_buckets, etc. can remain for direct testing)
@app.route('/api/list_buckets', methods=['GET'])
def list_keboola_buckets_endpoint():
    if not keboola_storage_client: return jsonify({"error": "Keboola client not initialized."}), 500
    try:
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = [{"id": b.get("id"), "name": b.get("name"), "stage": b.get("stage"), "uri": b.get("uri")} for b in buckets_data or []]
        return jsonify(bucket_info)
    except Exception as e: return jsonify({"error": str(e)}), 500

@app.route('/api/buckets/<path:bucket_id>/tables', methods=['GET'])
def list_tables_in_bucket_endpoint(bucket_id):
    if not keboola_storage_client: return jsonify({"error": "Keboola client not initialized."}), 500
    try:
        tables_data = keboola_storage_client.buckets.list_tables(bucket_id=bucket_id)
        table_info = [{"id": t.get("id"), "name": t.get("name"), "uri": t.get("uri"), "rowsCount": t.get("rowsCount")} for t in tables_data or []]
        return jsonify(table_info)
    except Exception as e: return jsonify({"error": str(e)}), 500

@app.route('/api/query_data', methods=['POST'])
def query_data_endpoint():
    request_data = request.get_json()
    if not request_data or 'sql_query' not in request_data:
        return jsonify({"error": "Missing 'sql_query' in JSON payload."}), 400
    sql_query = request_data['sql_query']
    result = internal_execute_sql_query(sql_query)
    if isinstance(result, dict) and result.get("status") != "success" and result.get("status") != "success_truncated":
        return jsonify(result), 500
    return jsonify(result)

# --- CHAT ENDPOINT using genai.Client and Automatic Function Calling ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini_client_style():
    if not gemini_sdk_client or not gemini_generation_config_with_tools:
        app.logger.error("/api/chat called but Gemini client or tool config is not initialized.")
        return jsonify({"error": "Gemini client/config not initialized. Check server logs."}), 500

    try:
        user_message_data = request.get_json()
        if not user_message_data or 'message' not in user_message_data:
            return jsonify({"error": "Missing 'message' in JSON payload."}), 400

        user_message = user_message_data['message']
        app.logger.info(f"Received user message for Gemini (genai.Client): {user_message}")

        # The documentation uses client.chats.create for multi-turn with automatic function calling.
        # Let's try to replicate that. We'll need to manage chat history if we want multi-turn.
        # For a single request-response with automatic function calling:
        # response = gemini_sdk_client.models.generate_content(...) from the doc might be simpler.
        # Let's try the chat style first.

        # For simplicity, we'll create a new chat for each request.
        # In a real app, you would manage chat history.
        chat_session = gemini_sdk_client.chats.create(
            model='gemini-2.0-flash', # Your preferred model
            config=gemini_generation_config_with_tools,
            # History would go here for multi-turn
        )
        app.logger.info(f"Created Gemini chat session. Sending message: '{user_message}'")

        # Add context to help Gemini understand it should use the tools proactively
        enhanced_prompt = f"""You are an expert Keboola Data Analyst Assistant. Your primary goal is to help users understand and retrieve insights from their data stored within a Keboola project. This project utilizes Keboola Storage (organized into 'buckets' containing 'tables') and a Google BigQuery data warehouse (project ID: kbc-use4-839-261b, dataset/workspace schema: WORKSPACE_21894820) for querying data that has been loaded into the workspace.
        
When users ask about:
- Tables, data, or datasets: Use the internal_execute_sql_query tool to query the database
- "Show me tables" or "what tables do I have": Query INFORMATION_SCHEMA.TABLES to list tables
- Specific data like "kapwa gardens" or "Undiscovered" or "Balay Kreative" or "Kulivate Labs": Search for it in the available tables
- Data analysis requests: Write and execute appropriate SQL queries

The database details:
- Project: kbc-use4-839-261b
- Dataset: WORKSPACE_21894820
- Always use fully qualified table names: kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME

User question: {user_message}

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

* Table names in your SQL queries **MUST** be fully qualified: kbc-use4-839-261b.WORKSPACE_21894820.ACTUAL_TABLE_NAME_FROM_WORKSPACE. The ACTUAL_TABLE_NAME_FROM_WORKSPACE is the name of the table as it appears *after* being loaded into the BigQuery workspace via Keboola's Table Input Mapping (this might be different from its name in the Storage bucket if aliased).

* Use standard BigQuery SQL syntax.

* Quote column and table names with backticks if they contain special characters or are reserved keywords.

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



Strive to use the tools efficiently to answer the user's questions about their Keboola data.
"""
        
        response = chat_session.send_message(enhanced_prompt)

        # With automatic function calling, the SDK should handle the loop.
        # response.text should contain the final answer.
        final_answer = ""
        try:
            final_answer = response.text
            app.logger.info(f"Gemini final answer (genai.Client/chat): {final_answer}")
        except ValueError as ve:
            app.logger.error(f"Gemini response did not directly yield text: {ve}. Parts: {response.parts if hasattr(response, 'parts') else 'N/A'}", exc_info=True)
            if response.parts:
                final_answer = f"LLM finished with non-text parts: {str(response.parts)}"
            else:
                return jsonify({"error": f"LLM response error: {ve}"}), 500
        except Exception as e_gen:
            app.logger.error(f"Generic error accessing response.text: {e_gen}", exc_info=True)
            return jsonify({"error": f"Error processing LLM response: {str(e_gen)}"}), 500

        # Create displays for structured data visualization
        displays = []
        
        # If the response contains table names, format as a structured display
        if "tables in your" in final_answer.lower() or "bigquery dataset" in final_answer.lower():
            lines = final_answer.split('\n')
            table_names = []
            for line in lines:
                if line.strip().startswith('- '):
                    table_name = line.strip()[2:]
                    if table_name:
                        table_names.append(table_name)
            
            if table_names:
                displays.append({
                    "type": "table",
                    "title": "Your Data Tables",
                    "content": [{"Table Name": name} for name in table_names]
                })
        
        # Enhanced: Check if Gemini executed a data query and format results
        query_data = None
        if hasattr(response, '_result') and hasattr(response._result, 'candidates'):
            for candidate in response._result.candidates:
                if hasattr(candidate, 'content') and hasattr(candidate.content, 'parts'):
                    for part in candidate.content.parts:
                        if hasattr(part, 'function_response') and part.function_response:
                            try:
                                func_response = part.function_response
                                if hasattr(func_response, 'response') and isinstance(func_response.response, dict):
                                    func_result = func_response.response
                                    if func_result.get('status') == 'success' and 'data' in func_result:
                                        query_data = func_result['data']
                                        app.logger.info(f"Found query data with {len(query_data)} rows")
                                        break
                            except Exception as e:
                                app.logger.error(f"Error parsing function response: {e}")
        
        # Fallback: also check the original response structure
        if not query_data and hasattr(response, 'parts'):
            for part in response.parts:
                if hasattr(part, 'function_response'):
                    try:
                        func_result = part.function_response.response if hasattr(part.function_response, 'response') else part.function_response
                        if isinstance(func_result, dict) and func_result.get('status') == 'success' and 'data' in func_result:
                            query_data = func_result['data']
                            app.logger.info(f"Found query data (fallback) with {len(query_data)} rows")
                            break
                    except Exception as e:
                        app.logger.error(f"Error in fallback function response parsing: {e}")
        
        # If we found table data, create a table display
        if query_data and len(query_data) > 0:
            # Extract table name from the final answer for title
            table_title = "Query Results"
            if "OUT_FACT_ORDERS_KAPWA_GARDENS" in final_answer:
                table_title = "Kapwa Gardens Orders"
            elif "OUT_DIM_CUSTOMERS" in final_answer:
                table_title = "Customer Data"
            elif "OUT_DIM_PRODUCTS" in final_answer:
                table_title = "Product Data"
            
            displays.append({
                "type": "table",
                "title": table_title,
                "content": query_data
            })
            app.logger.info(f"Created table display with {len(query_data)} rows")
        
        return jsonify({
            "reply": final_answer,
            "displays": displays
        })

    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint (genai.Client style): {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500

# --- Main Execution ---
if __name__ == '__main__':
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    # ... (other env var checks) ...
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    if not GEMINI_SDK_AVAILABLE:
        app.logger.critical("CRITICAL ERROR: google.genai SDK style could not be imported. Chat functionality will not work.")
    elif not all([KBC_API_URL, KBC_STORAGE_TOKEN, GOOGLE_APPLICATION_CREDENTIALS_PATH, KBC_WORKSPACE_SCHEMA, GEMINI_API_KEY]):
        app.logger.critical("CRITICAL ERROR: One or more essential environment variables are missing. Server cannot function fully.")

    app.run(host='0.0.0.0', port=8081, debug=False, use_reloader=False)