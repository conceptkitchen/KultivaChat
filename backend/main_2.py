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
        rows_list = [dict(row) for row in results]
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
        enhanced_prompt = f"""You are a data analyst assistant with access to a Keboola BigQuery data warehouse. 
        
When users ask about:
- Tables, data, or datasets: Use the internal_execute_sql_query tool to query the database
- "Show me tables" or "what tables do I have": Query INFORMATION_SCHEMA.TABLES to list tables
- Specific data like "kapwa gardens": Search for it in the available tables
- Data analysis requests: Write and execute appropriate SQL queries

The database details:
- Project: kbc-use4-839-261b
- Dataset: WORKSPACE_21894820
- Always use fully qualified table names: `kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME`

User question: {user_message}"""
        
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
        
        # If the response contains table names or data, format it as a structured display
        if "tables in your" in final_answer.lower() or "bigquery dataset" in final_answer.lower():
            # Extract table names from the response - handle both formats
            lines = final_answer.split('\n')
            table_names = []
            for line in lines:
                if line.strip().startswith('- '):
                    table_name = line.strip()[2:]  # Remove "- " prefix
                    if table_name:
                        table_names.append(table_name)
            
            if table_names:
                displays.append({
                    "type": "table",
                    "title": "Your Data Tables",
                    "content": [{"Table Name": name} for name in table_names]
                })
        
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