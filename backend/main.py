import os
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import google.generativeai as genai # For Gemini
from google.generativeai.types import HarmCategory, HarmBlockThreshold # For safety settings
import logging
import json # For handling JSON data

# --- Initialize Flask App ---
app = Flask(__name__)

# --- Configure logging ---
logging.basicConfig(level=logging.INFO)
app.logger.setLevel(logging.INFO)

# --- Load Configuration from Environment Variables (Replit Secrets) ---
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA') # BigQuery Dataset ID
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')

# --- Initialize Clients ---
keboola_storage_client = None
if KBC_API_URL and KBC_STORAGE_TOKEN:
    try:
        app.logger.info(f"Attempting to initialize Keboola Storage Client with URL: {KBC_API_URL}")
        keboola_storage_client = KeboolaStorageClient(KBC_API_URL, KBC_STORAGE_TOKEN)
        app.logger.info("Successfully initialized Keboola Storage Client.")
    except Exception as e:
        app.logger.error(f"Error initializing Keboola Storage Client: {e}", exc_info=True)
else:
    app.logger.error("CRITICAL ERROR (Keboola Client): KBC_API_URL or KBC_STORAGE_TOKEN not set.")

bigquery_client = None
if GOOGLE_APPLICATION_CREDENTIALS_PATH:
    try:
        app.logger.info(f"Attempting to initialize Google BigQuery Client using credentials from: {GOOGLE_APPLICATION_CREDENTIALS_PATH}")
        bigquery_client = bigquery.Client.from_service_account_json(GOOGLE_APPLICATION_CREDENTIALS_PATH)
        app.logger.info(f"Successfully initialized Google BigQuery Client. Project: {bigquery_client.project}")
    except Exception as e:
        app.logger.error(f"Error initializing Google BigQuery Client: {e}", exc_info=True)
else:
    app.logger.error("CRITICAL ERROR (BigQuery Client): GOOGLE_APPLICATION_CREDENTIALS environment variable not set or file not found.")

gemini_generative_model = None # Will be initialized with tools
if GEMINI_API_KEY:
    try:
        app.logger.info("Configuring Gemini API...")
        genai.configure(api_key=GEMINI_API_KEY)
        app.logger.info("Successfully configured Gemini API.")
        # We will initialize the model with tools later, typically per request or globally if tools are static
    except Exception as e:
        app.logger.error(f"Error configuring Gemini API: {e}", exc_info=True)
else:
    app.logger.error("CRITICAL ERROR (Gemini Client): GEMINI_API_KEY environment variable not set.")

# --- Tool Definitions for Gemini ---

# Tool for executing SQL queries against BigQuery
# This maps to our /api/query_data endpoint's functionality
execute_sql_query_tool = {
    "name": "execute_sql_query",
    "description": "Executes a BigQuery SQL query against the Keboola project's data warehouse and returns the results. Use this to answer questions about specific data, counts, aggregations, etc. The query should be a standard SQL SELECT statement. Ensure table names are fully qualified if necessary (e.g., `project_id.dataset_id.table_name`).",
    "parameters": {
        "type": "object",
        "properties": {
            "sql_query": {
                "type": "string",
                "description": "The BigQuery SQL SELECT query to execute."
            }
        },
        "required": ["sql_query"]
    }
}
# In the future, we can add more tools here for listing buckets, tables, etc.
# For now, let's focus on the query tool.

# --- Internal functions to execute our tools ---
# These functions will be called when Gemini requests a tool_call

def internal_execute_sql_query(sql_query: str):
    """Internal function to wrap the BigQuery query logic."""
    if not bigquery_client:
        app.logger.error("BigQuery client not initialized for internal_execute_sql_query.")
        return {"error": "BigQuery client not initialized."}

    app.logger.info(f"Internal tool call: execute_sql_query with query: {sql_query}")
    try:
        # The project ID and dataset ID (KBC_WORKSPACE_SCHEMA) context should be part of the sql_query
        # or handled by how the bigquery_client is configured if it has a default project/dataset.
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=30) # Wait for results with a timeout
        rows_list = [dict(row) for row in results]
        app.logger.info(f"Internal tool call: query executed, returned {len(rows_list)} rows.")
        # Gemini function calling expects a dict with a key matching the tool name,
        # and the value being the result (often a string or simple JSON structure).
        # Let's return it as a list of dicts which can be stringified if needed.
        # For very large results, you might want to summarize or truncate.
        return rows_list 
    except Exception as e:
        app.logger.error(f"Internal tool call: Error executing BigQuery query: {e}", exc_info=True)
        return {"error": f"Error executing BigQuery query: {str(e)}"}

# --- API Endpoints ---

@app.route('/')
def hello():
    return "Hello from your custom Keboola API Gateway!"

# (list_buckets and list_tables_in_bucket endpoints remain the same as before)
@app.route('/api/list_buckets', methods=['GET'])
def list_keboola_buckets():
    if not keboola_storage_client:
        app.logger.error("/api/list_buckets called but Keboola client not initialized.")
        return jsonify({"error": "Keboola client not initialized. Check server logs."}), 500
    try:
        app.logger.info("Attempting to list buckets via /api/list_buckets...")
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = []
        if buckets_data:
            for b in buckets_data:
                bucket_info.append({"id": b.get("id"), "name": b.get("name"), "stage": b.get("stage"), "uri": b.get("uri")})
        app.logger.info(f"Found {len(bucket_info)} buckets.")
        return jsonify(bucket_info)
    except Exception as e:
        app.logger.error(f"Error listing Keboola buckets: {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500

@app.route('/api/buckets/<path:bucket_id>/tables', methods=['GET'])
def list_tables_in_bucket(bucket_id):
    if not keboola_storage_client:
        app.logger.error(f"/api/buckets/{bucket_id}/tables called but Keboola client not initialized.")
        return jsonify({"error": "Keboola client not initialized. Check server logs."}), 500

    app.logger.info(f"Attempting to list tables for bucket_id: {bucket_id}")
    try:
        tables_data = keboola_storage_client.buckets.list_tables(bucket_id=bucket_id)
        table_info = []
        if tables_data:
            for t in tables_data:
                table_info.append({
                    "id": t.get("id"), "name": t.get("name"), "uri": t.get("uri"),
                    "lastImportDate": t.get("lastImportDate"), "lastChangeDate": t.get("lastChangeDate"),
                    "rowsCount": t.get("rowsCount"), "dataSizeBytes": t.get("dataSizeBytes")
                })
        app.logger.info(f"Found {len(table_info)} tables in bucket {bucket_id}.")
        return jsonify(table_info)
    except Exception as e:
        app.logger.error(f"Error listing tables for bucket {bucket_id}: {e}", exc_info=True)
        return jsonify({"error": f"Could not list tables for bucket {bucket_id}: {str(e)}"}), 500

@app.route('/api/query_data', methods=['POST'])
def query_data_endpoint(): # Renamed to avoid conflict with any internal function
    # This endpoint is now callable directly, AND can be used by our Gemini tool logic.
    request_data = request.get_json()
    if not request_data or 'sql_query' not in request_data:
        return jsonify({"error": "Missing 'sql_query' in JSON payload."}), 400

    sql_query = request_data['sql_query']
    result = internal_execute_sql_query(sql_query) # Use the internal function

    if isinstance(result, dict) and "error" in result:
        return jsonify(result), 500
    return jsonify(result)

# --- NEW CHAT ENDPOINT ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini():
    if not GEMINI_API_KEY: # Check if Gemini API key was configured
        app.logger.error("/api/chat called but GEMINI_API_KEY is not set.")
        return jsonify({"error": "Gemini API key not configured."}), 500

    try:
        user_message_data = request.get_json()
        if not user_message_data or 'message' not in user_message_data:
            return jsonify({"error": "Missing 'message' in JSON payload."}), 400

        user_message = user_message_data['message']
        app.logger.info(f"Received user message for Gemini: {user_message}")

        # Initialize the Gemini model with our defined tools
        # Consider choosing a specific model version, e.g., 'gemini-1.5-flash' or 'gemini-1.5-pro'
        # For function calling, 'gemini-pro' or newer models supporting it are needed.
        # Let's use 'gemini-1.0-pro' as a common one that supports function calling.
        # You might need to check for the latest recommended model.
        model = genai.GenerativeModel(
            'gemini-1.0-pro', # Or 'gemini-1.5-flash-latest', 'gemini-1.5-pro-latest' etc.
            tools=[execute_sql_query_tool], # Pass the tool definition
            # Optional: Add safety settings if needed
            # safety_settings=[
            #     {
            #         "category": HarmCategory.HARM_CATEGORY_HARASSMENT,
            #         "threshold": HarmBlockThreshold.BLOCK_NONE,
            #     },
            #     # ... other categories
            # ]
        )

        # Start a chat session (or manage ongoing sessions if you implement session handling)
        chat_session = model.start_chat(enable_automatic_function_calling=False) # Start with manual control for clarity

        app.logger.info(f"Sending message to Gemini: '{user_message}' with tool: {execute_sql_query_tool['name']}")
        response = chat_session.send_message(user_message)

        # Check if Gemini responded with a function call request
        function_call = None
        try:
            # Accessing function_calls attribute on the last Candidate
            if response.candidates and response.candidates[0].content.parts:
                for part in response.candidates[0].content.parts:
                    if part.function_call:
                        function_call = part.function_call
                        break
        except Exception as e:
            app.logger.warning(f"Could not extract function call from Gemini response (might be direct text): {e}")

        if function_call:
            tool_name = function_call.name
            tool_args = {key: value for key, value in function_call.args.items()} # Convert to dict
            app.logger.info(f"Gemini wants to call tool: {tool_name} with args: {tool_args}")

            api_response_content = None
            if tool_name == "execute_sql_query":
                sql_query_from_llm = tool_args.get("sql_query")
                if sql_query_from_llm:
                    # Call your internal function that executes the query
                    tool_result = internal_execute_sql_query(sql_query_from_llm)

                    # For Gemini, the function response needs to be structured correctly.
                    # It expects a dict with 'name' of the function and 'response' which itself is a dict
                    # containing the actual result data (often as a 'content' field or just the direct JSON).
                    # Let's wrap the result. Max 4096 tokens for function response.
                    api_response_content = {
                        "content": json.dumps(tool_result) # Send the result as a JSON string
                    }
                    if len(json.dumps(tool_result)) > 3000: # Crude check for length
                         api_response_content = {"content": json.dumps({"status": "success", "message": "Result too long, truncated or summarized."})}


                else:
                    api_response_content = {"content": json.dumps({"error": "Missing sql_query argument from LLM."})}
            else:
                api_response_content = {"content": json.dumps({"error": f"Unknown tool requested: {tool_name}"})}

            # Send the tool's result back to Gemini
            app.logger.info(f"Sending tool response back to Gemini for tool {tool_name}: {api_response_content}")

            # Construct the FunctionResponse part
            function_response_part = genai.types.Part(
                function_response=genai.types.FunctionResponse(
                    name=tool_name,
                    response=api_response_content 
                )
            )

            response = chat_session.send_message(function_response_part)
            # The final response from Gemini after processing the tool output
            final_answer = response.text
            app.logger.info(f"Gemini final answer after tool call: {final_answer}")
            return jsonify({"reply": final_answer})

        else:
            # If no function call, just return Gemini's direct text response
            final_answer = response.text
            app.logger.info(f"Gemini direct reply: {final_answer}")
            return jsonify({"reply": final_answer})

    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint: {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500


# --- Main Execution ---
if __name__ == '__main__':
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    app.logger.info(f"KBC_STORAGE_TOKEN from env: {'SET' if KBC_STORAGE_TOKEN else 'NOT SET'}")
    app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS from env: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'NOT SET'}")
    app.logger.info(f"KBC_WORKSPACE_SCHEMA from env (BigQuery Dataset ID): {'SET' if KBC_WORKSPACE_SCHEMA else 'NOT SET'}")
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    if not all([KBC_API_URL, KBC_STORAGE_TOKEN, GOOGLE_APPLICATION_CREDENTIALS_PATH, KBC_WORKSPACE_SCHEMA, GEMINI_API_KEY]):
        app.logger.critical("CRITICAL ERROR: One or more essential environment variables are missing. Server cannot function fully.")

    app.run(host='0.0.0.0', port=8080, debug=True)