import os
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import google.generativeai as genai
from google.generativeai.types import ( # <-- IMPORT THESE
    Tool, 
    FunctionDeclaration, 
    Schema, 
    Type,
    FunctionResponse, # For sending tool result back
    Part            # For sending tool result back
)
from google.generativeai.types import HarmCategory, HarmBlockThreshold
import logging
import json

# --- Initialize Flask App ---
app = Flask(__name__)

# --- Configure logging ---
logging.basicConfig(level=logging.INFO)
app.logger.setLevel(logging.INFO)

# --- Load Configuration from Environment Variables (Replit Secrets) ---
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')
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

# --- Define Tools & Initialize Gemini Client ---
gemini_model_instance = None # Will hold the initialized GenerativeModel
gemini_tools_list = []       # Will hold the Tool objects

if GEMINI_API_KEY:
    try:
        app.logger.info("Configuring Gemini API...")
        genai.configure(api_key=GEMINI_API_KEY)
        app.logger.info("Successfully configured Gemini API.")

        # Define the parameters schema for the execute_sql_query tool using SDK types
        sql_tool_parameters_schema = Schema(
            type=Type.OBJECT,
            properties={
                'sql_query': Schema(type=Type.STRING, description="The BigQuery SQL SELECT query to execute. Ensure table names are fully qualified, e.g., `project_id.dataset_id.table_name`.")
            },
            required=['sql_query']
        )

        # Define the function declaration for execute_sql_query
        sql_function_declaration = FunctionDeclaration(
            name='execute_sql_query',
            description="Executes a BigQuery SQL query against the Keboola project's data warehouse and returns the results. Use this to answer questions about specific data, counts, aggregations, etc.",
            parameters=sql_tool_parameters_schema
        )

        # Add other tools here in the same way if needed
        # For example, a placeholder for list_buckets (implementation would be similar to execute_sql_query)
        # list_buckets_declaration = FunctionDeclaration(name='list_keboola_buckets', description='Lists all buckets in the Keboola project.', parameters=Schema(type=Type.OBJECT, properties={}, required=[]))
        # list_tables_declaration = FunctionDeclaration(name='list_tables_in_bucket', description='Lists tables in a specific Keboola bucket.', parameters=Schema(type=Type.OBJECT, properties={'bucket_id': Schema(type=Type.STRING)}, required=['bucket_id']))


        # Create the list of Tool objects
        # For now, just the SQL tool. You can add more FunctionDeclarations to the list for a single Tool,
        # or create multiple Tool objects if they are conceptually separate sets of functions.
        gemini_tools_list = [
            Tool(function_declarations=[
                sql_function_declaration,
                # list_buckets_declaration, # Uncomment and implement if you want this tool
                # list_tables_declaration # Uncomment and implement if you want this tool
                ])
        ]

        # Initialize the GenerativeModel with the tools
        gemini_model_instance = genai.GenerativeModel(
            model_name='gemini-2.0-flash', # Or 'gemini-1.5-flash-latest', 'gemini-1.5-pro-latest'
            tools=gemini_tools_list,
            safety_settings=[ # Basic safety settings
                {"category": HarmCategory.HARM_CATEGORY_HARASSMENT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_HATE_SPEECH, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_SEXUALLY_EXPLICIT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_DANGEROUS_CONTENT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
            ]
        )
        app.logger.info(f"Gemini GenerativeModel initialized with tools: {[fd.name for fd in gemini_tools_list[0].function_declarations] if gemini_tools_list else 'No tools'}")
    except Exception as e:
        app.logger.error(f"Error configuring Gemini API or initializing model: {e}", exc_info=True)
        gemini_model_instance = None # Ensure it's None on error
else:
    app.logger.error("CRITICAL ERROR (Gemini Client): GEMINI_API_KEY environment variable not set.")


# --- Internal functions to execute our tools ---
def internal_execute_sql_query(sql_query: str):
    if not bigquery_client:
        app.logger.error("BigQuery client not initialized for internal_execute_sql_query.")
        return {"error": "BigQuery client not initialized."}

    app.logger.info(f"Internal tool call: execute_sql_query with query: {sql_query}")
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=30)
        rows_list = [dict(row) for row in results]
        app.logger.info(f"Internal tool call: query executed, returned {len(rows_list)} rows.")
        # For very large results, you might want to summarize or truncate.
        # Convert to string for Gemini if it's a large payload to avoid issues.
        result_str = json.dumps(rows_list)
        if len(result_str) > 3500: # Example limit, adjust as needed based on token limits
            return {"status": "success_truncated", "message": f"Query returned {len(rows_list)} rows. Result is too large to display fully. Here's a sample or summary: " + result_str[:3500]}
        return rows_list
    except Exception as e:
        app.logger.error(f"Internal tool call: Error executing BigQuery query: {e}", exc_info=True)
        return {"error": f"Error executing BigQuery query: {str(e)}"}

# --- API Endpoints ---
@app.route('/')
def hello():
    return "Hello from your custom Keboola API Gateway!"

@app.route('/api/list_buckets', methods=['GET'])
def list_keboola_buckets_endpoint(): # Renamed to avoid conflict
    # ... (implementation from before, using keboola_storage_client) ...
    if not keboola_storage_client:
        return jsonify({"error": "Keboola client not initialized."}), 500
    try:
        buckets_data = keboola_storage_client.buckets.list()
        bucket_info = [{"id": b.get("id"), "name": b.get("name"), "stage": b.get("stage"), "uri": b.get("uri")} for b in buckets_data or []]
        return jsonify(bucket_info)
    except Exception as e: return jsonify({"error": str(e)}), 500

@app.route('/api/buckets/<path:bucket_id>/tables', methods=['GET'])
def list_tables_in_bucket_endpoint(bucket_id): # Renamed
    # ... (implementation from before, using keboola_storage_client) ...
    if not keboola_storage_client:
        return jsonify({"error": "Keboola client not initialized."}), 500
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
    if isinstance(result, dict) and "error" in result:
        return jsonify(result), 500
    return jsonify(result)

# --- CHAT ENDPOINT ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini():
    if not gemini_model_instance: # Check if Gemini model was initialized
        app.logger.error("/api/chat called but Gemini model is not initialized.")
        return jsonify({"error": "Gemini model not initialized. Check server logs."}), 500

    try:
        user_message_data = request.get_json()
        if not user_message_data or 'message' not in user_message_data:
            return jsonify({"error": "Missing 'message' in JSON payload."}), 400

        user_message = user_message_data['message']
        app.logger.info(f"Received user message for Gemini: {user_message}")

        chat_session = gemini_model_instance.start_chat(enable_automatic_function_calling=False) 

        app.logger.info(f"Sending message to Gemini: '{user_message}'")
        response = chat_session.send_message(user_message)

        function_call = None
        # Check for function call in the response
        if response.candidates and response.candidates[0].content.parts:
            for part in response.candidates[0].content.parts:
                if part.function_call:
                    function_call = part.function_call
                    break

        if function_call:
            tool_name = function_call.name
            tool_args = {key: value for key, value in function_call.args.items()}
            app.logger.info(f"Gemini wants to call tool: {tool_name} with args: {tool_args}")

            tool_execution_result = None
            if tool_name == "execute_sql_query":
                sql_query_from_llm = tool_args.get("sql_query")
                if sql_query_from_llm:
                    tool_execution_result = internal_execute_sql_query(sql_query_from_llm)
                else:
                    tool_execution_result = {"error": "Missing sql_query argument from LLM for execute_sql_query."}
            # Add other tool handlers here if you define more tools
            # elif tool_name == "list_keboola_buckets":
            #     tool_execution_result = internal_list_keboola_buckets() # You'd need to create this function
            else:
                tool_execution_result = {"error": f"Unknown or unhandled tool requested: {tool_name}"}

            app.logger.info(f"Sending tool response back to Gemini for tool {tool_name}")

            # Construct the FunctionResponse part using genai.types
            # The 'response' field in FunctionResponse expects a dict.
            # The 'content' within that dict should be the actual result for the LLM.
            function_response_payload = {
                 "content": tool_execution_result # Pass the direct result (list of dicts or error dict)
            }

            gemini_response_part = Part(
                function_response=FunctionResponse(
                    name=tool_name,
                    response=function_response_payload
                )
            )

            response = chat_session.send_message(gemini_response_part)
            final_answer = response.text
            app.logger.info(f"Gemini final answer after tool call: {final_answer}")
            return jsonify({"reply": final_answer})

        else:
            final_answer = response.text
            app.logger.info(f"Gemini direct reply: {final_answer}")
            return jsonify({"reply": final_answer})

    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint: {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500


# --- Main Execution ---
if __name__ == '__main__':
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    # ... (other env var checks) ...
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    if not all([KBC_API_URL, KBC_STORAGE_TOKEN, GOOGLE_APPLICATION_CREDENTIALS_PATH, KBC_WORKSPACE_SCHEMA, GEMINI_API_KEY]):
        app.logger.critical("CRITICAL ERROR: One or more essential environment variables are missing. Server cannot function fully.")

    app.run(host='0.0.0.0', port=8080, debug=True)