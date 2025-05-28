import os
from flask import Flask, jsonify, request
from kbcstorage.client import Client as KeboolaStorageClient
from google.cloud import bigquery
import google.generativeai as genai
from google.generativeai.types import HarmCategory, HarmBlockThreshold
# Attempt to import the specific protobuf types for tool definition
try:
    from google.ai import generativelanguage as glm
    GLM_TYPES_AVAILABLE = True
except ImportError:
    app.logger.error("Could not import 'google.ai.generativelanguage' (glm). Will attempt dictionary-based tools.")
    GLM_TYPES_AVAILABLE = False
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
# (Keboola and BigQuery client initializations remain the same)
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
gemini_model_instance = None
gemini_tools_list = []

if GEMINI_API_KEY:
    try:
        app.logger.info("Configuring Gemini API...")
        genai.configure(api_key=GEMINI_API_KEY)
        app.logger.info("Successfully configured Gemini API.")

        if GLM_TYPES_AVAILABLE:
            app.logger.info("Attempting to define tools using explicit glm.Schema and glm.Type...")
            sql_tool_parameters_schema = glm.Schema(
                type=glm.Type.OBJECT,
                properties={
                    'sql_query': glm.Schema(type=glm.Type.STRING, description="The BigQuery SQL SELECT query to execute. Ensure table names are fully qualified, e.g., `project_id.dataset_id.table_name`.")
                },
                required=['sql_query']
            )
            sql_function_declaration = glm.FunctionDeclaration(
                name='execute_sql_query',
                description="Executes a BigQuery SQL query against the Keboola project's data warehouse (dataset: WORKSPACE_21894820, project: kbc-use4-839-261b) and returns the results. Use this to answer questions about specific data, counts, aggregations, etc.",
                parameters=sql_tool_parameters_schema
            )
            gemini_tools_list = [glm.Tool(function_declarations=[sql_function_declaration])]
            tool_names_for_log = [fd.name for fd in gemini_tools_list[0].function_declarations] if gemini_tools_list else []
        else:
            # Fallback to dictionary definition if glm types are not available (which led to errors before)
            # This part is unlikely to work given previous KeyErrors, but kept as a fallback concept.
            app.logger.warning("glm types not available, falling back to dictionary-based tool definition (might cause errors).")
            execute_sql_query_tool_declaration_dict = {
                "name": "execute_sql_query",
                "description": "Executes a BigQuery SQL query...",
                "parameters": { "type": "object", "properties": { "sql_query": {"type": "string", "description": "..."}}, "required": ["sql_query"]}
            }
            # The GenerativeModel 'tools' arg expects a list of genai.Tool or dicts that can be converted.
            # If we directly pass the dict, it needs to be convertible to FunctionDeclaration by the SDK.
            gemini_tools_list = [execute_sql_query_tool_declaration_dict] # This might need to be wrapped in a Tool dict
            tool_names_for_log = [execute_sql_query_tool_declaration_dict.get("name")]


        gemini_model_instance = genai.GenerativeModel(
            model_name='gemini-1.5-flash-latest', 
            tools=gemini_tools_list, 
            safety_settings=[ 
                {"category": HarmCategory.HARM_CATEGORY_HARASSMENT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_HATE_SPEECH, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_SEXUALLY_EXPLICIT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
                {"category": HarmCategory.HARM_CATEGORY_DANGEROUS_CONTENT, "threshold": HarmBlockThreshold.BLOCK_MEDIUM_AND_ABOVE},
            ]
        )
        app.logger.info(f"Gemini GenerativeModel initialized with model 'gemini-1.5-flash-latest' and tools: {tool_names_for_log}")

    except ImportError as ie:
         app.logger.error(f"An ImportError occurred during Gemini setup: {ie}. This likely means a required Google library (e.g., google.ai.generativelanguage) is not installed or accessible. Please check your 'google-generativeai' installation and its dependencies.", exc_info=True)
         gemini_model_instance = None
    except Exception as e:
        app.logger.error(f"Error configuring Gemini API or initializing model: {e}", exc_info=True)
        gemini_model_instance = None 
else:
    app.logger.error("CRITICAL ERROR (Gemini Client): GEMINI_API_KEY environment variable not set.")

# --- Internal functions to execute our tools (internal_execute_sql_query remains the same) ---
def internal_execute_sql_query(sql_query: str):
    # ... (implementation from before) ...
    if not bigquery_client:
        app.logger.error("BigQuery client not initialized for internal_execute_sql_query.")
        return {"error": "BigQuery client not initialized."}
    app.logger.info(f"Internal tool call: execute_sql_query with query: {sql_query}")
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result(timeout=60) 
        rows_list = [dict(row) for row in results]
        app.logger.info(f"Internal tool call: query executed, returned {len(rows_list)} rows.")
        result_str = json.dumps(rows_list)
        if len(result_str) > 3500: 
            return {"status": "success_truncated", "message": f"Query returned {len(rows_list)} rows. Result too large. Sample: " + result_str[:3500]}
        return rows_list 
    except Exception as e:
        app.logger.error(f"Internal tool call: Error executing BigQuery query: {e}", exc_info=True)
        return {"error": f"Error executing BigQuery query: {str(e)}"}

# --- API Endpoints (/, /api/list_buckets, etc. remain the same) ---
@app.route('/')
def hello(): return "Hello from your custom Keboola API Gateway!"

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
    if isinstance(result, dict) and "error" in result:
        return jsonify(result), 500
    return jsonify(result)

# --- CHAT ENDPOINT (Logic for handling function call response slightly adjusted for robustness) ---
@app.route('/api/chat', methods=['POST'])
def chat_with_gemini():
    if not gemini_model_instance: 
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

        while True:
            function_call = None
            if response.candidates and response.candidates[0].content and response.candidates[0].content.parts:
                part_content = response.candidates[0].content.parts[0]
                if hasattr(part_content, 'function_call') and part_content.function_call:
                    function_call = part_content.function_call

            if function_call:
                tool_name = function_call.name
                tool_args = {key: value for key, value in function_call.args.items()}
                app.logger.info(f"Gemini wants to call tool: {tool_name} with args: {tool_args}")

                tool_execution_result = None
                sql_query_from_llm = None 
                if tool_name == "execute_sql_query":
                    sql_query_from_llm = tool_args.get("sql_query")
                    if sql_query_from_llm:
                        tool_execution_result = internal_execute_sql_query(sql_query_from_llm)
                    else:
                        tool_execution_result = {"error": "Missing sql_query argument from LLM for execute_sql_query."}
                else:
                    tool_execution_result = {"error": f"Unknown or unhandled tool requested by LLM: {tool_name}"}

                app.logger.info(f"Sending tool response back to Gemini for tool {tool_name}")

                # Construct the function response for the SDK.
                # The SDK expects the actual result data to be in a dictionary under the 'response' key
                # when you build a FunctionResponse object or its dictionary equivalent.
                function_response_part_for_sdk = {
                    "name": tool_name,
                    "response": tool_execution_result # tool_execution_result is already a dict (list of dicts for success, or error dict)
                }

                # The send_message method expects an iterable of Parts (or dicts convertible to Parts).
                # A dict representing a FunctionResponse is one such Part.
                response = chat_session.send_message([ # Send as a list of parts
                    {"function_response": function_response_part_for_sdk}
                ]) 
            else: # No more function calls, this should be the final text answer
                final_answer = ""
                try:
                    final_answer = response.text
                except ValueError: # Handles "Could not convert part.function_call to text"
                    if response.candidates and response.candidates[0].content and response.candidates[0].content.parts:
                        final_answer = "".join(p.text for p in response.candidates[0].content.parts if hasattr(p, 'text'))
                    if not final_answer:
                         app.logger.error(f"Gemini response did not contain a direct text answer or a recognized function call. Parts: {response.candidates[0].content.parts if response.candidates else 'No candidates'}")
                         return jsonify({"error": "Received an unexpected response structure from LLM after tool execution."}), 500

                app.logger.info(f"Gemini final answer: {final_answer}")
                return jsonify({"reply": final_answer})

    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint: {e}", exc_info=True)
        return jsonify({"error": str(e)}), 500

# --- Main Execution ---
if __name__ == '__main__':
    # ... (env var checks remain the same) ...
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT SET'}")
    app.logger.info(f"KBC_STORAGE_TOKEN from env: {'SET' if KBC_STORAGE_TOKEN else 'NOT SET'}")
    app.logger.info(f"GOOGLE_APPLICATION_CREDENTIALS from env: {'SET' if GOOGLE_APPLICATION_CREDENTIALS_PATH else 'NOT SET'}")
    app.logger.info(f"KBC_WORKSPACE_SCHEMA from env (BigQuery Dataset ID): {'SET' if KBC_WORKSPACE_SCHEMA else 'NOT SET'}")
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT SET'}")

    if not all([KBC_API_URL, KBC_STORAGE_TOKEN, GOOGLE_APPLICATION_CREDENTIALS_PATH, KBC_WORKSPACE_SCHEMA, GEMINI_API_KEY]):
        app.logger.critical("CRITICAL ERROR: One or more essential environment variables are missing. Server cannot function fully.")

    app.run(host='0.0.0.0', port=8080, debug=True)