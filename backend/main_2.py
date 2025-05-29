import os
import logging
from flask import Flask, request, jsonify
from flask_cors import CORS
import json
from decimal import Decimal

# Import google.genai and google.genai.types
try:
    import google.genai as google_genai_for_client
    import google.genai.types as google_genai_types
    print("Successfully imported 'google.genai' and 'google.genai.types'")
except ImportError as e:
    print(f"Error importing google.genai modules: {e}")
    google_genai_for_client = None
    google_genai_types = None

# Import other necessary modules
from google.cloud import bigquery
from keboola.storage import Client as KeboolaStorageClient
from datetime import datetime
import pytz

app = Flask(__name__)
CORS(app)

# Set up logging
logging.basicConfig(level=logging.INFO)

# Initialize clients
KBC_API_URL = os.getenv('KBC_API_URL')
KBC_API_TOKEN = os.getenv('KBC_API_TOKEN')
GEMINI_API_KEY = os.getenv('GEMINI_API_KEY')

# Initialize Keboola Storage Client
if KBC_API_URL and KBC_API_TOKEN:
    try:
        app.logger.info(f"Attempting to initialize Keboola Storage Client with URL: {KBC_API_URL}")
        keboola_storage_client = KeboolaStorageClient(url=KBC_API_URL, token=KBC_API_TOKEN)
        app.logger.info("Successfully initialized Keboola Storage Client.")
    except Exception as e:
        app.logger.error(f"Error initializing Keboola Storage Client: {e}")
        keboola_storage_client = None
else:
    app.logger.warning("KBC_API_URL or KBC_API_TOKEN not set. Keboola Storage Client not initialized.")
    keboola_storage_client = None

# Initialize Google BigQuery Client
try:
    credentials_path = "/home/runner/workspace/backend/credentials-839-21894808.json"
    app.logger.info(f"Attempting to initialize Google BigQuery Client using credentials from: {credentials_path}")
    bigquery_client = bigquery.Client.from_service_account_json(credentials_path)
    project_id = bigquery_client.project
    app.logger.info(f"Successfully initialized Google BigQuery Client. Project: {project_id}")
except Exception as e:
    app.logger.error(f"Error initializing Google BigQuery Client: {e}")
    bigquery_client = None
    project_id = None

# Define tool functions
def internal_execute_sql_query(sql_query: str) -> dict:
    """Executes a BigQuery SQL query and returns the results."""
    try:
        query_job = bigquery_client.query(sql_query)
        results = query_job.result()
        
        rows = []
        for row in results:
            row_dict = {}
            for key, value in row.items():
                if isinstance(value, Decimal):
                    row_dict[key] = float(value)
                else:
                    row_dict[key] = value
            rows.append(row_dict)
        
        return {
            "status": "success",
            "data": rows,
            "display_type": "table",
            "display_title": "Query Results"
        }
    except Exception as e:
        return {
            "status": "error",
            "error_message": str(e)
        }

def get_current_time() -> dict:
    """Returns the current date, time, and timezone."""
    try:
        utc_now = datetime.now(pytz.UTC)
        est = pytz.timezone('America/New_York')
        est_time = utc_now.astimezone(est)
        
        return {
            "status": "success",
            "current_time": est_time.strftime("%Y-%m-%d %H:%M:%S %Z")
        }
    except Exception as e:
        return {
            "status": "error",
            "error_message": str(e)
        }

# Initialize Gemini client
gemini_sdk_client = None
gemini_generation_config_with_tools = None

if GEMINI_API_KEY and google_genai_for_client:
    try:
        app.logger.info("Initializing google.genai.Client with API key...")
        gemini_sdk_client = google_genai_for_client.Client(api_key=GEMINI_API_KEY)
        app.logger.info("Successfully initialized google.genai.Client.")

        # Create system instruction
        system_instruction = """You are an expert Keboola Data Analyst Assistant. Your primary goal is to help users understand and retrieve insights from their data stored within a Keboola project. This project utilizes Keboola Storage and a Google BigQuery data warehouse (project ID: kbc-use4-839-261b, dataset/workspace schema: WORKSPACE_21894820) for querying data.

When users ask about:
- Tables, data, or datasets: Use the internal_execute_sql_query tool to query the database
- "Show me tables" or "what tables do I have": Query INFORMATION_SCHEMA.TABLES to list tables
- Specific data like "kapwa gardens" or "Undiscovered" or "Balay Kreative" or "Kulivate Labs": Search for it in the available tables
- Data analysis requests: Write and execute appropriate SQL queries

The database details:
- Project: kbc-use4-839-261b
- Dataset: WORKSPACE_21894820
- Always use fully qualified table names: kbc-use4-839-261b.WORKSPACE_21894820.TABLE_NAME

CRITICAL INSTRUCTIONS:
1. ALWAYS use fully qualified table names in SQL queries: kbc-use4-839-261b.WORKSPACE_21894820.ACTUAL_TABLE_NAME
2. For table discovery, first query INFORMATION_SCHEMA.TABLES to see what tables are available
3. Be proactive: If a user asks about data, immediately execute SQL queries to provide actual results
4. For data exploration: If users mention specific terms like "kapwa gardens", search across available tables to find relevant data
5. Always format results clearly and provide insights based on the actual data returned

Remember: You have direct access to query the BigQuery data warehouse. Use it actively to provide real, current data insights!"""

        gemini_tool_functions_list = [internal_execute_sql_query, get_current_time]
        
        app.logger.info(f"Defining tools for Gemini: {[f.__name__ for f in gemini_tool_functions_list]}")
        # Create GenerateContentConfig with system instruction
        gemini_generation_config_with_tools = google_genai_types.GenerateContentConfig(
            tools=gemini_tool_functions_list,
            system_instruction=system_instruction
        )
        app.logger.info("Gemini GenerateContentConfig with tools and system instruction created successfully.")

    except Exception as e:
        app.logger.error(f"Error initializing Gemini client or GenerateContentConfig: {e}", exc_info=True)
        gemini_sdk_client = None
        gemini_generation_config_with_tools = None
else:
    app.logger.warning("GEMINI_API_KEY not set or google.genai not available. Gemini client not initialized.")

@app.route('/api/hello', methods=['GET'])
def hello():
    return "Hello from your custom Keboola API Gateway (using genai.Client)!"

@app.route('/api/chat', methods=['POST'])
def chat_with_gemini_client_style():
    try:
        data = request.get_json()
        user_message = data.get('message', '')
        
        if not user_message:
            return jsonify({"error": "No message provided"}), 400
        
        if not gemini_sdk_client or not gemini_generation_config_with_tools:
            return jsonify({"error": "Gemini client not initialized"}), 500
        
        app.logger.info(f"Received user message for Gemini (genai.Client): {user_message}")
        
        # Create chat session with system instruction already configured
        chat_session = gemini_sdk_client.chats.create(
            model='gemini-2.0-flash',
            config=gemini_generation_config_with_tools,
        )
        app.logger.info(f"Created Gemini chat session. Sending message: '{user_message}'")

        # Send the user message directly since system instruction is set in config
        response = chat_session.send_message(user_message)
        
        # Process the response and return structured data
        response_text = response.text if hasattr(response, 'text') else str(response)
        
        return jsonify({
            "response": response_text,
            "displays": []  # Add any structured displays here if needed
        })
        
    except Exception as e:
        app.logger.error(f"Error in /api/chat endpoint (genai.Client style): {e}")
        return jsonify({"error": str(e)}), 500

# Additional utility endpoints
@app.route('/api/env-check', methods=['GET'])
def env_check():
    return jsonify({
        "KBC_API_URL": "SET" if KBC_API_URL else "NOT_SET",
        "KBC_API_TOKEN": "SET" if KBC_API_TOKEN else "NOT_SET",
        "GEMINI_API_KEY": "SET" if GEMINI_API_KEY else "NOT_SET",
        "keboola_storage_client": "INITIALIZED" if keboola_storage_client else "NOT_INITIALIZED",
        "bigquery_client": "INITIALIZED" if bigquery_client else "NOT_INITIALIZED",
        "gemini_sdk_client": "INITIALIZED" if gemini_sdk_client else "NOT_INITIALIZED"
    })

if __name__ == '__main__':
    app.logger.info(f"KBC_API_URL from env: {'SET' if KBC_API_URL else 'NOT_SET'}")
    app.logger.info(f"GEMINI_API_KEY from env: {'SET' if GEMINI_API_KEY else 'NOT_SET'}")
    app.run(debug=False, host='0.0.0.0', port=8081)