# backend/run_flask.py
import os
import json
from flask import Flask, request, jsonify
from flask_cors import CORS
import logging

# Correctly import the initializer function from the decoupled main_2 module
from backend.main_2 import initialize_services

# --- Initialization ---
app = Flask(__name__)
CORS(app)
logging.basicConfig(level=logging.INFO)

# Initialize the services ONCE when the app starts
try:
    runner, session_service, APP_NAME, USER_ID = initialize_services()
    logging.info("Successfully initialized all services")
    services_ready = True
except Exception as e:
    app.logger.critical(f"Failed to initialize services: {e}")
    runner = None
    session_service = None
    APP_NAME = "kultivachat_app"  # fallback values
    USER_ID = "user_default"
    services_ready = False

# --- API Route ---
@app.route('/api/chat', methods=['POST'])
async def chat_handler():
    if not services_ready:
        return jsonify({"error": "Services not initialized. Check server logs."}), 500
    
    try:
        data = request.json
        if not data or 'message' not in data:
            return jsonify({"error": "Message not provided"}), 400

        user_message = data['message']
        session_id = data.get('sessionId', 'default_session')
        
        logging.info(f"Received message for session: {session_id}")

        # Ensure a session exists
        try:
            await session_service.get_session(app_name=APP_NAME, user_id=USER_ID, session_id=session_id)
        except Exception:
            logging.info(f"Creating new session: {session_id}")
            await session_service.create_session(app_name=APP_NAME, user_id=USER_ID, session_id=session_id)

        # --- Agent Execution ---
        from google.genai import types as genai_types
        
        content = genai_types.Content(role='user', parts=[genai_types.Part(text=user_message)])
        final_response = "Sorry, I encountered an error during processing."
        
        events = runner.run(user_id=USER_ID, session_id=session_id, new_message=content)
        
        for event in events:
            if event.is_final_response() and event.content and event.content.parts:
                part = event.content.parts[0]
                if hasattr(part, 'text') and part.text:
                    final_response = part.text
                    break
        
        # --- Structured Response ---
        response_text = str(final_response)
        
        # Check if response contains table indicators
        if ("headers" in response_text and "rows" in response_text) or "Order ID" in response_text:
            try:
                table_data = json.loads(response_text)
                return jsonify({"reply": {"type": "table", "data": table_data}})
            except json.JSONDecodeError:
                return jsonify({"reply": {"type": "text", "data": response_text}})
        else:
            return jsonify({"reply": {"type": "text", "data": response_text}})

    except Exception as e:
        app.logger.error(f"A critical error occurred in chat_handler: {e}", exc_info=True)
        return jsonify({"error": "An internal server error occurred."}), 500

# Health check endpoint
@app.route('/api/health', methods=['GET'])
def health_check():
    return jsonify({"status": "healthy", "services_initialized": runner is not None})

# This block is for LOCAL DEVELOPMENT ONLY.
# Gunicorn will be used in production and will NOT run this.
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080, debug=True)