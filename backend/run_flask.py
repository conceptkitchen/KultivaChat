# backend/run_flask.py
from flask import Flask, request, jsonify
from flask_cors import CORS
from backend.main_2 import main_agent, runner, session_service, APP_NAME, USER_ID
from google.genai import types
import logging

# Configure basic logging
logging.basicConfig(level=logging.INFO)

# Initialize Flask app
app = Flask(__name__)
# Be specific with CORS in production for better security
CORS(app, resources={r"/api/*": {"origins": "*"}}) # Or specify your frontend domain

@app.route('/api/chat', methods=['POST'])
async def chat_handler():
    try:
        data = request.json
        if not data or 'message' not in data:
            logging.warning("Received request with no message.")
            return jsonify({"error": "Message not provided"}), 400

        user_message = data['message']
        session_id = data.get('sessionId', 'default_session')
        logging.info(f"Received message for session: {session_id}")

        # Ensure session exists
        try:
            await session_service.get_session(app_name=APP_NAME, user_id=USER_ID, session_id=session_id)
        except Exception:
            logging.info(f"Creating new session: {session_id}")
            await session_service.create_session(app_name=APP_NAME, user_id=USER_ID, session_id=session_id)

        content = types.Content(role='user', parts=[types.Part(text=user_message)])

        final_response = "Sorry, I couldn't process your request."
        events = runner.run(user_id=USER_ID, session_id=session_id, new_message=content)
        for event in events:
            if event.is_final_response() and event.content and event.content.parts:
                # Handle both structured (tool) and text responses
                part = event.content.parts[0]
                if part.function_call:
                    # If the final response is a tool call, we should ideally format it
                    # For now, we'll send a structured response
                    final_response = {"type": "tool_call", "data": {"name": part.function_call.name, "args": dict(part.function_call.args)}}
                elif part.text:
                    final_response = {"type": "text", "data": part.text}
                break
        
        return jsonify({"reply": final_response})

    except Exception as e:
        logging.error(f"An unhandled error occurred in chat_handler: {e}", exc_info=True)
        return jsonify({"error": "A critical server error occurred."}), 500

# This block is for LOCAL DEVELOPMENT ONLY.
# Gunicorn will be used in production and will NOT run this.
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)