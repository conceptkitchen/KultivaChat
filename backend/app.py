#!/usr/bin/env python3
"""
Flask application entry point for Gunicorn deployment
"""
import os
import sys
import uuid
from flask import Flask, request, jsonify
from flask_cors import CORS

# Initialize Flask app
app = Flask(__name__)
CORS(app)

# Global variables for ADK services
runner = None
session_service = None
app_name = None
user_id = None

def initialize_adk_services():
    """Initialize ADK services with proper error handling"""
    global runner, session_service, app_name, user_id
    
    try:
        from main_2 import initialize_services
        runner, session_service, app_name, user_id = initialize_services()
        print("✓ ADK services initialized successfully")
        return True
    except Exception as e:
        print(f"ERROR: Failed to initialize ADK services: {e}")
        return False

# Try to initialize ADK services, but don't fail if they're not available
adk_available = initialize_adk_services()

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint for deployment verification"""
    return jsonify({"status": "healthy", "service": "kultivachat-backend"})

@app.route('/api/chat', methods=['POST'])
def chat():
    """Main chat endpoint for AI conversations"""
    global runner, adk_available
    
    if not adk_available or runner is None:
        return jsonify({"error": "ADK services not available"}), 503
    
    try:
        data = request.get_json()
        if not data or 'message' not in data:
            return jsonify({"error": "Message is required"}), 400
        
        user_message = data['message']
        session_id = str(uuid.uuid4())
        
        # Use correct ADK Runner API
        response_generator = runner.run(
            user_id=user_id,
            session_id=session_id,
            new_message=user_message
        )
        
        # Collect all events from the generator
        response_text = ""
        for event in response_generator:
            if hasattr(event, 'text') and event.text:
                response_text += event.text
            elif hasattr(event, 'content') and event.content:
                response_text += str(event.content)
        
        if not response_text:
            response_text = "I received your message but couldn't generate a response."
        
        return jsonify({
            "response": response_text,
            "status": "success"
        })
        
    except Exception as e:
        print(f"Chat error: {e}")
        import traceback
        traceback.print_exc()
        return jsonify({"error": str(e)}), 500

@app.route('/api/conversations', methods=['GET'])
def get_conversations():
    """Get user conversations"""
    # TODO: Implement conversation history if needed
    return jsonify({"conversations": []})

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8080))
    print(f"Starting Flask server on port {port}")
    app.run(host="0.0.0.0", port=port, debug=False)