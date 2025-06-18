#!/usr/bin/env python3
"""
Flask application entry point for Gunicorn deployment
"""
import os
import sys
from flask import Flask, request, jsonify
from flask_cors import CORS
from main_2 import initialize_services

# Initialize Flask app
app = Flask(__name__)
CORS(app)

# Initialize ADK services
try:
    runner, session_service, app_name, user_id = initialize_services()
    print("✓ ADK services initialized successfully")
except Exception as e:
    print(f"ERROR: Failed to initialize ADK services: {e}")
    sys.exit(1)

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint for deployment verification"""
    return jsonify({"status": "healthy", "service": "kultivachat-backend"})

@app.route('/api/chat', methods=['POST'])
def chat():
    """Main chat endpoint for AI conversations"""
    try:
        data = request.get_json()
        if not data or 'message' not in data:
            return jsonify({"error": "Message is required"}), 400
        
        user_message = data['message']
        
        # Process the message through the ADK runner
        # Generate a unique session ID for each request
        import uuid
        session_id = str(uuid.uuid4())
        
        response = runner.run(
            session_id=session_id,
            new_message=user_message
        )
        
        return jsonify({
            "response": response.text if hasattr(response, 'text') else str(response),
            "status": "success"
        })
        
    except Exception as e:
        print(f"Chat error: {e}")
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