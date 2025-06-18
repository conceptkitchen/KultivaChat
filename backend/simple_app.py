#!/usr/bin/env python3
"""
Simple Flask application for testing deployment
"""
import os
from flask import Flask, request, jsonify
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint for deployment verification"""
    return jsonify({
        "status": "healthy", 
        "service": "kultivachat-backend",
        "port": os.environ.get("PORT", "8080")
    })

@app.route('/api/chat', methods=['POST'])
def chat():
    """Simple chat endpoint for testing"""
    try:
        data = request.get_json()
        if not data or 'message' not in data:
            return jsonify({"error": "Message is required"}), 400
        
        user_message = data['message']
        
        # Simple echo response for testing
        return jsonify({
            "response": f"Echo: {user_message}",
            "status": "success",
            "service": "kultivachat-backend"
        })
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/api/conversations', methods=['GET'])
def get_conversations():
    """Get user conversations"""
    return jsonify({"conversations": []})

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8080))
    print(f"Starting Flask server on port {port}")
    app.run(host="0.0.0.0", port=port, debug=False)