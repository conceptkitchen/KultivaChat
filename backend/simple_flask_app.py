#!/usr/bin/env python3
"""
Simple Flask application for deployment - optimized for quick startup
"""
import os
import sys
import time
from flask import Flask, request, jsonify
from flask_cors import CORS

# Initialize Flask app
app = Flask(__name__)
CORS(app)

# Store startup time for health checks
startup_time = time.time()

@app.route('/health', methods=['GET'])
def health_check():
    """Health check endpoint for deployment verification"""
    uptime = time.time() - startup_time
    return jsonify({
        "status": "healthy", 
        "service": "kultivachat-backend",
        "uptime_seconds": round(uptime, 2),
        "timestamp": time.strftime("%Y-%m-%d %H:%M:%S")
    })

@app.route('/ready', methods=['GET'])
def ready_check():
    """Readiness check endpoint"""
    return jsonify({"status": "ready"})

@app.route('/api/chat', methods=['POST'])
def chat():
    """Simple chat endpoint for testing deployment"""
    try:
        data = request.get_json()
        if not data or 'message' not in data:
            return jsonify({"error": "Message is required"}), 400
        
        user_message = data['message']
        
        # Simple echo response for now - ADK integration can be added later
        response_text = f"Echo: {user_message} (Backend is working!)"
        
        return jsonify({
            "response": response_text,
            "status": "success",
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S")
        })
        
    except Exception as e:
        print(f"Chat error: {e}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/conversations', methods=['GET'])
def get_conversations():
    """Get user conversations"""
    return jsonify({"conversations": []})

@app.route('/api/conversations', methods=['POST'])
def create_conversation():
    """Create a new conversation"""
    try:
        data = request.get_json()
        conversation_id = f"conv_{int(time.time())}"
        
        # Simple conversation structure
        conversation = {
            "id": conversation_id,
            "title": data.get("title", "New Conversation"),
            "created_at": time.strftime("%Y-%m-%d %H:%M:%S"),
            "messages": []
        }
        
        return jsonify(conversation), 201
        
    except Exception as e:
        print(f"Create conversation error: {e}")
        return jsonify({"error": str(e)}), 500

@app.route('/api/conversations/<conversation_id>', methods=['GET'])
def get_conversation(conversation_id):
    """Get a specific conversation"""
    # Simple mock conversation
    conversation = {
        "id": conversation_id,
        "title": "Sample Conversation",
        "created_at": time.strftime("%Y-%m-%d %H:%M:%S"),
        "messages": []
    }
    return jsonify(conversation)

@app.route('/api/conversations/<conversation_id>', methods=['DELETE'])
def delete_conversation(conversation_id):
    """Delete a conversation"""
    return jsonify({"message": "Conversation deleted successfully"})

@app.route('/api/conversations/<conversation_id>/messages', methods=['POST'])
def add_message_to_conversation(conversation_id):
    """Add a message to a conversation"""
    try:
        data = request.get_json()
        if not data or 'content' not in data:
            return jsonify({"error": "Message content is required"}), 400
        
        message = {
            "id": f"msg_{int(time.time())}",
            "role": data.get("role", "user"),
            "content": data["content"],
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S")
        }
        
        return jsonify(message), 201
        
    except Exception as e:
        print(f"Add message error: {e}")
        return jsonify({"error": str(e)}), 500

# Authentication endpoints
@app.route('/api/auth/me', methods=['GET'])
def get_current_user():
    """Get current user info"""
    return jsonify({
        "id": 1,
        "username": "demo_user",
        "email": "demo@kultivate.ai",
        "authenticated": True
    })

@app.route('/api/auth/logout', methods=['POST'])
def logout():
    """Logout user"""
    return jsonify({"message": "Logged out successfully"})

@app.route('/api/auth/login', methods=['POST'])
def login():
    """Login user"""
    try:
        data = request.get_json()
        return jsonify({
            "id": 1,
            "username": data.get("username", "demo_user"),
            "email": "demo@kultivate.ai",
            "authenticated": True,
            "message": "Login successful"
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 400

@app.route('/', methods=['GET'])
def root():
    """Root endpoint"""
    return jsonify({
        "message": "Kultivate AI Assistant Backend API",
        "status": "running",
        "endpoints": [
            "/health", 
            "/ready", 
            "/api/chat", 
            "/api/conversations",
            "/api/conversations/<id>",
            "/api/conversations/<id>/messages"
        ]
    })

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8081))
    print(f"Starting simple Flask server on port {port}")
    app.run(host="0.0.0.0", port=port, debug=False)