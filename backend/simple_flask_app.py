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

@app.route('/', methods=['GET'])
def root():
    """Root endpoint"""
    return jsonify({
        "message": "Kultivate AI Assistant Backend API",
        "status": "running",
        "endpoints": ["/health", "/ready", "/api/chat", "/api/conversations"]
    })

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    print(f"Starting simple Flask server on port {port}")
    app.run(host="0.0.0.0", port=port, debug=False)