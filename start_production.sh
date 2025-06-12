#!/bin/bash

# Production startup script for both frontend and backend
echo "Starting Kultivate AI production servers..."

# Kill any existing processes on ports 5000 and 8081
lsof -ti:5000 | xargs kill -9 2>/dev/null || true
lsof -ti:8081 | xargs kill -9 2>/dev/null || true

# Start Python Flask backend in background
echo "Starting Python Flask backend on port 8081..."
cd backend
export PYTHONUNBUFFERED=1
python main_2.py &
FLASK_PID=$!
echo "Flask backend started with PID: $FLASK_PID"

# Wait a moment for Flask to start
sleep 3

# Start Node.js frontend
echo "Starting Node.js frontend on port 5000..."
cd ..
NODE_ENV=production node dist/index.js &
NODE_PID=$!
echo "Node.js frontend started with PID: $NODE_PID"

# Function to cleanup on exit
cleanup() {
    echo "Shutting down servers..."
    kill $FLASK_PID 2>/dev/null || true
    kill $NODE_PID 2>/dev/null || true
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Wait for both processes
wait $NODE_PID
wait $FLASK_PID