#!/bin/bash

# Sequential startup script - frontend first, then backend
echo "Starting Kultivate AI production servers sequentially..."

# Kill any existing processes on ports 5000 and 8081
lsof -ti:5000 | xargs kill -9 2>/dev/null || true
lsof -ti:8081 | xargs kill -9 2>/dev/null || true

# Start Node.js frontend first
echo "Starting Node.js frontend on port 5000 (maps to external port 80)..."
NODE_ENV=production node dist/index.js &
NODE_PID=$!
echo "Node.js frontend started with PID: $NODE_PID"

# Wait for frontend to be ready by checking if port 5000 is listening
echo "Waiting for frontend to be ready..."
while ! nc -z localhost 5000; do
    sleep 1
    echo "Waiting for frontend on port 5000..."
done
echo "Frontend is ready!"

# Now start Python Flask backend
echo "Starting Python Flask backend on port 8081..."
cd backend
export PYTHONUNBUFFERED=1
python main_2.py &
FLASK_PID=$!
echo "Flask backend started with PID: $FLASK_PID"

# Wait for backend to be ready
echo "Waiting for backend to be ready..."
while ! nc -z localhost 8081; do
    sleep 1
    echo "Waiting for backend on port 8081..."
done
echo "Backend is ready! Full stack application is now running."
echo "Frontend: http://localhost:5000 (external port 80)"
echo "Backend API: http://localhost:8081"

# Function to cleanup on exit
cleanup() {
    echo "Shutting down servers..."
    kill $FLASK_PID 2>/dev/null || true
    kill $NODE_PID 2>/dev/null || true
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Keep script running and wait for processes
cd ..
wait $NODE_PID
wait $FLASK_PID