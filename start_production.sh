#!/bin/bash

# Sequential startup script - frontend first, then backend
echo "Starting Kultivate AI production servers sequentially..."

# Kill existing Node.js and Python processes
pkill -f "node dist/index.js" || true
pkill -f "python main_2.py" || true
pkill -f "tsx server/index.ts" || true
sleep 2

# Start Node.js frontend first
echo "Starting Node.js frontend on port 5000 (maps to external port 80)..."
NODE_ENV=production node dist/index.js &
NODE_PID=$!
echo "Node.js frontend started with PID: $NODE_PID"

# Wait for frontend to be ready (simple sleep since nc not available)
echo "Waiting for frontend to initialize..."
sleep 5

# Check if frontend process is still running
if ! kill -0 $NODE_PID 2>/dev/null; then
    echo "ERROR: Frontend failed to start"
    exit 1
fi
echo "Frontend is ready!"

# Now start Python Flask backend
echo "Starting Python Flask backend on port 8081..."
cd backend
export PYTHONUNBUFFERED=1
python main_2.py &
FLASK_PID=$!
echo "Flask backend started with PID: $FLASK_PID"

# Wait for backend to initialize
echo "Waiting for backend to initialize..."
sleep 5

# Check if backend process is still running
if ! kill -0 $FLASK_PID 2>/dev/null; then
    echo "ERROR: Backend failed to start"
    exit 1
fi

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