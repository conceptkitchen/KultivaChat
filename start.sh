#!/bin/bash

# Production startup script that replaces npm start
echo "Starting production servers..."

# Kill existing processes
pkill -f "node dist/index.js" 2>/dev/null || true
pkill -f "python main_2.py" 2>/dev/null || true
sleep 2

# Start frontend
echo "Starting frontend on port 5000..."
NODE_ENV=production node dist/index.js &
FRONTEND_PID=$!
echo "Frontend PID: $FRONTEND_PID"

# Wait for frontend to initialize
sleep 3

# Start backend
echo "Starting backend on port 8081..."
cd backend
PYTHONUNBUFFERED=1 python main_2.py &
BACKEND_PID=$!
echo "Backend PID: $BACKEND_PID"
cd ..

# Function to cleanup
cleanup() {
    echo "Cleaning up..."
    kill $FRONTEND_PID 2>/dev/null || true
    kill $BACKEND_PID 2>/dev/null || true
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

echo "Both servers started successfully!"
echo "Frontend: port 5000 (external: 80)"
echo "Backend: port 8081"

# Keep the script running by waiting for both processes
while kill -0 $FRONTEND_PID 2>/dev/null && kill -0 $BACKEND_PID 2>/dev/null; do
    sleep 5
done

echo "One of the servers stopped, shutting down..."
cleanup