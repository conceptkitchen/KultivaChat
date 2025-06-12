#!/bin/bash

echo "Starting Python Flask backend on port 8081..."
cd backend

# Kill any existing backend processes
pkill -f "python main_2.py" 2>/dev/null || true

# Start backend in background with logging
nohup python main_2.py > ../backend.log 2>&1 &
BACKEND_PID=$!

echo "Backend started with PID: $BACKEND_PID"
echo $BACKEND_PID > ../backend.pid

# Wait for backend to initialize
sleep 5

# Test if backend is responding
if curl -s http://localhost:8081/api/health > /dev/null 2>&1; then
    echo "Backend is responding on port 8081"
else
    echo "Backend started but health check failed - checking logs..."
    tail -5 ../backend.log
fi