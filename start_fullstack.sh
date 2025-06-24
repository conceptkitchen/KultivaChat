#!/bin/bash

# Kultivate AI Full Stack Startup Script
# Starts both frontend (Node.js) and backend (Python Flask) services

echo "Starting Kultivate AI Full Stack Application..."

# Start Python backend on port 8081 using proper nohup approach
echo "Starting Python Flask backend on port 8081..."
cd backend
nohup python -u main_2.py > ../backend.log 2>&1 </dev/null &
BACKEND_PID=$!
echo "Backend started with PID: $BACKEND_PID" > ../backend.pid
echo "Backend PID $BACKEND_PID saved to backend.pid"
cd ..

# Wait for backend to initialize
echo "Waiting for backend to initialize..."
sleep 8

# Test backend connection
echo "Testing backend connection..."
curl -s http://localhost:8081/api/user > /dev/null
if [ $? -eq 0 ]; then
    echo "Backend connection successful!"
else
    echo "Backend connection failed, checking logs..."
    tail -10 backend.log
fi

# Start Node.js frontend/proxy on port 5000
echo "Starting Node.js frontend/proxy on port 5000..."
npm run dev &
FRONTEND_PID=$!
echo "Frontend started with PID: $FRONTEND_PID"

# Keep script running and monitor processes
echo "Both services started successfully!"
echo "Frontend (Node.js): http://localhost:5000"
echo "Backend (Python): http://localhost:8081"
echo "Press Ctrl+C to stop all services"

# Monitor backend process
while kill -0 $BACKEND_PID 2>/dev/null; do
    sleep 5
done

echo "Backend process terminated, cleaning up..."
kill $FRONTEND_PID 2>/dev/null || true