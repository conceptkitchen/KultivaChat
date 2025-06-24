#!/bin/bash

# Kultivate AI Full Stack Startup Script
# Starts both frontend (Node.js) and backend (Python Flask) services

echo "Starting Kultivate AI Full Stack Application..."

# Start Python backend on port 8081
echo "Starting Python Flask backend on port 8081..."
cd backend
python main_2.py &
BACKEND_PID=$!
echo "Backend started with PID: $BACKEND_PID"
cd ..

# Wait for backend to initialize
sleep 5

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

# Wait for processes
wait $BACKEND_PID $FRONTEND_PID