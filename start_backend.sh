#!/bin/bash

# Start backend server with proper process management
cd backend

# Kill any existing backend processes
pkill -f "python.*main_2.py" 2>/dev/null || true

# Start backend with nohup and proper redirection
nohup python -u main_2.py > ../backend.log 2>&1 </dev/null &

# Save PID for monitoring
echo $! > ../backend.pid
PID=$(cat ../backend.pid)
echo "Backend server started with PID: $PID"

# Wait for server to initialize
sleep 8

# Test if server is responding
if curl -s http://localhost:8081/api/user > /dev/null 2>&1; then
    echo "Backend server is responding on port 8081"
else
    echo "Backend server failed to start properly"
    tail -10 ../backend.log
    exit 1
fi

cd ..