#!/bin/bash
cd backend
python main_2.py &
BACKEND_PID=$!
echo "Python backend started with PID: $BACKEND_PID"
echo $BACKEND_PID > backend.pid
sleep 5
echo "Backend should be running on port 8081"