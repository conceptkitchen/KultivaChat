#!/bin/bash

# Comprehensive production startup script
echo "Starting Kultivate AI production deployment..."

# Force kill all related processes
pkill -9 -f "node dist/index.js" 2>/dev/null || true
pkill -9 -f "python main_2.py" 2>/dev/null || true  
pkill -9 -f "tsx server/index.ts" 2>/dev/null || true
sleep 3

# Verify dist directory exists
if [ ! -f "dist/index.js" ]; then
    echo "Building production bundle..."
    npm run build
fi

echo "Starting frontend server (port 5000 -> external port 80)..."
NODE_ENV=production nohup node dist/index.js > frontend.log 2>&1 &
FRONTEND_PID=$!
echo "Frontend PID: $FRONTEND_PID"

# Give frontend time to bind to port
sleep 8

echo "Starting backend server (port 8081)..."
cd backend
nohup python main_2.py > ../backend.log 2>&1 &
BACKEND_PID=$!
echo "Backend PID: $BACKEND_PID"
cd ..

# Verify both processes are running
sleep 5

if ! kill -0 $FRONTEND_PID 2>/dev/null; then
    echo "ERROR: Frontend failed to start"
    cat frontend.log
    exit 1
fi

if ! kill -0 $BACKEND_PID 2>/dev/null; then
    echo "ERROR: Backend failed to start"  
    cat backend.log
    exit 1
fi

echo "SUCCESS: Both servers are running"
echo "Frontend: http://localhost:5000 (external: port 80)"
echo "Backend: http://localhost:8081"

# Keep the main process alive to prevent deployment termination
cleanup() {
    echo "Received termination signal, cleaning up..."
    kill $FRONTEND_PID 2>/dev/null || true
    kill $BACKEND_PID 2>/dev/null || true
    exit 0
}

# Set signal handlers
trap cleanup SIGINT SIGTERM

# Monitor and keep both processes alive indefinitely
while true; do
    # Check if frontend is still running
    if ! kill -0 $FRONTEND_PID 2>/dev/null; then
        echo "Frontend process died, restarting..."
        NODE_ENV=production nohup node dist/index.js > frontend.log 2>&1 &
        FRONTEND_PID=$!
        echo "Frontend restarted with PID: $FRONTEND_PID"
    fi
    
    # Check if backend is still running
    if ! kill -0 $BACKEND_PID 2>/dev/null; then
        echo "Backend process died, restarting..."
        cd backend
        nohup python main_2.py > ../backend.log 2>&1 &
        BACKEND_PID=$!
        echo "Backend restarted with PID: $BACKEND_PID"
        cd ..
    fi
    
    # Sleep before next check
    sleep 15
done