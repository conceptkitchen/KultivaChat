#!/bin/bash
set -e 

echo "--- STARTING PRODUCTION BUILD & DEPLOY ---"

echo "[1/5] Installing root dependencies..."
npm install express http-proxy-middleware

echo "[2/5] Installing frontend dependencies..."
cd client && npm install && cd ..

echo "[3/5] Building production frontend..."
cd client && npm run build && cd ..

echo "[4/5] Installing backend dependencies..."
pip install -r backend/requirements_minimal.txt

echo "[5/5] Starting servers..."
# Start Python backend on port 8081
cd backend
gunicorn --config gunicorn.conf.py wsgi:application &
BACKEND_PID=$!

# Wait a moment for backend to start
sleep 3

# Start Node.js frontend server on port 5000
cd ..
exec node simple_server.js