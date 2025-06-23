#!/bin/bash
set -e 

echo "--- STARTING PRODUCTION BUILD & DEPLOY ---"

echo "[1/4] Installing frontend dependencies..."
cd client && npm install && cd ..

echo "[2/4] Building production frontend..."
cd client && npm run build && cd ..

echo "[3/4] Installing backend dependencies..."
pip install -r backend/requirements.txt

echo "[4/4] Starting Python Gunicorn server..."
exec gunicorn --workers 3 --timeout 120 'backend.run_flask:app' --bind 0.0.0.0:8080