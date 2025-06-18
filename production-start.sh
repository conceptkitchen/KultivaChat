#!/bin/bash
set -e # Exit immediately if a command exits with a non-zero status.

echo "--- KULTIVACHAT PRODUCTION STARTUP ---"

# 1. Install frontend dependencies
echo "[1/4] Installing frontend dependencies..."
cd client
npm install
cd ..
echo "✅ Frontend dependencies installed."

# 2. Build the production frontend
echo "[2/4] Building production frontend..."
cd client
npm run build
cd ..
echo "✅ Frontend build complete."

# 3. Install backend dependencies
echo "[3/4] Installing backend dependencies..."
pip install -r backend/requirements.txt
echo "✅ Backend dependencies installed."

# 4. Start the Gunicorn backend server on Replit's designated port
echo "[4/4] Starting Gunicorn server..."
# --timeout: Prevents timeouts on slow AI tasks.
# --workers: Number of processes to handle requests. Adjust based on your Repl's resources.
# --bind: Binds to the port Replit exposes to the internet.
exec gunicorn --bind 0.0.0.0:8080 --workers 3 --timeout 120 backend.simple_app:app