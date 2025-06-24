#!/bin/bash

# Start the Node.js frontend server in the background
npm run dev &

# Navigate to the backend directory
cd backend

# Install Python dependencies if needed
pip install -r requirements.txt

# Start the Python Flask backend server
python main_2.py