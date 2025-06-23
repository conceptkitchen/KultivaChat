#!/bin/bash
cd backend
python -u main_2.py &
FLASK_PID=$!
echo $FLASK_PID > ../flask_server.pid
echo "Flask server started with PID: $FLASK_PID"
wait $FLASK_PID