#!/bin/bash

# Function to start Flask server
start_flask() {
    echo "Starting Flask server..."
    cd /home/runner/workspace/backend
    python main_2.py &
    FLASK_PID=$!
    echo "Flask server started with PID: $FLASK_PID"
    echo $FLASK_PID > flask.pid
}

# Function to check if Flask is running
is_flask_running() {
    if [ -f flask.pid ]; then
        PID=$(cat flask.pid)
        if ps -p $PID > /dev/null; then
            return 0
        else
            return 1
        fi
    else
        return 1
    fi
}

# Main loop
while true; do
    if ! is_flask_running; then
        echo "Flask server is not running. Starting..."
        start_flask
    else
        echo "Flask server is running."
    fi
    sleep 5
done