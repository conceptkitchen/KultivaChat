#!/usr/bin/env python3
"""
Production Flask server with robust startup and error handling
"""
import os
import sys
import time
import signal
import subprocess
import threading
from pathlib import Path

def run_flask_production():
    """Run Flask server with production-ready error handling"""
    script_dir = Path(__file__).parent
    main_script = script_dir / "main_2.py"
    
    # Set environment for production
    env = os.environ.copy()
    env['PYTHONUNBUFFERED'] = '1'
    env['FLASK_ENV'] = 'production'
    
    # Remove PORT variable to ensure Flask uses 8081
    if 'PORT' in env:
        del env['PORT']
    
    print(f"Starting Flask production server from {main_script}")
    print("Environment variables configured for production")
    
    try:
        process = subprocess.Popen(
            [sys.executable, str(main_script)],
            cwd=script_dir,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            universal_newlines=True,
            bufsize=1
        )
        
        # Monitor output and signal readiness
        flask_ready = False
        for line in process.stdout:
            print(line.strip())
            
            # Check for Flask server ready signals
            if any(signal in line for signal in [
                "Running on http://127.0.0.1:8081",
                "Starting Flask server",
                "Serving Flask app"
            ]):
                if not flask_ready:
                    flask_ready = True
                    print("✓ Flask server is ready for production")
                    
        process.wait()
        print(f"Flask process completed with exit code {process.returncode}")
        
    except KeyboardInterrupt:
        print("Shutting down Flask production server...")
        if 'process' in locals():
            process.terminate()
            process.wait()
    except Exception as e:
        print(f"Error in Flask production server: {e}")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(run_flask_production())