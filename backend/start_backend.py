#!/usr/bin/env python3
import subprocess
import sys
import os

def start_backend():
    # Change to backend directory
    os.chdir('/home/runner/workspace/backend')
    
    # Start the main_2.py server
    try:
        subprocess.run([sys.executable, 'main_2.py'], check=True)
    except KeyboardInterrupt:
        print("Backend server stopped")
    except Exception as e:
        print(f"Error starting backend: {e}")

if __name__ == "__main__":
    start_backend()