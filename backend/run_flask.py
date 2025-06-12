#!/usr/bin/env python3
"""
Robust Flask server runner with auto-restart capability
"""
import os
import sys
import time
import signal
import subprocess
from pathlib import Path

def run_flask():
    """Run Flask server with automatic restart on crash"""
    script_dir = Path(__file__).parent
    main_script = script_dir / "main_2.py"
    
    while True:
        try:
            print(f"Starting Flask server from {main_script}")
            process = subprocess.Popen(
                [sys.executable, str(main_script)],
                cwd=script_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                bufsize=1
            )
            
            # Stream output in real-time
            for line in process.stdout:
                print(line.strip())
                if "Running on" in line:
                    print("Backend server is ready!")
                    
            process.wait()
            print(f"Flask process exited with code {process.returncode}")
            
            if process.returncode != 0:
                print("Restarting Flask server in 5 seconds...")
                time.sleep(5)
            else:
                break
                
        except KeyboardInterrupt:
            print("Shutting down Flask server...")
            if 'process' in locals():
                process.terminate()
            break
        except Exception as e:
            print(f"Error running Flask server: {e}")
            time.sleep(5)

if __name__ == "__main__":
    run_flask()