#!/usr/bin/env python3
import subprocess
import sys
import time
import signal
import os

def run_flask():
    """Run Flask server with automatic restart on crash"""
    while True:
        try:
            print("Starting Flask server...")
            # Start the Flask process
            process = subprocess.Popen([
                sys.executable, 'main_2.py'
            ], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            
            # Monitor the process
            while True:
                output = process.stdout.readline()
                if output == '' and process.poll() is not None:
                    break
                if output:
                    print(output.strip())
                    
            # Process has terminated
            return_code = process.poll()
            print(f"Flask server exited with code {return_code}")
            
            if return_code != 0:
                print("Restarting in 5 seconds...")
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

if __name__ == '__main__':
    run_flask()