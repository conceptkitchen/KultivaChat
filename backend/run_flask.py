#!/usr/bin/env python3
import subprocess
import time
import sys
import os

def run_flask():
    """Run Flask server with automatic restart on crash"""
    while True:
        try:
            print("Starting Flask server...")
            # Change to backend directory
            os.chdir('/home/runner/workspace/backend')
            
            # Run the Flask app
            process = subprocess.Popen(
                [sys.executable, 'main_2.py'],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                bufsize=1
            )
            
            # Monitor the process and print output
            while True:
                output = process.stdout.readline()
                if output == '' and process.poll() is not None:
                    break
                if output:
                    print(output.strip())
                    
            # Process has ended
            return_code = process.poll()
            print(f"Flask server stopped with return code: {return_code}")
            
            if return_code == 0:
                print("Flask server stopped normally.")
                break
            else:
                print("Flask server crashed. Restarting in 3 seconds...")
                time.sleep(3)
                
        except KeyboardInterrupt:
            print("Shutting down Flask server...")
            if 'process' in locals():
                process.terminate()
            break
        except Exception as e:
            print(f"Error running Flask server: {e}")
            time.sleep(3)

if __name__ == "__main__":
    run_flask()