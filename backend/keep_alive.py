#!/usr/bin/env python3
"""
Persistent backend server manager
Ensures Flask server stays running and handles restarts
"""
import os
import sys
import time
import signal
import subprocess
import threading
from pathlib import Path

class BackendManager:
    def __init__(self):
        self.process = None
        self.running = True
        self.script_dir = Path(__file__).parent
        self.main_script = self.script_dir / "main_2.py"
        
    def start_server(self):
        """Start the Flask server process"""
        try:
            print(f"Starting Flask server: {self.main_script}")
            env = os.environ.copy()
            env['PYTHONUNBUFFERED'] = '1'
            
            self.process = subprocess.Popen(
                [sys.executable, str(self.main_script)],
                cwd=self.script_dir,
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                bufsize=1
            )
            return True
        except Exception as e:
            print(f"Failed to start server: {e}")
            return False
    
    def monitor_server(self):
        """Monitor server output and handle restarts"""
        while self.running and self.process:
            try:
                line = self.process.stdout.readline()
                if line:
                    print(f"[Backend] {line.strip()}")
                    # Check if server is ready
                    if "Running on" in line and "8081" in line:
                        print("[Backend] Server is ready and accepting connections!")
                elif self.process.poll() is not None:
                    # Process has terminated
                    print(f"[Backend] Process exited with code {self.process.returncode}")
                    if self.running:
                        print("[Backend] Restarting in 3 seconds...")
                        time.sleep(3)
                        if self.start_server():
                            continue
                    break
            except Exception as e:
                print(f"[Backend] Monitor error: {e}")
                break
    
    def run(self):
        """Main execution loop"""
        print("Backend Manager starting...")
        
        def signal_handler(signum, frame):
            print("Shutting down backend manager...")
            self.running = False
            if self.process:
                self.process.terminate()
            sys.exit(0)
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)
        
        if self.start_server():
            self.monitor_server()
        else:
            print("Failed to start initial server")

if __name__ == "__main__":
    manager = BackendManager()
    manager.run()