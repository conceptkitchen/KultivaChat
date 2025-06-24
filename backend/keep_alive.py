#!/usr/bin/env python3
"""
Persistent backend server manager
Ensures Flask server stays running and handles restarts
"""
import subprocess
import time
import signal
import sys
import os
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class BackendManager:
    def __init__(self):
        self.process = None
        self.running = True
        
    def start_server(self):
        """Start the Flask server process"""
        try:
            logger.info("Starting Flask backend server...")
            self.process = subprocess.Popen(
                ["python", "main_2.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                bufsize=1
            )
            logger.info(f"Backend server started with PID: {self.process.pid}")
            return True
        except Exception as e:
            logger.error(f"Failed to start backend server: {e}")
            return False
    
    def monitor_server(self):
        """Monitor server output and handle restarts"""
        while self.running and self.process:
            try:
                # Check if process has terminated
                if self.process.poll() is not None:
                    logger.warning("Backend server process terminated, restarting...")
                    self.start_server()
                    continue
                
                # Read and log server output
                line = self.process.stdout.readline()
                if line:
                    print(f"[BACKEND] {line.strip()}")
                
                time.sleep(0.1)
            except Exception as e:
                logger.error(f"Error monitoring server: {e}")
                time.sleep(1)
    
    def stop_server(self):
        """Stop the Flask server"""
        self.running = False
        if self.process:
            logger.info("Stopping backend server...")
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
            logger.info("Backend server stopped")
    
    def run(self):
        """Main execution loop"""
        def signal_handler(signum, frame):
            logger.info("Received shutdown signal")
            self.stop_server()
            sys.exit(0)
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)
        
        if self.start_server():
            logger.info("Backend manager running. Press Ctrl+C to stop.")
            self.monitor_server()
        else:
            logger.error("Failed to start backend server")
            sys.exit(1)

if __name__ == "__main__":
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    manager = BackendManager()
    manager.run()