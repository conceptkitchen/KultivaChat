import subprocess
import os
import json
import threading
import queue
import time

# --- DEBUG Prints (to verify environment variables are seen by this script) ---
print(f"--- Start of main.py execution ---")
print(f"DEBUG in main.py - KBC_STORAGE_TOKEN: {os.environ.get('KBC_STORAGE_TOKEN')}")
print(f"DEBUG in main.py - KBC_WORKSPACE_SCHEMA: {os.environ.get('KBC_WORKSPACE_SCHEMA')}")
print(f"DEBUG in main.py - GOOGLE_APPLICATION_CREDENTIALS: {os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')}")
print(f"DEBUG in main.py - KBC_API_URL: {os.environ.get('KBC_API_URL')}")
print(f"DEBUG in main.py - GEMINI_API_KEY present: {'YES' if os.environ.get('GEMINI_API_KEY') else 'NO'}")
print(f"--- End of initial debug prints ---")

# --- Configuration ---
KBC_API_URL = os.environ.get('KBC_API_URL')
stderr_queue = queue.Queue()

def enqueue_output(pipe, q):
    """Reads lines from a pipe and puts them into a queue. (Used for stderr)"""
    try:
        with pipe:
            for line in iter(pipe.readline, ''):
                q.put(line)
    except Exception as e:
        q.put(f"Error in reading pipe: {e}\n")

def start_mcp_server_sse(): # Renamed function for clarity
    api_url = os.environ.get('KBC_API_URL')
    storage_token = os.environ.get('KBC_STORAGE_TOKEN')
    workspace_schema = os.environ.get('KBC_WORKSPACE_SCHEMA')

    if not all([api_url, storage_token, workspace_schema]):
        print("Error: One or more required environment variables for MCP server CLI args are not set.")
        if not api_url: print("KBC_API_URL is missing for MCP CLI.")
        if not storage_token: print("KBC_STORAGE_TOKEN is missing for MCP CLI.")
        if not workspace_schema: print("KBC_WORKSPACE_SCHEMA is missing for MCP CLI.")
        return None

    mcp_command = [
        "python",
        "-m", "keboola_mcp_server.cli",
        "--api-url", api_url,
        "--storage-token", storage_token,
        "--workspace-schema", workspace_schema,
        "--transport", "sse",                 # <-- ENSURE THIS IS SET TO sse
        "--port", "8765",
        "--host", "0.0.0.0",
        "--log-level", "DEBUG"
    ]
    print(f"Starting MCP Server with command: {' '.join(mcp_command)}")
    try:
        process = subprocess.Popen(
            mcp_command,
            stderr=subprocess.PIPE, 
            text=True,
            bufsize=1,
            env=os.environ.copy()
        )
        print(f"MCP Server (SSE mode) process started with PID: {process.pid}")

        stderr_thread = threading.Thread(target=enqueue_output, args=(process.stderr, stderr_queue))
        stderr_thread.daemon = True
        stderr_thread.start()
        return process
    except FileNotFoundError:
        print(f"Error: Command {' '.join(mcp_command)} not found or keboola_mcp_server not installed correctly for python -m.")
        return None
    except Exception as e:
        print(f"Error starting MCP Server (SSE mode) process: {e}")
        return None

def process_stderr(print_all=False):
    """Processes messages from the MCP server's stderr queue."""
    found_messages = False
    while not stderr_queue.empty():
        try:
            line = stderr_queue.get_nowait()
            print(f"MCP stderr: {line.strip()}")
            found_messages = True
        except queue.Empty:
            break
        except Exception as e:
            print(f"Error processing stderr queue: {e}")
            break
    if not found_messages and print_all:
        print("MCP stderr: (No new messages)")

# --- Main Execution ---
if __name__ == "__main__":
    print("Attempting to start MCP Server in SSE mode on port 8765...")
    mcp_process = start_mcp_server_sse()

    if mcp_process:
        print("MCP Server should be starting in SSE mode.")
        print("Check MCP stderr below for startup messages.")
        print("This script will now wait for 120 seconds to allow server to start and for you to run curl.")
        print("The MCP server process will continue running in the background in Replit if started successfully.")

        end_time = time.time() + 300  # 5 minutes from now
        initial_server_check_done = False
        while time.time() < end_time:
            if not initial_server_check_done and time.time() > (end_time - 110): 
                 print("\n--- Checking initial MCP stderr (SSE mode startup) ---")
                 process_stderr(print_all=True)
                 initial_server_check_done = True

            process_stderr(print_all=False) 

            if mcp_process.poll() is not None: 
                print(f"\nMCP Server (SSE mode) exited early during wait with code: {mcp_process.returncode}")
                break
            time.sleep(1) 

        if mcp_process.poll() is None: 
            print("\n--- Final check of MCP stderr (SSE mode after 120s) ---")
            process_stderr(print_all=True) 
            print(f"\nMCP Server (SSE mode) appears to be running in the background on port 8765.")
            print("You should have tried to access it using 'curl' from a NEW Replit shell during the wait, following the three-step process:")
            print("Step A (GET for SSE stream & session ID): curl -i -N -X GET -H \"Accept: text/event-stream\" http://localhost:8765/mcp/")
            print("Step B (POST initialize): curl -X POST -H \"Content-Type: application/json\" -H \"Accept: application/json\" -H \"mcp-session-id: <YOUR_SESSION_ID>\" -d '{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{\"clientInfo\":{\"name\":\"my-curl-client\",\"version\":\"0.1.0\"},\"capabilities\":{},\"protocolVersion\":\"1.0\"},\"id\":\"curl-init-sse-1\"}' http://localhost:8765/mcp/")
            print("Step C (POST tools/list): curl -X POST -H \"Content-Type: application/json\" -H \"Accept: application/json\" -H \"mcp-session-id: <YOUR_SESSION_ID>\" -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":\"curl-tools-list-sse-1\"}' http://localhost:8765/mcp/")
        else: 
            print(f"\nMCP Server (SSE mode) already exited with code: {mcp_process.returncode}")
            print("\n--- Checking MCP stderr after early exit ---")
            process_stderr(print_all=True) 

    else:
        print("MCP Server (SSE mode) did not start.")

    print("\nmain.py script finished.")