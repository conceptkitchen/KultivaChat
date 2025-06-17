# backend/main_2.py
import os
from typing import Optional
import logging
import json
import time

# Configure logging
logging.basicConfig(level=logging.INFO)

# --- Configuration ---
APP_NAME = "kultivachat_app"
USER_ID = "user_default"
SESSION_ID = "session_default"
MODEL_ID = "gemini-1.5-flash-latest"

# Load environment variables
GEMINI_API_KEY = os.environ.get('GEMINI_API_KEY')
KBC_API_URL = os.environ.get('KBC_API_URL')
KBC_STORAGE_TOKEN = os.environ.get('KBC_STORAGE_TOKEN')
GOOGLE_APPLICATION_CREDENTIALS_PATH = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')
KBC_WORKSPACE_SCHEMA = os.environ.get('KBC_WORKSPACE_SCHEMA')

# --- Tool Definitions ---
def get_zip_codes_for_city(city_name: str, state_code: Optional[str] = None) -> dict:
    """Gets the zip codes for a given city."""
    logging.info(f"Tool Call: get_zip_codes_for_city for {city_name}, State: {state_code}")
    
    # Mock implementation
    mock_zip_data = {
        "san francisco, ca": ["94102", "94103", "94104", "94105", "94107"],
        "daly city, ca": ["94014", "94015", "94016", "94017"],
        "vacaville, ca": ["95687", "95688", "95696"]
    }
    
    effective_state_code = state_code
    if not effective_state_code and city_name.lower() in ["san francisco", "daly city", "vacaville"]:
        effective_state_code = "ca"
    
    search_key = f"{city_name.lower()}"
    if effective_state_code:
        search_key = f"{city_name.lower()}, {effective_state_code.lower()}"
    
    if search_key in mock_zip_data:
        logging.info(f"Found zip codes for '{search_key}'")
        return {"status": "success", "zip_codes": mock_zip_data[search_key]}
    
    logging.warning(f"No zip codes found for '{search_key}'")
    return {
        "status": "error",
        "error_message": f"Could not find zip codes for {city_name}{f', {state_code}' if state_code else ''}"
    }

def query_database_tool(query: str) -> dict:
    """Queries the database and returns structured data."""
    print(f"--- Tool: query_database_tool called with query: {query} ---")
    
    if "orders" in query.lower():
        table_data = {
            "headers": ["Order ID", "Product", "Amount", "Status"],
            "rows": [
                ["1001", "Laptop", 1200, "Shipped"],
                ["1002", "Mouse", 25, "Processing"],
                ["1003", "Keyboard", 75, "Shipped"],
            ]
        }
        return {"status": "success", "type": "table", "data": table_data}
    else:
        return {"status": "error", "message": "I can only provide data for 'orders'."}

def get_current_time() -> dict:
    """Returns the current date, time, and timezone."""
    logging.info("Tool Call: get_current_time")
    current_time_str = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    return {"status": "success", "current_time": current_time_str}

# --- Service Initialization ---
def initialize_services():
    """Initializes and returns the core services and agent runner."""
    if not GEMINI_API_KEY:
        raise ValueError("GEMINI_API_KEY environment variable not set!")
    
    logging.info("Initializing services...")
    
    try:
        # Import ADK components only when needed
        from google.adk.agents import Agent, LlmAgent
        from google.adk.runners import Runner
        from google.adk.sessions import InMemorySessionService
        from google.genai import Client
        
        client = Client(api_key=GEMINI_API_KEY)
        session_service = InMemorySessionService()
        
        # Create the main agent
        main_agent = LlmAgent(
            model=MODEL_ID,
            name='kultivachat_main_agent',
            instruction="You are a helpful assistant. Use the provided tools to answer user questions.",
            tools=[get_zip_codes_for_city, query_database_tool, get_current_time]
        )
        
        runner = Runner(
            agent=main_agent,
            app_name=APP_NAME,
            session_service=session_service
        )
        
        logging.info("Services initialized successfully")
        return runner, session_service, APP_NAME, USER_ID
        
    except ImportError as e:
        logging.error(f"Failed to import ADK components: {e}")
        raise ValueError(f"Could not initialize AI services: {e}")
    except Exception as e:
        logging.error(f"Failed to initialize services: {e}")
        raise ValueError(f"Service initialization error: {e}")