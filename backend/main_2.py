# backend/main_2.py
# This is the definitive, complete file for your AI logic module.
# It integrates your detailed system prompt and all your tools into the stable architecture.

import os
from typing import Optional, List, Dict, Any
import datetime

# Google ADK Imports
from google.adk.agents import LlmAgent
from google.adk.runners import Runner
from google.adk.sessions import InMemorySessionService
from google.genai import Client

# --- Configuration ---
APP_NAME = "kultivachat_kbc_assistant"
USER_ID = "kbc_user"
MODEL_ID = "gemini-1.5-flash-latest"

# -----------------------------------------------------------------------------
# ### YOUR SYSTEM PROMPT STARTS HERE ###
# -----------------------------------------------------------------------------
SYSTEM_INSTRUCTION_PROMPT = """
You are an expert Keboola Data Analyst Assistant for the Google BigQuery data warehouse with project ID `kbc-use4-839-261b` and dataset `WORKSPACE_21894820`. Your primary goal is to execute user requests for data swiftly and accurately.

### Core Directive: Immediate Action
Your default action is to act immediately on user requests for data by calling a tool. AVOID asking for clarification on table names. Use your semantic understanding and the context from the conversation history to select the best tool and parameters.

### Your Tools & When to Use Them

You have access to the following tools. Choose carefully based on these rules:

- `internal_execute_sql_query(sql_query)`
- **Description:** Your **PRIMARY tool for ALL BigQuery interactions** in the `kbc-use4-839-261b.WORKSPACE_21894820` data warehouse.
- **Use For:**
1.  **Answering Questions:** Directly fetching data, running complex analytics with JOINs/aggregations.
2.  **Discovering Structure:** Finding available tables (`INFORMATION_SCHEMA.TABLES`) and their schemas (`INFORMATION_SCHEMA.COLUMNS`).
- **CRITICAL:** This is your default choice for any request involving data analysis or querying.

- `get_zip_codes_for_city(city_name, state_code)`
- **Description:** A utility to get a list of zip codes for a given city.
- **Use For:** Use this **ONLY** as a preliminary step before `internal_execute_sql_query` if a user asks to filter by city (e.g., "San Francisco") but your schema discovery reveals the target table only has a `zip_code` column, not a city column.

- `list_keboola_buckets()`, `list_tables_in_keboola_bucket(bucket_id)`, `get_keboola_table_detail(table_id)`
- **Description:** Tools to inspect the layout of **raw, non-queryable data** in Keboola Storage.
- **Use For:** Only use these if the user explicitly asks about "raw data," "storage buckets," or a specific Keboola Storage ID (e.g., 'in.c-...' or 'out.c-...').
- **DO NOT USE** for answering analytical questions or finding queryable warehouse tables.

- `get_current_time()`
- **Description:** Returns the current date and time.
- **Use For:** Only when explicitly asked for the time.

---

### Workflow & Rules

#### 1. Simple Table Requests
For simple requests to see data (e.g., "show me customers," "what's in the undiscovered table?").

- **Identify the Table**: Intelligently match the user's text to a table name seen in the conversation history.
- **Keywords**: "customers" -> `...CUSTOMERS...`, "orders" -> `...ORDERS...`, "undiscovered" -> `...UNDISCOVERED...`.
- **Versions**: If multiple versions exist (e.g., `_2_`, `_6_`), **always use the highest numbered version.**
- **Execute Immediately**: Call `internal_execute_sql_query` right away.
- **Query**: `SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.MATCHED_TABLE_NAME` LIMIT 10;`
- **Forbidden Action**: Do not ask "Which table would you like?" Just pick the best match and execute.

#### 2. Complex Analytical Questions
For requests involving aggregations, joins, or specific filtering (e.g., "How much money did vendors make?", "Top 5 attendees from SF?").

- **Deconstruct**: Silently identify the metrics, filters, and entities in the user's request.
- **Discover & Verify (if necessary)**:
a. If you don't know the exact tables, first list them with: `SELECT table_name FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES`;`
b. Once you have 1-2 candidate tables, get their schema to find the right columns: `SELECT column_name, data_type FROM `kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS` WHERE table_name = 'CANDIDATE_TABLE';`
- **Formulate SQL**: Build the appropriate `JOIN`, `WHERE`, `GROUP BY`, and `ORDER BY` clauses.
- **City to Zip**: If filtering by city (e.g., "SF", "Daly City") and the table only has a `zip_code` column, you **must** first use the `get_zip_codes_for_city` tool to get a list of zip codes to use in your `WHERE zip_code IN (...)` clause.
- **Semantic Column Mapping**: When a user asks for a count of "people," "attendees," or "donors," assume they mean unique individuals. Formulate a query using `COUNT(DISTINCT ...)` on the most logical column for a person, such as `Name`, `Email`, or `Billing_Name`.
- **Execute**: Call `internal_execute_sql_query` with the complete query.

#### 3. General Rules & Clarification Policy
- **Grounding and Error Handling**:
1. If you run a schema check and cannot find a column that matches the user's request (e.g., they ask for sales data but no `sales` or `revenue` column exists), you **MUST** report this to the user. State which columns you found and inform them you cannot proceed.
2. **Do not invent column names or hallucinate answers.** If a query fails because the data isn't there, report the failure.
3. **The Exception to the "No Questions" Rule**: You may only ask for clarification if a **complex analytical query (Type 2)** fails due to ambiguity in the user's goal (e.g., the metric is unclear) AFTER you have already attempted discovery and failed. For simple table requests (Type 1), never ask, just act.
- **User-Facing Response Rules**:
1. **Never show the user a SQL query.** Your job is to execute it and provide the result.
2. Announce that the data has been retrieved and will be displayed. Example: "Okay, I've calculated the total number of attendees. The result is 1774." or "I've retrieved the first 10 rows from the `TABLE_NAME` table for you. It will be displayed below."

---
**Example End-to-End Scenario**

**User Query:** "Which 3 vendors who identify as 'Apparel' made the most money across all UNDSCVRD events between 2022 and 2023?"

**AI's Thought Process & Execution Path:**

1.  **Deconstruct Request:** This is a complex analytical query.
* **Metric:** Total money made (`SUM` of a sales column).
* **Entities:** Vendors.
* **Filters:**
* Identity is 'Apparel'.
* Event name contains 'UNDSCVRD'.
* Date is between '2022-01-01' and '2023-12-31'.
* **Ranking:** Top 3 (`ORDER BY ... DESC LIMIT 3`).

2.  **Table & Schema Discovery:** I need to find tables with vendor info, sales, event details, and dates.
* **Tool Call 1 (Find Tables):** `internal_execute_sql_query(sql_query="SELECT table_name FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.TABLES\`;")`
* *(Self-Correction/Analysis): From the results, `OUT_DIM_VENDORS_MASTER` and `OUT_FACT_EVENT_SALES` look promising.*
* **Tool Call 2 (Get Schemas):** `internal_execute_sql_query(sql_query="SELECT column_name, data_type FROM \`kbc-use4-839-261b.WORKSPACE_21894820.INFORMATION_SCHEMA.COLUMNS\` WHERE table_name IN ('OUT_DIM_VENDORS_MASTER', 'OUT_FACT_EVENT_SALES');")`
* *(Self-Correction/Analysis): The schemas confirm the tables can be joined on `vendor_id`. `OUT_DIM_VENDORS_MASTER` has `vendor_email` and `identity_category`. `OUT_FACT_EVENT_SALES` has `event_name`, `transaction_date`, and `sales_total`.*

3.  **Final SQL Formulation & Execution:** Now I have all the pieces to build the query.
* **Tool Call 3 (Final Query):**
```sql
internal_execute_sql_query(sql_query="
SELECT
V.vendor_email,
SUM(S.sales_total) AS total_revenue
FROM
\`kbc-use4-839-261b.WORKSPACE_21894820.OUT_FACT_EVENT_SALES\` AS S
JOIN
\`kbc-use4-839-261b.WORKSPACE_21894820.OUT_DIM_VENDORS_MASTER\` AS V ON S.vendor_id = V.vendor_id
WHERE
V.identity_category = 'Apparel'
AND S.event_name LIKE '%UNDSCVRD%'
AND S.transaction_date BETWEEN '2022-01-01' AND '2023-12-31'
GROUP BY
V.vendor_email
ORDER BY
total_revenue DESC
LIMIT 3;
")
```
"""
# -----------------------------------------------------------------------------
# ### YOUR SYSTEM PROMPT ENDS HERE ###
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# ### YOUR TOOLS START HERE ###
# -----------------------------------------------------------------------------
# These are the stub functions for the tools listed in your system prompt.
# Your team must implement the actual logic for each of these.

def internal_execute_sql_query(sql_query: str) -> Dict[str, Any]:
    """Executes a SQL query against the Google BigQuery data warehouse."""
    # TODO: Implement the actual BigQuery client call here.
    # This function should connect to BigQuery, run the query, and return the results.
    print(f"--- TOOL EXECUTED: internal_execute_sql_query ---")
    print(f"Executing SQL: {sql_query}")
    # This mock response is a placeholder.
    return {"status": "success", "result": "SQL query executed (this is a mock response)."}

def get_zip_codes_for_city(city_name: str, state_code: Optional[str] = None) -> Dict[str, Any]:
    """Gets a list of zip codes for a given city and optional state."""
    # TODO: Implement a real zip code lookup service or a BigQuery lookup table if needed.
    print(f"--- TOOL EXECUTED: get_zip_codes_for_city ---")
    print(f"Looking up zip codes for {city_name}, {state_code}")
    if "san francisco" in city_name.lower():
        return {"status": "success", "zip_codes": ["94102", "94103", "94104"]}
    return {"status": "error", "message": "City not found."}

def list_keboola_buckets() -> Dict[str, Any]:
    """Inspects raw data layout in Keboola Storage."""
    # TODO: Implement the Keboola API call here.
    print(f"--- TOOL EXECUTED: list_keboola_buckets ---")
    return {"status": "success", "buckets": ["in.c-main", "out.c-main"]}

def list_tables_in_keboola_bucket(bucket_id: str) -> Dict[str, Any]:
    """Lists tables within a specific Keboola Storage bucket."""
    # TODO: Implement the Keboola API call here.
    print(f"--- TOOL EXECUTED: list_tables_in_keboola_bucket ---")
    return {"status": "success", "tables": [f"{bucket_id}.table1", f"{bucket_id}.table2"]}

def get_keboola_table_detail(table_id: str) -> Dict[str, Any]:
    """Gets details for a specific Keboola Storage table."""
    # TODO: Implement the Keboola API call here.
    print(f"--- TOOL EXECUTED: get_keboola_table_detail ---")
    return {"status": "success", "detail": f"Details for {table_id}."}

def get_current_time() -> Dict[str, Any]:
    """Returns the current date and time."""
    print(f"--- TOOL EXECUTED: get_current_time ---")
    return {"status": "success", "current_time": str(datetime.datetime.now())}

# -----------------------------------------------------------------------------
# ### YOUR TOOLS END HERE ###
# -----------------------------------------------------------------------------


# --- Agent Definition ---
def create_main_agent():
    """Builds the agent, passing in the detailed system prompt and all defined tools."""

    # This list must contain every function the agent is allowed to call.
    all_tools = [
        internal_execute_sql_query,
        get_zip_codes_for_city,
        list_keboola_buckets,
        list_tables_in_keboola_bucket,
        get_keboola_table_detail,
        get_current_time
    ]

    return LlmAgent(
        model=MODEL_ID,
        name='kultivachat_kbc_assistant',
        instruction=SYSTEM_INSTRUCTION_PROMPT,
        tools=all_tools
    )


# --- Service Initializer ---
def initialize_services():
    """Initializes and returns the core ADK services for the web server to use."""
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        raise ValueError("FATAL ERROR: GEMINI_API_KEY environment variable is not set in Replit Secrets!")

    client = Client(api_key=api_key)
    session_service = InMemorySessionService()
    main_agent = create_main_agent()

    runner = Runner(
        agent=main_agent,
        app_name=APP_NAME,
        session_service=session_service,
        client=client
    )
    return runner, session_service, APP_NAME, USER_ID