import os
from google.cloud import bigquery
from google.auth.exceptions import RefreshError # To specifically catch this
import logging

# Configure basic logging
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

credentials_path = os.environ.get('GOOGLE_APPLICATION_CREDENTIALS')
logging.info(f"Using GOOGLE_APPLICATION_CREDENTIALS from env: {credentials_path}")

if not credentials_path:
    logging.error("Error: GOOGLE_APPLICATION_CREDENTIALS environment variable is not set.")
elif not os.path.exists(credentials_path):
    logging.error(f"Error: Credentials file not found at path: {credentials_path}")
else:
    try:
        logging.info("Attempting to initialize BigQuery client...")
        # Explicitly pass the credentials file to the client constructor
        # This is an alternative to relying solely on the environment variable being picked up,
        # though the environment variable method is standard and should work.
        client = bigquery.Client.from_service_account_json(credentials_path)
        logging.info(f"BigQuery client initialized. Project from client: {client.project}")

        logging.info("\nAttempting to list datasets (max 5) in project associated with credentials...")
        datasets = list(client.list_datasets(max_results=5)) # A simple read operation

        if datasets:
            logging.info(f"Successfully listed {len(datasets)} dataset(s):")
            for dataset in datasets:
                logging.info(f"- {dataset.dataset_id} (Project: {dataset.project})")
        else:
            logging.info("No datasets found or able to list (but auth might have succeeded if no error).")

        logging.info("\nGoogle Cloud authentication test seems to have passed if no errors were printed above from the client operations.")

    except RefreshError as re:
        logging.error(f"RefreshError during BigQuery operation (e.g., Invalid JWT Signature): {re}", exc_info=True)
    except Exception as e:
        logging.error(f"An unexpected error occurred: {e}", exc_info=True)