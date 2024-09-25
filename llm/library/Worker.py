import requests
import time
import json
import threading
import concurrent.futures
import logging
from library.Task import Task, TaskStatus
from escape_helpers import sparql_escape_string, sparql_escape_uri

import pymupdf4llm

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class WorkerManager:
    def __init__(self, worker_count, fetch_endpoint, post_endpoint, sleep_time=5, **kwargs):
        logging.info("Initializing WorkerManager")
        self.worker_count = worker_count
        self.fetch_endpoint = fetch_endpoint
        self.post_endpoint = post_endpoint
        self.sleep_time = sleep_time

        for key, value in kwargs.items():
            setattr(self, key, value)

        self.workers = [Worker(fetch_endpoint, post_endpoint, sleep_time, **kwargs) for _ in range(worker_count)]
        logging.info(f"Initialized {worker_count} workers")

    def start_workers(self):
        logging.info("Starting workers")
        for worker in self.workers:
            worker.reset()
        self.executor = concurrent.futures.ThreadPoolExecutor(max_workers=self.worker_count)
        self.futures = [self.executor.submit(worker.work) for worker in self.workers]
        logging.info("Workers started")

    def stop_workers(self):
        logging.info("Stopping workers")
        for worker in self.workers:
            worker.stop()
        self.executor.shutdown(wait=True)
        logging.info("Workers stopped")

class Worker:
    def __init__(self, fetch_endpoint, post_endpoint, sleep_time, **kwargs):
        logging.info("Initializing Worker")
        self.fetch_endpoint = fetch_endpoint
        self.post_endpoint = post_endpoint
        self.stop_event = threading.Event()
        self.sleep_time = sleep_time

        for key, value in kwargs.items():
            setattr(self, key, value)
        logging.info("Worker initialized")

    def post_results(self, uri, result_type, results):
        logging.info(f"Posting results for {uri}")
        logging.debug(f"Going to post following: {results}")
        data = {
            "body": sparql_escape_string(results),
            "motivation": "-enrichment process-",
            "annotation_type": result_type,
            "besluit_uri": uri
        }
        try:
            response = requests.post(self.post_endpoint, json=data)
            response.raise_for_status()
            logging.info(f"Results posted successfully for {uri}")
        except requests.exceptions.RequestException as e:
            error_message = f"Failed to post results for {uri}: {e}"
            logging.info(error_message)
            raise Exception(error_message)

    def process_task(self, task):
        logging.info(f"Processing task: {task}")
        try:
            if hasattr(self, 'processor'):
                processor = self.processor
            else:
                logging.info("Processor not found.")
                return

            besluit_uri = task.get("uri", "")
            download_link = task.get("downloadLink", "")

            if download_link:
                local_filename = "downloaded_file_tmp.pdf"
                try:
                    logging.info(f"Fetching file from {download_link}")
                    response = requests.get(download_link)
                    response.raise_for_status()

                    with open(local_filename, 'wb') as f:
                        f.write(response.content)
                    logging.info(f"File downloaded successfully: {local_filename}")

                    content = pymupdf4llm.to_markdown(local_filename)

                    results = processor.extract_keywords(content)
                    logging.info(f"Keywords extracted: {results}")
                    
                    # Ensure results is always a string
                    if isinstance(results, dict):
                        results_str = json.dumps(results)
                        logging.info(f"Results converted to JSON string: {results_str}")
                    else:
                        results_str = str(results)
                        logging.info(f"Results is already a string: {results_str}")

                    self.post_results(besluit_uri, "keywords", results_str)

                except Exception as e:
                    error_message = f"Failed to process task (processing or post): {e}"
                    logging.info(error_message)

        except Exception as e:
            error_message = f"Failed to process task: {e}"
            logging.info(error_message)

    def stop(self):
        logging.info("Stopping worker")
        self.stop_event.set()

    def reset(self):
        logging.info("Resetting worker")
        self.stop_event.clear()

    def work(self):
        logging.info("Worker started working")
        while not self.stop_event.is_set():
            try:
                tasks = requests.get(self.fetch_endpoint).json()
                task = tasks[0] if tasks else None
                logging.info(f"Received task: {task}")
                if task:
                    logging.info(f"Processing task: {task}")
                    self.process_task(task)
                else:
                    logging.info(f"No pending tasks. Sleeping for {self.sleep_time} seconds...")
                    self.stop_event.wait(self.sleep_time)
            except Exception as e:
                logging.info(f"Error fetching tasks: {e}")