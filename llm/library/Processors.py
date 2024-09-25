import json
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class DecisionProcessor:
    def __init__(self, llm_model):
        logging.info("Initializing DecisionProcessor")
        self.llm_model = llm_model

    def extract_keywords(self, text):
        logging.info("Extracting keywords within the LLM class")
        try:
            prompt_string, system_message, _, _ = self.llm_model.generate_keyword_task(text)
            response = self.llm_model.generate_response(system_message, prompt_string, stream=False, json_mode=True)
            logging.debug(f"Generated response: {response}")

            response_json = self.llm_model.extract_json(response)
            logging.info("Keywords extracted successfully")
            return response_json
        except Exception as e:
            logging.error(f"Error extracting keywords: {e}")
            raise

    def translate(self, text, language, text_format=None):
        logging.info("Translating text")
        try:
            if text_format is None:
                text_format = """{'id': 'Do not translate', 'name': 'Translated Name', 'source': 'Source language', 'target': 'Target language'}"""
            logging.debug(f"Using text_format: {text_format}")

            prompt_string, system_message, task_string, context = self.llm_model.generate_translation_task(text, language, text_format)
            logging.debug(f"Generated translation task: {prompt_string}")

            response = self.llm_model.generate_response(system_message, prompt_string)
            logging.debug(f"Generated response: {response}")

            response_json = self.llm_model.extract_json(response)
            logging.info("Translation completed successfully")
            return response_json
        except Exception as e:
            logging.error(f"Error translating text: {e}")
            raise

    def classify(self, text, taxonomy=None):
        logging.info("Classifying text")
        try:
            if taxonomy is None:
                taxonomy = self.taxonomy
            logging.debug(f"Using taxonomy: {taxonomy}")

            text_json = json.dumps(text)
            logging.debug(f"Converted text to JSON string: {text_json}")

            prompt_string, system_message, _, _ = self.llm_model.generate_classification_task(text_json, taxonomy)
            logging.debug(f"Generated classification task: {prompt_string}")

            response = self.llm_model.generate_response(system_message, prompt_string, stream=False, json_mode=False)
            logging.debug(f"Generated response: {response}")

            response_json = self.llm_model.extract_json(response)
            logging.info("Classification completed successfully")
            return response_json
        except Exception as e:
            logging.error(f"Error classifying text: {e}")
            raise