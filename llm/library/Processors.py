import json


class DecisionProcessor:
    def __init__(self, llm_model):
        self.llm_model = llm_model

    def extract_keywords(self, agendapunt):
        # Convert the JSON object to a string
        text = json.dumps(agendapunt)

        # Generate the classification task
        prompt_string, system_message, task_string, context = self.llm_model.generate_keyword_task(text)

        # Generate the response using OpenAI (or any other method)
        response = self.llm_model.generate_response(system_message, prompt_string, stream=False, json_mode=True)
        response_json = self.llm_model.extract_json(response)

        if response_json is not None:
            task_info = self.llm_model.get_task_info("keywords_agendapunt", "keywords", "keywords_agendapunt", "json", "json")
            saveable = self.llm_model.formatted_results(task_info, text, task_string, context, system_message, prompt_string, response)

        return response_json, saveable

    def translate(self, agendapunt, language, agenda_punten_format = None):

        if agenda_punten_format is None:
            agenda_punten_format = """{'id': 'Do not translate', 'name': 'Translated Name', 'source': 'Source language', 'target': 'Target language'}"""

        prompt_string, system_message, task_string, context = self.llm_model.generate_translation_task(agendapunt, language, agenda_punten_format)
        response = self.llm_model.generate_response(system_message,prompt_string)

        response_json = self.llm_model.extract_json(response)

        if response_json is not None:
            task_info = self.llm_model.get_task_info("translate_agendapunt", "translate", "translate_agendapunt", "json", "json", language)
            saveable = self.llm_model.formatted_results(task_info, None, task_string, context, system_message, prompt_string, response)

        return response_json, saveable
    
    def classify(self, agendapunt, taxonomy = None):
    
        if taxonomy is None:
            taxonomy = self.taxonomy
            
        # Convert the JSON object to a string
        text = json.dumps(agendapunt)

        # Generate the classification task
        prompt_string, system_message, task_string, context = self.llm_model.generate_classification_task(text, taxonomy)

        # Generate the response using OpenAI (or any other method)
        response = self.llm_model.generate_response(system_message, prompt_string, stream=False, json_mode=False)
        response_json = self.llm_model.extract_json(response)

        if response_json is not None:
            task_info = self.llm_model.get_task_info("classification_agendapunt", "classification", "classification_agendapunt", "json", "json") 
            saveable = self.llm_model.formatted_results(task_info, text, task_string, context, system_message, prompt_string, response)

        return response_json, saveable
    