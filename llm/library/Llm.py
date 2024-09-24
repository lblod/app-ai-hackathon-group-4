import json
import uuid
import os
from openai import OpenAI, AzureOpenAI
import requests
import re


class LLM:
    """
    A class to interact with with the LLM.
    """

    def __init__(self, base_url, model_name, api_key, temperature=0.0, max_tokens=2048, top_p=0.9, azure = False):
        """
        Initialize the LLM class with base_url, model_name and api_key.
        """
        if not azure:
            self.client = OpenAI(
                api_key=api_key,
                base_url=base_url
            )
        else:
            self.client = AzureOpenAI(
                api_key=api_key,  
                api_version='2024-02-01',
                azure_endpoint=base_url
            )
        self.model_name = model_name
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.top_p = top_p

    # Generate response from the LLM API
    def generate_response(self, system_message, prompt_string, stream=False, json_mode =False):
        """
        Generate a response from the OpenAI API based on the system_message and prompt_string.
        """

        # Set the response_format based on the json_mode parameter
        response_format = {"type": "json_object"} if json_mode else None

        chat_completion = self.client.chat.completions.create(
            model=self.model_name,
            response_format=response_format,
            messages=[
                {"role": "system", "content": system_message},
                {"role": "user", "content": prompt_string},
            ],
            stream=stream,
            temperature=self.temperature,
            max_tokens=self.max_tokens,
            top_p=self.top_p
        )
    
        return chat_completion
      
    def generate_response_messaging(self, messages, system_message, stream=True):
        """
        Generate a response from the OpenAI API based on the system_message and a list of messages.
        """
        messages = [{"role":"system", "content": system_message}] + list(messages)
        chat_completion = self.client.chat.completions.create(
            model=self.model_name,
            messages=messages,
            stream=stream,
            temperature=self.temperature,
            max_tokens=self.max_tokens,
            top_p=self.top_p
        )
    
        return chat_completion
    
    # Prompt formatting
    def generate_prompt(self, task, context, system_message, include_system_message=True):
        if include_system_message:
            return f"####\nContext: {context}\n####\nInstructions:  {system_message} \n####\n{task}", system_message
        else:
            return f"####\nContext: {context}\n####\n{task}", system_message

    def generate_json_prompt(self, task, context, include_system_message=True):
        system_message = "Your task is to generate responses in JSON format. Ensure that your output strictly follows the provided JSON structure. Each key in the JSON should be correctly populated according to the instructions given. Pay attention to details and ensure the JSON is well-formed and valid."
        
        return self.generate_prompt(task, context, system_message, include_system_message)

    def generate_rag_prompt(self, task, context, include_system_message=True):
        system_message = """You are a Retrieval Augmented Generator (RAG). Your task is to complete tasks and answer questions based on the provided context.
    1. Language: Respond only in Dutch.
    2. Relevance: Use only the information from the provided context to form your response. If the context does not contain relevant information to the question or task, respond with 'Sorry, ik kan de gevraagde informatie niet terugvinden.'
    3. Only respond to questions related to the context."""
        
        return self.generate_prompt(task, context, system_message, include_system_message)


    # Response formatting

    def clean_json_string(self, json_string):
        pattern = r'^```json\s*(.*?)\s*```$'
        cleaned_string = re.sub(pattern, r'\1', json_string, flags=re.DOTALL)
        return cleaned_string.strip()

    def extract_json_formatted(self, response):
        #remove trailing and leading json formatting
        response_text = response.choices[0].message.content
        response_text = self.clean_json_string(response_text)
        try:
            return json.loads(response_text)
        except json.JSONDecodeError:
            print(f"Skipping invalid JSON in response: {response_text}")
            return None
        
    def extract_json(self, response):
        response_text = response.choices[0].message.content
        try:
            start = response_text.index('{')
            end = response_text.rindex('}') + 1
            json_text = response_text[start:end]
            return json.loads(json_text)
        except (json.JSONDecodeError, ValueError) as e:
            print(f"Error decoding JSON: {e}")
            return {"failed to extract JSON": response_text}

    # Response processing

    def get_task_info(self, task_name, category, sub_category ,  input_data_type = "bpmn", response_data_type ="json" , language = "Dutch"):
        task_info = {
        "task_name": task_name,
        "category": category,
        "sub_category": sub_category,
        "input_data_type": input_data_type,
        "response_data_type": response_data_type,
        "language": language
        }
        return task_info

    def formatted_results(self, task_info, user_input, task, context, system_message, prompt_string, response):

        #extract message and meta from response
        response_json = json.loads(response.model_dump_json())
        response_text = response_json["choices"][0].pop("message", {"content":"Sorry, ik kan de gevraagde informatie niet terugvinden."})["content"]

        #use remaining information as meta
        meta = response_json
        
        meta["task_info"] = task_info

        # Create the data to save
        data = {
            'id': str(uuid.uuid4()),  # Generate a new UUID for the ID
            'user_input': user_input,
            'task': task,
            'context': context,
            'system_message': system_message,
            'prompt': prompt_string,
            'response': response_text,
            'meta': meta
        }

        return data


    # TASK: keyword extraction

    def generate_keyword_task(self, context, include_system_message=True):

        task = """
        
            You are an expert in extracting keywords from text data based on a provided context. Your task is to identify the most relevant keywords in the given text provided in the context.
        
            Extract the keywords from the document provided in context. Ensure that the keywords are accurate and specific to the context provided.
            Focus on general themes, organizations, locations, etc. These keywords should provide a high-level overview of the document's content.
            Order the keywords based on their relevance and importance in the document.
        
            Desired Output:
            Output the keywords in a JSON format, like this:
            {
                "keywords": ["keyword1", "keyword2", "keyword3"]
            }
        """

        prompt, system = self.generate_json_prompt(task, context, include_system_message)

        return prompt, system, task, {"context": context}
    
    def extract_keywords_text(self, text):
        # Generate the classification task
        prompt_string, system_message, _, _ = self.generate_keyword_task(text)

        # Generate the response using OpenAI (or any other method)
        response = self.generate_response(system_message, prompt_string, stream=False, json_mode=True)

        return response.choices[0].message.content

    # TASK: classification
    def generate_classification_task(self, context, taxonomy, include_system_message=True):

        task_string = """

        You are an expert in classifying text data based on a provided context and a predefined hierarchy. The hierarchy is provided in JSON format. Your task is to classify the given text according to this hierarchy and the context provided. If none of the existing classes or subclasses apply, you may generate new ones.

        Here is the classification hierarchy:

        {taxonomy}

        Classify the document provided in context according to the hierarchy. If no suitable class exists within the provided hierarchy, suggest a new one. A document can belong to multiple classes or subclasses. Ensure that the classification is accurate and specific to the context provided.

        Desired Output:
        Output the classification path or the new class suggestion in a JSON format, like this:
        {{
            "classification": {{
                "Category1": ["Subcategory1", "Subcategory2"],
                "Category2": ["Subcategory3"]
            }}
        }}
        or
        {{
            "classification": {{
                "name_new_category_1": ["name_new_subcategory_1"],
                "name_new_category_2": ["name_new_subcategory_2", "name_new_subcategory_3"]
            }}
        }}

        """

        task = task_string.format(taxonomy=json.dumps(taxonomy, indent=4))
        prompt, system = self.generate_json_prompt(task, context, include_system_message)

        return prompt, system, task, {"context": context, "taxonomy": taxonomy}

    def classify_text(self, text, taxonomy):
        """
        Classify the given text based on the provided taxonomy.
    
        Args:
            text (str): The text to be classified.
            taxonomy (dict): The taxonomy to be used for classification. The taxonomy should be a dictionary where each key is a category and each value is a list of keywords related to that category.
    
        Returns:
            str: The classified text.
        """
        # Generate the classification task
        prompt_string, system_message, _, _ = self.generate_classification_task(text, taxonomy)
    
        # Generate the response using OpenAI (or any other method)
        response = self.generate_response(system_message, prompt_string, stream=False, json_mode=False)
    
        return response.choices[0].message.content
    
    def classify_json(self, json_object, taxonomy):
        """
        Classify the given JSON object based on the provided taxonomy.
    
        Args:
            json_object (dict): The JSON object to be classified.
            taxonomy (dict): The taxonomy to be used for classification. The taxonomy should be a dictionary where each key is a category and each value is a list of keywords related to that category.
    
        Returns:
            dict: The classified JSON object.
        """
        # Convert the JSON object to a string
        text = json.dumps(json_object)
    
        # Classify the text
        response_text = self.classify_text(text, taxonomy, context=json_object)
        response_json = self.extract_json(response_text)
        return response_json

    # TASK: translation
    
    def generate_translation_task(self, context, language, format, include_system_message=False):
        task = f"""Task: Translate all fields of the item in the context to {language}, except for the 'id' field and any field explicitly marked as 'do not translate'. Place names and IDs should remain in their original language. Return the translated text as a JSON object with the following format:


        {format}
        """
        prompt, system = self.generate_json_prompt(task, json.dumps(context), include_system_message)
        return prompt, system, task, {"context": context, "language": language, "format": format}

    def generate_translation_tasks(self, context, language, format, include_system_message=False):
        task = f"""Task: Translate all fields of each item in the context to {language}, except for the 'id' field and any field explicitly marked as 'do not translate'. Place names and IDs should remain in their original language. Return the translated text as a JSON array, maintaining the original structure and format of each item:
        {{"translations": [{format}, {format}, ...]}}

        """
        prompt, system = self.generate_json_prompt(task, json.dumps(context), include_system_message)
        return prompt, system, task, {"context": context, "language": language, "format": format}

    def translate_text(self, text, language, format):
        """
        Translate the given text to the specified language.
    
        Args:
            text (str): The text to be translated.
            language (str): The language to translate the text to.
            format (str): The format of the translated text.
    
        Returns:
            str: The translated text.
        """
        # Generate the translation task
        prompt_string, system_message, _, _ = self.generate_translation_task(text, language, format)
    
        # Generate the response using OpenAI (or any other method)
        response = self.generate_response(system_message, prompt_string, stream=False, json_mode=False)
    
        return response.choices[0].message.content
    

    # TASK: summarization

    def generate_summarization_task(self, context, include_system_message=True):

        task = """
        You are an expert in summarizing text data based on a provided context. Your task is to summarize the given text provided in the context.
        
        Summarize the document provided in context. Ensure that the summary is concise and captures the main points of the document. 
        Keep the text in its original language and do not translate the names of people, places, or organizations.
        
        Desired Output:
        Output the summary in a JSON format, like this:
        {
            "summary": "Your summary here."
        }
        """

        prompt, system = self.generate_json_prompt(task, context, include_system_message)

        return prompt, system, task, {"context": context}
    
    def summarize_text(self, text):
        # Generate the classification task
        prompt_string, system_message, _, _ = self.generate_summarization_task(text)

        # Generate the response using OpenAI (or any other method)
        response = self.generate_response(system_message, prompt_string, stream=False, json_mode=True)

        return response.choices[0].message.content
    

    # TASK: general prompting

    def generate_prompt_task(self, context, include_system_message=True):

        task = """
        You are an expert in asnwering prompts based on a provided context. Your task is to interpret the given prompt and generate a response based on the context provided.
                
        Desired Output:
        
        
        """

        prompt, system = self.generate_json_prompt(task, context, include_system_message)

        return prompt, system, task, {"context": context}