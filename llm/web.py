# Standard library imports
import json
import os
from datetime import datetime

# Third party imports
from fastapi import FastAPI, UploadFile, HTTPException, File, Request, Body
from fastapi.responses import HTMLResponse, JSONResponse
from pydantic import BaseModel
from typing import List, Optional, Dict

# Local application imports (from image)
from escape_helpers import sparql_escape_string, sparql_escape_uri


# Local application imports (from library)
from library.Llm import LLM
from library.Task import TaskStatus
from library.Worker import WorkerManager
from library.Processors import DecisionProcessor
from library.helpers import update, query, generate_uuid

upload_folder = '/app/uploads/'

accepted_file_extensions = ['.bpmn', '.txt', '.xml']


# Initialize the LLM
LLM_API_KEY = os.environ.get('LLM_API_KEY', 'ollama')
LLM_ENDPOINT = os.environ.get('LLM_ENDPOINT', 'http://ollama:11434/v1/')
LLM_MODEL_NAME = os.environ.get('LLM_MODEL_NAME', 'llama3.1:8b-instruct-q4_0')
LLM_MODEL_URI = os.environ.get('LLM_MODEL_URI', 'https://ollama.com/library/llama3.1:8b-instruct-q4_0')
LLM_ON_AZURE = os.environ.get('LLM_ON_AZURE', 'False').lower() == 'true'


default_graph = os.getenv('DEFAULT_GRAPH', "http://mu.semte.ch/graphs/public")
queue_graph = os.getenv('QUEUE_GRAPH', "http://mu.semte.ch/graphs/public")


print(f"Starting LLM with endpoint: {LLM_ENDPOINT}, model: {LLM_MODEL_NAME}, azure: {LLM_ON_AZURE}, api_key: {LLM_API_KEY}")

abb_llm = LLM(base_url = LLM_ENDPOINT, api_key = LLM_API_KEY, model_name = LLM_MODEL_NAME, azure = LLM_ON_AZURE)


""" decision_processor = DecisionProcessor(abb_llm)


# Initialize the worker manager
worker_manager = WorkerManager(1,
                                sleep_time=10,
                                queue_endpoint="http://localhost/tasks",
                                graph_endpoint="http://localhost/tasks/results",
                                agendapunten_processor=decision_processor)
 """

# input classes for the endpoints
class TaskInput(BaseModel):
    id: str
    type: str
    action: str
    parameters: dict
    context: dict

class TaskResultInput(BaseModel):
    id: str
    user_input: Optional[str]
    task: str
    context: dict
    system_message: str
    prompt: str
    response: str
    meta: dict

class TranslationInput(BaseModel):
    text: str
    language: str
    format: Dict[str, str]

class KeywordExtractionInput(BaseModel):
    text: str

class ClassificationInputTaxonomy(BaseModel):
    taxonomy: Dict[str, List[str]]

class SummarizationInput(BaseModel):
    text: str

class RawPromptInput(BaseModel):
    system_message: str
    prompt: str

class AnnotationInput(BaseModel):
    body: str
    motivation: str
    annotation_type: str
    besluit_uri: str

# methods for storing the results of the tasks
async def batch_update(queries, request : Request):
    """
    Execute a batch of SPARQL INSERT queries.

    This function receives a list of SPARQL INSERT queries and a request object.
    It iterates over the list of queries and executes each one.

    Args:
    queries: A list of SPARQL INSERT queries to be executed.
    request: A Request object that contains client request information.

    Returns:
    None. The function executes the queries but does not return anything.
    """
    for query in queries:
        update(query, request)

# main function for storing the results into the graph
def generate_annotation_insert_query(annotation: AnnotationInput, sparql_graph: str = default_graph) -> str:
    """
    Generate SPARQL INSERT query for an annotation.

    Parameters:
    annotation (AnnotationInput): The annotation input model containing annotation properties. The structure should be:
        {
            "body": "The body of the annotation",
            "motivation": "The motivation for the annotation",
            "annotation_type": "The type of the annotation or type of enrichment: summarization, keywords, etc",
            "besluit_uri": "The URI of the besluit",
        }
    sparql_graph (str): The URI of the SPARQL graph to insert data into.

    Returns:
    str: The SPARQL INSERT query.
    """

    # Generate a unique UUID for the annotation
    annotation_uuid = generate_uuid()
    annotation_uri = f"http://data.lblod.info/annotations/{annotation_uuid}"

    # Get annotation-type URI
    annotation_type_uri = f"http://data.lblod.info/annotations/annotation_types/{annotation.annotation_type}"

    # Define prefixes
    prefixes = """
        PREFIX dct: <http://purl.org/dc/terms/>
        PREFIX oa: <http://www.w3.org/ns/oa#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
    """

    # Construct the SPARQL INSERT query
    query = f"""
        {prefixes}
        INSERT DATA {{
            GRAPH <{sparql_graph}> {{
                {sparql_escape_uri(annotation_uri)} a oa:Annotation ;
                    mu:uuid "{annotation_uuid}" ;
                    oa:hasBody "{annotation.body}" ;
                    dct:created "{datetime.now().isoformat()}"^^xsd:dateTime ;
                    oa:motivatedBy "{annotation.motivation}" ;

                    oa:hasTarget {sparql_escape_uri(annotation.besluit_uri)} ;
                    dct:type {sparql_escape_uri(annotation_type_uri)} ;
                    dct:creator {sparql_escape_uri(LLM_MODEL_URI)} .

                {sparql_escape_uri(annotation_type_uri)} a oa:AnnotationType ;
                    skos:prefLabel "{annotation.annotation_type}" .
            }}
        }}
    """
    return query


""" 
# event handlers for worker management
@app.on_event("startup")
async def startup_event():
    worker_manager.start_workers()

@app.on_event("shutdown")
async def shutdown_event():
    worker_manager.stop_workers()

@app.post("/restart_workers")
async def restart_workers():
    worker_manager.stop_workers()
    worker_manager.start_workers()
    return {"message": "Workers restarted successfully"}

 """
# Default endpoint
@app.get("/", response_class=HTMLResponse)
async def root():
    return """
    <html>
        <body>
            <p>Welcome to our ABB hackathon LLM services! For more information visit <a href="/docs">/docs</a>.</p>
        </body>
    </html>
    """

# Task queue endpoints
@app.get("/tasks", tags=["tasks"])
async def get_tasks(request: Request, limit: int = 1):
    """
    Get all tasks with a specified status.

    This function queries the task queue in the application graph 
    for tasks and returns a fixed number of items.

    Args:
        limit (int): The maximum number of tasks to retrieve.

    Returns:
        list: A list of tasks with their attributes.
    """
    print("Triggered get_tasks")
    # Create the SPARQL SELECT query
    query_string = f"""
            PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
            PREFIX dct: <http://purl.org/dc/terms/>
            PREFIX oa: <http://www.w3.org/ns/oa#>
    
            SELECT ?decision ?downloadLink ?title ?concept
            WHERE {{
                GRAPH {sparql_escape_uri(queue_graph)} {{
                    ?decision a besluit:Besluit;
                       ext:dowloadLink ?downloadLink;
                       ext:oeBesluitUri ?besluit_uri;

    
                    # Optionally retrieve title and concept
                    OPTIONAL {{ ?decision dct:title ?title . }}
                    OPTIONAL {{ ?decision dct:type ?concept . }}
    
                    # Ensure there are no annotations linked via oa:hasTarget
                    FILTER NOT EXISTS {{
                        ?annotation a oa:Annotation ;
                                    oa:hasTarget ?decision .
                    }}
                }}
            }}
            LIMIT {limit}
        """
    # Execute the SPARQL SELECT query
    print("Executing query", query_string)
    try:
        result = query(query_string, request)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

    tasks = result["results"]["bindings"]
    if not tasks:
        raise HTTPException(status_code=404, detail="No tasks found.")

    return [
        {
            "uri": task["decision"]["value"],
            "downloadLink": task["downloadLink"]["value"],
            "title": task.get("title", {}).get("value", ""),
            "concept": task.get("concept", {}).get("value", ""),
        }
        for task in tasks
    ]

# storing results back to the graph
@app.post("/tasks/results", tags=["tasks"])
async def store_task_results(request: Request, annotation: AnnotationInput, sparql_graph: Optional[str] = default_graph):
    """
    Endpoint for generating SPARQL INSERT queries from a dictionary.

    Parameters:
    graph_dict (dict): The dictionary to generate queries from.
    sparql_graph (str): The URI of the SPARQL graph to insert data into.

    Returns:
    list: A list of SPARQL INSERT queries.
    """
    try:
        query = generate_annotation_insert_query(annotation, sparql_graph)
        print(f"Generated query: {query}")
        # Assuming batch_update is an async function

        await update(query, request)

        return {"status": "success", "message": "Batch update completed successfully"}
    except Exception as e:
        return {"status": "error", "message": str(e)}

#general ai endpoints
@app.post("/translate", tags=["text"])
async def translate_text(translation_input: TranslationInput):
    """
    Translates the given text to the specified language.
    
    Args:
        translation_input (TranslationInput): An object containing the text to be translated, the target language, and the format of the response.
    
    Returns:
        dict: A dictionary containing the translated text, the source language, and the target language.
    
    Raises:
        HTTPException: If there's an error during the translation.
    
    Example:
        To use this endpoint, you can send a POST request to `/translate` with a JSON body like this:
    
        {
            "text": "Dit is een zin die vertaald moet worden naar het Engels.",
            "language": "en",
            "format": {
                "translated_text": "Translated text",
                "source_language": "Source language",
                "target_language": "Target language"
            }
        }
    
        The response will be a dictionary containing the translated text, the source language, and the target language, like this:
    
        {
            "text": "This is a sentence that needs to be translated into English.",
            "Source": "nl",
            "Target": "en"
        }
    """
    try:
        result = abb_llm.translate_text(translation_input.text, language=translation_input.language, format=translation_input.format)
        return result
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))
    
@app.post("/extract_keywords", tags=["text"])
async def extract_keywords_text(keyword_input: KeywordExtractionInput):
    """
    Extracts keywords from the given text.

    Args:
        keyword_extraction_input (KeywordExtractionInput): An object containing the text from which keywords should be extracted.

    Returns:
        list: A list of keywords extracted from the text.

    Raises:
        HTTPException: If there's an error during the keyword extraction.

    Example:
        To use this endpoint, you can send a POST request to `/extract_keywords` with a JSON body like this:

        {
            "text": "Dit is een zin met keywords zoals Computer Vision, Natural Language Processing en Machine Learning."
        }

        The response will be a list of keywords, like this:

        {"keywords": ["Computer Vision", "Natural Language Processing", "Machine Learning"]}
    """
    try:
        keywords = abb_llm.extract_keywords_text(keyword_input.text)
        return keywords
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))
    
@app.post("/classify_text", tags=["text"])
async def classify_text(text: str, classification_taxonomy: ClassificationInputTaxonomy):
    """
    Classifies the given text according to the provided taxonomy.

    Args:
        text (str): The text to be classified.
        classification_taxonomy (ClassificationInputTaxonomy): An object containing the taxonomy.

    Returns:
        str: The classification of the text according to the taxonomy.

    Raises:
        HTTPException: If there's an error during the classification.

    Example:
        To use this endpoint, you can send a POST request to `/classify_text` with a JSON body like this:

        {
            
                "taxonomy": {
                    "Computer science": ["Computer Vision", "Natural Language Processing", "Machine Learning"],
                    "Physics": ["Quantum Mechanics", "General Relativity"],
                    "Biology": ["Evolution", "Genetics"]
                }
            
        }

        The response will be the classification of the text, like this:

        ```json{"classification": {"Computer science": ["Machine Learning"] }}```
    """
    try:
        classification = abb_llm.classify_text(text, taxonomy=classification_taxonomy.taxonomy)
        return classification
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.post("/summarize", tags=["text"])
async def summarize_text(summarization_input: SummarizationInput):
    """
    Summarizes the given text.

    Args:
        summarization_input (SummarizationInput): An object containing the text to be summarized.

    Returns:
        dict: A dictionary containing the summarized text.

    Raises:
        HTTPException: If there's an error during the summarization.

    Example:
        To use this endpoint, you can send a POST request to `/summarize` with a JSON body like this:

        {
            "text": "This is a long text that needs to be summarized."
        }

        The response will be a dictionary containing the summarized text, like this:

        {
            "summary": "This is a summary."
        }
    """
    try:
        summary = abb_llm.summarize_text(summarization_input.text)
        return summary
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/raw_prompt", tags=["text"])
async def raw_prompt(raw_prompt_input: RawPromptInput):
    """
    Processes a raw prompt and returns the response.

    Args:
        raw_prompt_input (RawPromptInput): An object containing the raw prompt.

    Returns:
        dict: A dictionary containing the response to the raw prompt.

    Raises:w
        HTTPException: If there's an error during the processing of the raw prompt.

    Example:
        To use this endpoint, you can send a POST request to `/raw_prompt` with a JSON body like this, the system message or prompt should mention "JSON". 
        Best explicitly give the expected JSON format as an example in the prompt.

        {
            "prompt": "Tell me a joke. Return it as a JSON object with the following format: {\"joke\": \"the joke itself\"}"
        }

        The response will be a dictionary containing the response to the raw prompt, like this:

        {
            "joke": "Why did the scarecrow win an award? Because he was outstanding in his field!"
        }
    """
    try:
        response = abb_llm.process_raw_prompt(raw_prompt_input.system_message, raw_prompt_input.prompt)
        return response
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))