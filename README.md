This project was developed as part of a hackathon. Please take everything you see here with a grain of salt

# The Goal and Motivation

Our focus is on expert users. We aim to provide a tool to validate the enriched data extracted from decisions linked to the monuments. The benefit is twofold: having validated data that is proven to be correct and receiving feedback on the enrichment process to create datasets and fine-tune the models in an iterative approach later on. For this hackathon, the focus was on decisions linked to monument data, but the same setup can be generalized to any kind of data that can be enriched (e.g., agenda items, BPMN diagrams, etc.). Similarly, while we demonstrate the use of generating summaries for the decisions, the same principles can be applied to any kind of AI-generated metadata (e.g., tags, classes, link suggestions, etc.).

For instance, if the task is classification according to a given taxonomy, we can use a similar approach: first generating a suggestion of one or more classes for the decision, displaying it with a warning to expert users, and giving them the option to provide feedback on it (up- or down-vote the suggestions or correct the classes by removing or adding new ones). This ensures the correctness and completeness of the data and creates a dataset over time on which the models can be fine-tuned (the same LLM or a custom transformer-based classifier).

**Reasoning:** Everything starts with the quality of the data. A good model requires high-quality data. Currently, the biggest problem within ABB (from a data science perspective!) is the lack of qualitative data to train on (lack of correctly labeled data, lack of user feedback, etc.). To improve this and work more future-oriented, it’s important to begin gathering this data as early as possible to improve the quality of the already developed models (CI/CD principles and possible data drift throughout the lifecycle) and build datasets that can be used directly or in the future to further automate existing processes.

**KISS:** Start with something simple that can be extended upon. Use it for purposes that are achievable at the moment with the current technology (i.e., low-hanging fruit) and require little effort from the users to validate and correct (e.g., binary feedback or adding/removing labels) before moving on to complex tasks such as the correctness and completeness of conversational AI.

**Why not RAG:** Based on previous work with LLMs in the context of agenda items and decisions, current smaller models (2-8B) are inadequate for QA tasks in Dutch, especially in the context of legal documents or ABB in general. The grammar is still not 100% correct, and hallucinations are frequent when conversations drag on (deeper reasoning beyond the context is out of the question: e.g., "explain why I can't paint my window in X"). However, for tasks that rely on the provided context (such as documents or search results) and extracting information from it, small models can be effective. Examples of these tasks include summarization, translation, classification, and NER. Additionally, handling feedback and correcting the answers for fine-tuning requires significantly more effort (e.g., checking for validity and completeness with each linked planning decision) compared to metadata enrichment tasks.

# Architecture

## LLM Service

The model is built to run with both a local LLM (via Ollama) or accept an endpoint from an LLM provider (Deepinfra, Azure, OpenAI, etc.), through which a local, open-source, or private model can be used throughout the stack. In the local mode, it requires the Ollama Docker image to be included in the stack, and simply setting the endpoint of the LLM service to the endpoint of the Ollama service (see the [docker-compose.yml](docker-compose.yml) file).

This project is built using the [lblod/mu-python-ml-fastapi-template](https://github.com/lblod/mu-python-ml-fastapi-template) to make it easier to handle async requests. It provides interactive API documentation and an exploration web user interface at `http://endpoint:2000/docs#/`. For this hackathon, the service provides methods for fetching unprocessed decisions, endpoints for processing them using the LLM, and functionality to store the processed results into the triplestore as `oa:Annotations`.

Currently this service is included as a development service with it's source in the app repository itself. This should be turned into its own repository/microservice but we ran out of time before we could do this.

# Semantic Model

For the semantic model, we considered the following application profiles:

## [Cultureel Erfgoed Object](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-object/)

We used this to represent our monument instances, called 'aanduidingsobject' in our resources model. We reused what we could based on the concepts returned by the api, but we made some shortcuts, turning some types into regular literals for instance and leaving bits out. This was done to save time during the hackathon.

However, it is worth pointing out that there seem to be some issues with this model? The predicates don't seem to be typical clean uris and some types even use spaces in the URI. In the limited time we had to study this model it also feels like we would need some extensions in this model to make it truly useable. Many types are defined but don't have properties for instance and leave much up for interpretation. For the Location type we already used locn:Address to subclass from prov:Location.

Another important issue is that there is no good link between Besluit instances and Mens Gemaakt Object instances in the model. We created our own for in the ext namespace for now, but since this has to be reusable this namespace should be avoided. If this system were to go in production, it would be a good opportunity to align the Cultureel Erfgoed Object model with the business case and maybe refine or extend it.

## [Cultureel Erfgoed Event](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-event/)

This wasn't used yet. Not only did we have to prioritize, we also found some difficulties applying this model to this particular case, e.g. the model does say what actions are taken but no which actions are allowed... So we would need severe extensions anyway

## [Slimme Raadpleeg Omgeving](https://data.vlaanderen.be/doc/applicatieprofiel/slimmeraadpleegomgeving/)

Here we specifically use the http://www.w3.org/ns/oa#Annotation class to flag our AI enrichments. We use the predicates in [the annotation-vocab W3C recommendation](https://www.w3.org/TR/annotation-vocab/) to link it to the agent that created the annotation (and mark that agent as an AI agent).

We currently are rather basic in our use of this model. It could be extended with:

- better motivation instances that point to the exact sections of the document that were used and the models used to create the ai generated content
- better description of the type of content that is generated. We currently use just Annotation with a `dct:type` but this can go much further, generating actual bits of the OSLO model(s) used in the final product and having every annotation refer to the subject, predicate and object that it generates.

# AI Model

Here's the corrected version, maintaining the markdown format:

## AI Model Choice and Reasoning

**Based on the requirements of the hackathon:** the model had to be open-source and capable of running locally.

**Based on our needs:** achieving accurate results without being limited to real-time performance (not RAG/conversational), the model did not necessarily need to be small. While an LLM was not strictly needed, within the given time, training a custom model or fine-tuning was out of the question. As the decisions can be quite large, we chose to work with a model with a high enough context window to avoid chunking the text into pieces and merging it later on.

While an LLM has been fine-tuned to some degree on the decisions of _Lokaal Beslist_ for tasks like classification and keyword extraction, it's based on the older LLaMA 3 8B instruct model ([llama-3-8b-instruct-abb](https://huggingface.co/svercoutere/llama-3-8b-instruct-abb)). The latest LLaMA version 3.1 should perform better out of the box, with a significantly higher context window: 16k for the custom model vs. 128k of the default LLaMA 3.1. It is also easier to run locally without any additional setup (as custom models require), which makes testing it individually quicker during this hackathon.

The specific model we used is [llama3.1:8b-instruct-q4_0](https://ollama.com/library/llama3.1:8b-instruct-q4_0). Since the focus is not on conversation, we chose to work with an instruct model, which tends to perform better for instruction-based tasks and returning JSON-formatted responses. The q4 quantized version (reduced precision of the parameters) provides us with decent performance without any noticeable difference in accuracy (though this should be evaluated with some kind of dataset, perhaps? 😉).

# Assumptions, Shortcuts and Hacks

## Turning API responses into resource objects

Normally this belongs in its own service. For this hackathon, we started out doing this from the frontend side as it saves us time (we can use the resources json-api to convert things).

## Mu-authorization: all public

We put everything in the public graph of mu-authorization and don't care about users signing in. In the real application it would be better to do this properly so at least annotation-feedback is attributed to the right user and (maybe) certain monuments are only accessible by certain users, possibly depending on the region they are in?

## Supplying feedback for ai generated content

Our model for supplying feedback to the ai model is very basic. It would be better to use a deeper model like for instance the [FAIR\* Reviews Ontology](https://sparontologies.github.io/fr/current/fr.html). Currently we have a counter (number literal) for both positive and negative feedback and a set of comments left by users. We don't even link to the user leaving the review as we don't have mu-authorization/login. All of this would be a straight forward extension though, this was simply done in the interest of time.
