This project was developed as part of a hackathon. Please take everything you see here with a grain of salt

# Architecture

The ollama service provides as an endpoint for the locally running llm model
The llm service uses the ollama service processes documents, prompts the llm model via the ollama service, and stores the resulting annotated objects into the triplestore


# Semantic Model

For the semantic model, we considered the following application profiles:

## [Cultureel Erfgoed Object](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-object/)

We used this to represent our monument instances, called 'aanduidingsobject' in our resources model. We reused what we could based on the concepts returned by the api, but we made some shortcuts, turning some types into regular literals for instance and leaving bits out. This was done to save time during the hackathon.

However, it is worth pointing out that there seem to be some issues with this model? The predicates don't seem to be typical clean uris and some types even use spaces in the URI. In the limited time we had to study this model it also feels like we would need some extensions in this model to make it truly useable. Many types are defined but don't have properties for instance and leave much up for interpretation. For the Location type we already used locn:Address to subclass from prov:Location.

## [Cultureel Erfgoed Event](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-event/)

This wasn't used yet. Not only did we have to prioritize, we also found some difficulties applying this model to this particular case, e.g. the model does say what actions are taken but no which actions are allowed... So we would need severe extensions anyway

## [Slimme Raadpleeg Omgeving](https://data.vlaanderen.be/doc/applicatieprofiel/slimmeraadpleegomgeving/)

# AI Model

# Assumptions, Shortcuts and Hacks

## Turning API responses into resource objects

Normally this belongs in its own service. For this hackathon, we started out doing this from the frontend side as it saves us time (we can use the resources json-api to convert things).
