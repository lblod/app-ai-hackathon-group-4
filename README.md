This project was developed as part of a hackathon. Please take everything you see here with a grain of salt

# Architecture

# Semantic Model

For the semantic model, we considered the following application profiles:

## [Cultureel Erfgoed Object](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-object/)

We used this to represent our monument instances, called 'aanduidingsobject' in our resources model. We reused what we could based on the concepts returned by the api, but we made some shortcuts, turning some types into regular literals for instance and leaving bits out. This was done to save time during the hackathon.

However, it is worth pointing out that there seem to be some issues with this model? The predicates don't seem to be typical clean uris and some types even use spaces in the URI. In the limited time we had to study this model it also feels like we would need some extensions in this model to make it truly useable. Many types are defined but don't have properties for instance and leave much up for interpretation. For the Location type we already used locn:Address to subclass from prov:Location.

## [Cultureel Erfgoed Event](https://data.vlaanderen.be/doc/applicatieprofiel/cultureel-erfgoed-event/)

This wasn't used yet. Not only did we have to prioritize, we also found some difficulties applying this model to this particular case, e.g. the model does say what actions are taken but no which actions are allowed... So we would need severe extensions anyway

## [Slimme Raadpleeg Omgeving](https://data.vlaanderen.be/doc/applicatieprofiel/slimmeraadpleegomgeving/)

Here we specifically use the http://www.w3.org/ns/oa#Annotation class to flag our AI enrichments. We use the predicates in [the annotation-vocab W3C recommendation](https://www.w3.org/TR/annotation-vocab/) to link it to the agent that created the annotation (and mark that agent as an AI agent).

We currently are rather basic in our use of this model. It could be extended with:

- better motivation instances that point to the exact sections of the document that were used and the models used to create the ai generated content
- better description of the type of content that is generated. We currently use just Annotation with a `dct:type` but this can go much further, generating actual bits of the OSLO model(s) used in the final product and having every annotation refer to the subject, predicate and object that it generates.

# AI Model

# Assumptions, Shortcuts and Hacks

## Turning API responses into resource objects

Normally this belongs in its own service. For this hackathon, we started out doing this from the frontend side as it saves us time (we can use the resources json-api to convert things).

## No mu-authorization

We didn't use mu-authorization (or mu-sparql-parser) for this hackathon. This means no deltas and no access control. In a real project this would probably not be acceptable, but it saved us a lot of time during the hackathon.

## Supplying feedback for ai generated content

Our model for supplying feedback to the ai model is very basic. It would be better to use a deeper model like for instance the [FAIR\* Reviews Ontology](https://sparontologies.github.io/fr/current/fr.html). Currently we have a counter (number literal) for both positive and negative feedback and a set of comments left by users. We don't even link to the user leaving the review as we don't have mu-authorization/login. All of this would be a straight forward extension though, this was simply done in the interest of time.
