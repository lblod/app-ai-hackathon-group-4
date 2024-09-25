#!/bin/sh

# Check if the model name is provided
if [ -z "$OLLAMA_MODEL" ]; then
  echo "No model specified. Exiting."
  exit 1
fi

# Pulling the model
echo "Pulling model $OLLAMA_MODEL"
ollama pull $OLLAMA_MODEL


# Start Ollama service
echo "Starting Ollama service"
ollama serve


