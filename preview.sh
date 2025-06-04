#!/bin/bash

echo "🚀 Starting local preview server..."
echo "The website will be available at http://localhost:4444"
echo "Press Ctrl+C to stop the server"

# Run quarto preview with a specific port
quarto preview --port 4444 