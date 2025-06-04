#!/bin/bash

# Print start message
echo "Starting website deployment process..."

# Run the R deployment script
Rscript deploy/deploy.R

# Check if the deployment was successful
if [ $? -eq 0 ]; then
    echo "✅ Website built successfully!"
    echo "You can now commit and push your changes to GitHub."
else
    echo "❌ There was an error building the website. Please check the error messages above."
    exit 1
fi 