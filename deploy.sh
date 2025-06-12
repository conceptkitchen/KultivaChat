#!/bin/bash
# Deployment script that ensures both frontend and backend start

echo "Building application..."
npm run build

echo "Starting production servers..."
exec ./start.sh