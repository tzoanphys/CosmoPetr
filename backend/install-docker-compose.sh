#!/bin/bash

# Script to install docker-compose in WSL/Ubuntu

echo "Installing docker-compose..."

# Method 1: Install docker-compose-plugin (recommended for newer Docker versions)
echo "Attempting to install docker-compose-plugin..."
sudo apt-get update
sudo apt-get install -y docker-compose-plugin

# Verify installation
if docker compose version > /dev/null 2>&1; then
    echo "✅ docker compose plugin installed successfully!"
    echo "You can now use: docker compose up --build"
    exit 0
fi

# Method 2: Install standalone docker-compose (fallback)
echo "Installing standalone docker-compose..."
DOCKER_COMPOSE_VERSION="v2.24.0"
sudo curl -L "https://github.com/docker/compose/releases/download/${DOCKER_COMPOSE_VERSION}/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Verify installation
if docker-compose --version > /dev/null 2>&1; then
    echo "✅ docker-compose installed successfully!"
    echo "You can now use: docker-compose up --build"
    exit 0
fi

echo "❌ Installation failed. Please check the errors above."
exit 1

