#!/bin/bash

# Quick setup script to enable docker-compose in current session

echo "Setting up docker-compose..."

# Add /usr/local/bin to PATH for current session
export PATH=/usr/local/bin:$PATH

# Verify docker-compose is accessible
if docker-compose --version > /dev/null 2>&1; then
    echo "✅ docker-compose is now available!"
    echo ""
    echo "You can now run:"
    echo "  docker-compose up --build"
    echo ""
    echo "Note: This PATH change is only for this terminal session."
    echo "For permanent fix, run: source ~/.bashrc"
else
    echo "❌ docker-compose still not found. Checking installation..."
    if [ -f /usr/local/bin/docker-compose ]; then
        echo "✅ docker-compose exists at /usr/local/bin/docker-compose"
        echo "Try running: /usr/local/bin/docker-compose --version"
    else
        echo "❌ docker-compose not installed. Please run install-docker-compose.sh first"
    fi
fi

