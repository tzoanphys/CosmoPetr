#!/bin/bash

# Script to update Node.js in WSL to a compatible version
# This script installs nvm (Node Version Manager) and updates Node.js

set -e

echo "=== Updating Node.js in WSL ==="
echo "Current Node.js version: $(node --version 2>/dev/null || echo 'not installed')"
echo "Current npm version: $(npm --version 2>/dev/null || echo 'not installed')"
echo ""

# Check if nvm is already installed
if [ -s "$HOME/.nvm/nvm.sh" ]; then
    echo "nvm is already installed. Loading it..."
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
else
    echo "Installing nvm (Node Version Manager)..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash
    
    # Load nvm
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
fi

echo ""
echo "Installing Node.js 20 (LTS)..."
nvm install 20
nvm use 20
nvm alias default 20

echo ""
echo "=== Update Complete ==="
echo "New Node.js version: $(node --version)"
echo "New npm version: $(npm --version)"
echo ""
echo "To use this Node.js version in future sessions, add these lines to your ~/.bashrc:"
echo "  export NVM_DIR=\"\$HOME/.nvm\""
echo "  [ -s \"\$NVM_DIR/nvm.sh\" ] && \. \"\$NVM_DIR/nvm.sh\""
echo "  [ -s \"\$NVM_DIR/bash_completion\" ] && \. \"\$NVM_DIR/bash_completion\""

