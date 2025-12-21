#!/bin/bash
# Install/Update Node.js in WSL using nvm

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

echo "Installing Node.js 20 (LTS)..."
nvm install 20
nvm use 20
nvm alias default 20

echo ""
echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"

