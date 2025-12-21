#!/bin/bash
# Reinstall node_modules in WSL to get Linux binaries

# Load nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# Use Node.js 20
nvm use 20

echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"
echo ""

# Navigate to frontend
cd /mnt/c/Users/tzoan/OneDrive/Desktop/CosmoPetr/frontend

echo "Removing old node_modules (Windows binaries)..."
rm -rf node_modules package-lock.json

echo "Installing dependencies for Linux (WSL)..."
npm install

echo ""
echo "Installation complete! Now you can run: npm run dev"

