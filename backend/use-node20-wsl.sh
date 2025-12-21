#!/bin/bash
# Load nvm and use Node.js 20, then run the frontend dev server

# Load nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# Use Node.js 20
nvm use 20

# Verify versions
echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"
echo ""

# Navigate to frontend and run dev server
cd /mnt/c/Users/tzoan/OneDrive/Desktop/CosmoPetr/frontend
echo "Starting dev server..."
npm run dev

