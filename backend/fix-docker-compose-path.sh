#!/bin/bash

# Script to fix docker-compose PATH issue in WSL

echo "Fixing docker-compose PATH issue..."

# Check if docker-compose exists
if [ -f /usr/local/bin/docker-compose ]; then
    echo "✅ docker-compose found at /usr/local/bin/docker-compose"
else
    echo "❌ docker-compose not found. Please install it first."
    exit 1
fi

# Add /usr/local/bin to PATH in .bashrc if not already there
if ! grep -q "/usr/local/bin" ~/.bashrc; then
    echo "" >> ~/.bashrc
    echo "# Add /usr/local/bin to PATH for docker-compose" >> ~/.bashrc
    echo "export PATH=/usr/local/bin:\$PATH" >> ~/.bashrc
    echo "✅ Added /usr/local/bin to PATH in ~/.bashrc"
else
    echo "✅ /usr/local/bin already in PATH"
fi

# Also add to .profile for non-interactive shells
if [ -f ~/.profile ] && ! grep -q "/usr/local/bin" ~/.profile; then
    echo "" >> ~/.profile
    echo "# Add /usr/local/bin to PATH for docker-compose" >> ~/.profile
    echo "export PATH=/usr/local/bin:\$PATH" >> ~/.profile
    echo "✅ Added /usr/local/bin to PATH in ~/.profile"
fi

echo ""
echo "✅ PATH fix complete!"
echo ""
echo "To apply changes, run one of these:"
echo "  source ~/.bashrc"
echo "  OR"
echo "  export PATH=/usr/local/bin:\$PATH"
echo ""
echo "Then test with: docker-compose --version"

