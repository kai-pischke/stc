#!/bin/bash

# Installation script for Session Type Checker VSCode Extension

set -e

echo "Installing Session Type Checker Language Support for VSCode..."

# Detect OS
if [[ "$OSTYPE" == "linux-gnu"* ]] || [[ "$OSTYPE" == "darwin"* ]]; then
    VSCODE_EXT_DIR="$HOME/.vscode/extensions"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]]; then
    VSCODE_EXT_DIR="$USERPROFILE/.vscode/extensions"
else
    echo "Unsupported OS: $OSTYPE"
    exit 1
fi

# Create extensions directory if it doesn't exist
mkdir -p "$VSCODE_EXT_DIR"

# Extension directory name
EXT_NAME="stc-language-0.1.0"
TARGET_DIR="$VSCODE_EXT_DIR/$EXT_NAME"

# Remove old version if exists
if [ -d "$TARGET_DIR" ]; then
    echo "Removing old version..."
    rm -rf "$TARGET_DIR"
fi

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Copy extension files
echo "Copying extension to $TARGET_DIR..."
cp -r "$SCRIPT_DIR" "$TARGET_DIR"

# Remove the install script from the destination
rm -f "$TARGET_DIR/install.sh"

echo ""
echo "✓ Installation complete!"
echo ""
echo "Next steps:"
echo "1. Restart VSCode"
echo "2. Open a .stc file"
echo "3. Verify syntax highlighting is working"
echo ""
echo "To uninstall, run:"
echo "  rm -rf \"$TARGET_DIR\""

