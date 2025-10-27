#!/bin/bash

# Installation script for Session Type Checker - Cursor Edition

set -e

echo "Installing Session Type Checker Language Support for Cursor..."

# Cursor extensions directory
CURSOR_EXT_DIR="$HOME/.cursor/extensions"

# Create extensions directory if it doesn't exist
mkdir -p "$CURSOR_EXT_DIR"

# Extension directory name
EXT_NAME="stc-language-0.1.0"
TARGET_DIR="$CURSOR_EXT_DIR/$EXT_NAME"

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

# Remove the install scripts from the destination
rm -f "$TARGET_DIR/install.sh"
rm -f "$TARGET_DIR/install-cursor.sh"

echo ""
echo "✓ Installation complete!"
echo ""
echo "Next steps:"
echo "1. Restart Cursor (Cmd+Q then reopen, or Cmd+Shift+P -> 'Reload Window')"
echo "2. Open a .stc file (try: examples/calculator.stc)"
echo "3. Check bottom-right corner - should show 'Session Type Checker'"
echo "4. Verify syntax highlighting is working"
echo ""
echo "To uninstall, run:"
echo "  rm -rf \"$TARGET_DIR\""

