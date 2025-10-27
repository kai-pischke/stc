# Installation Guide

## Quick Install (Manual)

1. **Locate your VSCode extensions folder**:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`
   - **macOS/Linux**: `~/.vscode/extensions/`

2. **Copy the extension**:
   ```bash
   # From the stc project root
   cp -r vscode-extension ~/.vscode/extensions/stc-language-0.1.0
   ```

3. **Restart VSCode**

4. **Verify installation**:
   - Open a `.stc` file
   - Check the language mode in the bottom-right corner (should show "Session Type Checker")
   - Verify syntax highlighting is active

## Using VSCE (Recommended for Distribution)

### Prerequisites
```bash
npm install -g @vscode/vsce
```

### Package the Extension
```bash
cd vscode-extension
vsce package
```

This creates a `.vsix` file (e.g., `stc-language-0.1.0.vsix`)

### Install the Package

**Option 1: Via Command Line**
```bash
code --install-extension stc-language-0.1.0.vsix
```

**Option 2: Via VSCode UI**
1. Open VSCode
2. Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
3. Click the "..." menu at the top-right
4. Select "Install from VSIX..."
5. Choose the `.vsix` file

### Publish to Marketplace (Optional)

1. **Create a Publisher Account**:
   - Go to https://marketplace.visualstudio.com/manage
   - Create a publisher ID

2. **Login with vsce**:
   ```bash
   vsce login your-publisher-name
   ```

3. **Publish**:
   ```bash
   vsce publish
   ```

## Development Setup

For developing the extension:

1. **Open in VSCode**:
   ```bash
   cd vscode-extension
   code .
   ```

2. **Test the extension**:
   - Press F5 to open a new VSCode window with the extension loaded
   - Open a `.stc` file to test syntax highlighting

3. **Make changes**:
   - Edit `syntaxes/stc.tmLanguage.json` for syntax rules
   - Edit `language-configuration.json` for language features
   - Reload the extension window (Ctrl+R / Cmd+R) to see changes

## Troubleshooting

### Extension not loading
- Check that the folder name is correct: `stc-language-0.1.0`
- Restart VSCode completely
- Check VSCode console for errors: Help → Toggle Developer Tools

### Syntax highlighting not working
- Verify the file extension is `.stc`
- Check the language mode in bottom-right corner
- Try closing and reopening the file

### Changes not appearing
- After modifying the extension, reload VSCode: Ctrl+R / Cmd+R (in the Extension Development Host)
- Or restart VSCode completely

## Uninstall

**Manual Installation**:
```bash
rm -rf ~/.vscode/extensions/stc-language-0.1.0
```

**VSIX Installation**:
1. Open Extensions view
2. Find "Session Type Checker Language Support"
3. Click the gear icon → Uninstall

