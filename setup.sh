#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET_DIR="${1:-.opencode}"

echo "Setting up cl-toolkit..."

# Create opencode tools directory
mkdir -p "$TARGET_DIR/tools"

# Copy the plugin
echo "Installing opencode plugin..."
cp "$SCRIPT_DIR/opencode/tools/cl-toolkit.ts" "$TARGET_DIR/tools/"

# Update paths to be absolute
sed -i "s|path.resolve(__dirname, \"../../build/cl-toolkit\")|\"$SCRIPT_DIR/build/cl-toolkit\"|" "$TARGET_DIR/tools/cl-toolkit.ts"
sed -i "s|path.resolve(__dirname, \"../..\")  // Updated by setup.sh|\"$SCRIPT_DIR\"|" "$TARGET_DIR/tools/cl-toolkit.ts"

echo ""
echo "Setup complete!"
echo "  Plugin: $TARGET_DIR/tools/cl-toolkit.ts"
echo "  Binary will be built automatically on first use."
