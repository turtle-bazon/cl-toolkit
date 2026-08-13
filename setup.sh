#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET_DIR="${1:-.opencode}"

echo "Setting up cl-toolkit..."

# Create opencode tools directory
mkdir -p "$TARGET_DIR/tools"

# Copy the tool plugin
echo "Installing opencode plugin..."
cp "$SCRIPT_DIR/opencode/tools/cl-toolkit.ts" "$TARGET_DIR/tools/"

# Update paths to be absolute
sed -i "s|path.resolve(__dirname, \"../../build/cl-toolkit\")|\"$SCRIPT_DIR/build/cl-toolkit\"|" "$TARGET_DIR/tools/cl-toolkit.ts"
sed -i "s|path.resolve(__dirname, \"../..\")  // Updated by setup.sh|\"$SCRIPT_DIR\"|" "$TARGET_DIR/tools/cl-toolkit.ts"

# Create package.json for module resolution if it doesn't exist
if [ ! -f "$TARGET_DIR/package.json" ]; then
  cat > "$TARGET_DIR/package.json" << 'EOF'
{
  "name": "opencode-tools",
  "type": "module",
  "dependencies": {
    "@opencode-ai/plugin": "*"
  }
}
EOF
  echo "Created $TARGET_DIR/package.json"
fi

# Install dependencies if node_modules doesn't exist
if [ ! -d "$TARGET_DIR/node_modules/@opencode-ai/plugin" ]; then
  echo "Installing dependencies..."
  (cd "$TARGET_DIR" && npm install --ignore-scripts 2>/dev/null || true)
fi

echo ""
echo "Setup complete!"
echo "  Plugin: $TARGET_DIR/tools/cl-toolkit.ts"
echo "  Binary: $SCRIPT_DIR/build/cl-toolkit"
