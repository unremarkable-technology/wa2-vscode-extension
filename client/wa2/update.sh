#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

RUST_TB="rust"
NODE_TB="node"

echo "Building wa2lsp server..."
toolbox run -c "$RUST_TB" bash -lc "
  cd \"$ROOT/../../server/wa2lsp\"
  cargo build --release
"

echo "Copying binary to bin/..."
mkdir -p "$ROOT/bin"

VOLATILE_TARGET_DIR="/var/mnt/volatile/build/wa2"
SERVER_BIN="$VOLATILE_TARGET_DIR/release/wa2lsp"

if [[ ! -x "$SERVER_BIN" ]]; then
  echo "ERROR: expected server binary at: $SERVER_BIN"
  echo "Did cargo build --release succeed?"
  exit 1
fi

cp "$SERVER_BIN" "$ROOT/bin/"

echo "Current version: $(toolbox run -c "$NODE_TB" bash -lc "cd \"$ROOT\" && node -p \"require('./package.json').version\"")"
read -r -p "Bump to (patch/minor/major): " bump_type

toolbox run -c "$NODE_TB" bash -lc "
  cd \"$ROOT\"
  npm version \"$bump_type\"
  echo \"Packaging extension...\"
  npx @vscode/vsce package --allow-package-all-secrets
"

NEW_VERSION="$(toolbox run -c "$NODE_TB" bash -lc "cd \"$ROOT\" && node -p \"require('./package.json').version\"")"
echo "New version: $NEW_VERSION"
echo "✅ Extension packaged successfully!"
echo "📦 To publish: npx @vscode/vsce publish"
echo "🏷️  Or create git tag: git tag v$NEW_VERSION && git push origin v$NEW_VERSION"