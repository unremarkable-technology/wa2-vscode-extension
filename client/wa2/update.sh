#!/bin/bash
set -e

echo "Building wa2lsp server..."
cd ../../server/wa2lsp
cargo build --release
cd ../../client/wa2

echo "Copying binary to bin/..."
mkdir -p bin
CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-../../server/wa2lsp/target}"
cp "$CARGO_TARGET_DIR/release/wa2lsp" bin/

echo "Current version: $(node -p "require('./package.json').version")"
#read -p "Bump to (patch/minor/major): " bump_type
#npm version $bump_type

NEW_VERSION=0.0.13
#NEW_VERSION=$(node -p "require('./package.json').version")
#echo "New version: $NEW_VERSION"

echo "Packaging extension..."
npx @vscode/vsce package

echo ""
echo "✅ Extension packaged successfully!"
echo "📦 To publish: npx @vscode/vsce publish"
echo "🏷️  Or create git tag: git tag v$NEW_VERSION && git push origin v$NEW_VERSION"