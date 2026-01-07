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

echo "Bumping version..."
npm version patch

echo "Packaging extension..."
npx @vscode/vsce package

echo ""
echo "✅ Extension packaged successfully!"
echo "📦 Upload at: https://marketplace.visualstudio.com/manage/publishers/FigmentEngineLtd"