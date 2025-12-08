## ensure our server is up to date
cargo build --release

# Marketplace does not allow re-uploading the same version.
npm version patch

# Package the extension
npx @vscode/vsce package

# Publish to the Marketplace
echo upload at https://marketplace.visualstudio.com/manage/publishers/FigmentEngineLtd