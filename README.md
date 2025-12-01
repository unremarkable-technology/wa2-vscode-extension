
# WA2 VS Code Extension (Early Preview)

The **WA2 VS Code extension** provides **fast, workspace-aware validation**
for CloudFormation templates inside VS Code.

This is an **early open-source prototype** focused on Phase 1 of the WA2
project: a high-performance language server (LSP) that can analyse CloudFormation
YAML/JSON and deliver near-instant diagnostics.

## ✨ Goals (Phase 1)

The initial version is intentionally small and focused:

- **Extremely fast LSP** (written in Rust)
- **Local, per-file structural validation**
- **Parse CloudFormation YAML/JSON (L1 parsing)**
- **Minimal resource-reference graph (L2 structural IR)**
- **Immediate diagnostics inside VS Code**

This early work lays the foundation for more advanced analysis, without
introducing any higher-level semantics yet.

## 🚧 Current Status

The extension successfully starts a custom LSP server and returns basic test
diagnostics. Structural parsing and fast-path validation are being added.

This repository is being built over 45 days starting December 1, 2025.
I was going to do 30 days, but I'm sure crimbo etc will get in the way ;-)

## 🛠 How to build

### 1. Build the LSP server

```bash
cd server/stackgraph-lsp
cargo build --release
```

Ensure the resulting binary (stackgraph-lsp) is on your $PATH.

### 2. Run the extension
```bash
cd client/wa2
npm install
npm run compile    # or npm run watch, depending on your setup
```

Then press F5 in VS Code to launch the Extension Development Host.

## 🧩 Architecture

```
VS Code client (TypeScript)
      ⬇ LSP / stdio
Rust language server (tower-lsp)
      ⬇ parses
CloudFormation YAML/JSON (L1)
      ⬇ builds
Minimal structural IR (L2)
```

📦 License

This project is released under the **Apache 2.0 License**. See [LICENSE](LICENSE) for details.