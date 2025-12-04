
# WA2 VS Code Extension (Early Preview)

The **WA2 VS Code extension** provides **fast, workspace-aware validation**
for CloudFormation templates inside VS Code, the vision being **an ultra-fast architecture guide inside your IDE**.

This is an **early open-source prototype** focused on Phase 1 of the WA2
project: a high-performance language server (LSP) that can analyse CloudFormation
YAML/JSON and deliver near-instant diagnostics. Rust is used for the language server to give low-latency validation even on very large CloudFormation workspaces - we intend to use
time-budgets to ensure fast response, with any heavy work done only on idle.

## Preview
![Markdown Logo](assets/images/wa2.screenshot.png)

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

The extension launches a custom Rust LSP (`wa2lsp`) and returns real-time diagnostics.  
Initial structural checks — including YAML/JSON parsing and basic CloudFormation shape validation — are implemented.

This repository is being built over **45 days**, starting December 1, 2025.  
(Originally 30 days… but let’s be honest, December is December! 😊)

## Supported file types

Phase 1 focuses on CloudFormation templates written in:

- `.yaml`/`.yml`
- `.json`

## 🛠 How to build

### 1. Build the LSP server

From the repository root:

```bash
git clone https://github.com/unremarkable-technology/wa2-vscode-extension
cd wa2-vscode-extension
cargo install --path server/wa2lsp
```

This installs the wa2lsp binary into your Cargo bin directory (typically ~/.cargo/bin), which should already be on your PATH.

Verify:

```bash
wa2lsp --version
```

Ensure the resulting binary (wa2lsp) is on your $PATH.

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
The LSP executes fast-path analysis under strict time budgets, ensuring the editor stays snappy.

## 📦 License

This project is released under the **Apache 2.0 License**. See [LICENSE](LICENSE) for details.