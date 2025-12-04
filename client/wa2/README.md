# WA2 — CloudFormation Fast Linting & Cross-Stack Foundations (Preview)

The **WA2 VS Code extension** provides an **ultra-fast, workspace-aware CloudFormation validator** powered by a Rust language server (`wa2lsp`).  
It performs real-time checks as you type — catching structural problems long before deployment.

This early preview focuses on the *fast-path engine*: instant YAML/JSON parsing and foundational template checks.

---

## ✨ Features (Preview)

- **Real-time diagnostics** for CloudFormation-style YAML & JSON  
- **Ultra-fast Rust LSP** (written in Rust, non-blocking design)  
- **Background validation** with strict time budgeting  
- **Parse error detection** for YAML/JSON  
- **CloudFormation structural checks**, including:
  - Missing top-level `Resources` section

More checks will roll out as the underlying engine develops.

---

## 🚀 Getting Started

### 1. Install the WA2 language server (`wa2lsp`)

You need Rust installed (`rustup`), then install the LSP from this repository:

```bash
git clone https://github.com/unremarkable-technology/wa2-vscode-extension
cd wa2-vscode-extension
cargo install --path server/wa2lsp
```

### 2. Install the WA2 VS Code extension

If you have a packaged `.vsix`, install it via:
VS Code → Extensions → … → Install from VSIX

Once installed:
* Open any CloudFormation-style YAML or JSON file.
* Type something invalid.
* Diagnostics should appear instantly.

# ⚙ Requirements
* VS Code ≥ 1.95
* Rust toolchain (stable)
* wa2lsp must be on your system PATH