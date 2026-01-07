# WA2 — Fast CloudFormation Validation for VS Code

**WA2** provides ultra-fast, accurate CloudFormation template validation powered by a Rust language server (`wa2lsp`).

Real-time validation as you type — catch errors before deployment with **9× faster performance** than AWS Toolkit.

---

## ✨ Features

### Core Validation (71% Coverage)
- **Real-time diagnostics** for YAML & JSON CloudFormation templates
- **Resource type validation** - 1000+ AWS resource types from official schemas
- **Property validation** - Required properties, type checking, valid values
- **Intrinsic functions** - Full support for Ref, GetAtt, Sub, Join, Select, If, Equals, FindInMap, and 10+ more
- **Advanced features**:
  - AWS::LanguageExtensions (Fn::ForEach, Transform)
  - SAM & Serverless transform support
  - Custom resources and third-party types
  - Nested intrinsics and dot notation

### Smart Type Checking
- **CloudFormation coercion rules** - String→Number, Number→String (mirrors AWS behavior)
- **Lenient intrinsic types** - Handles Ref/GetAtt where runtime type is unknown
- **Special cases** - Stack.Outputs.*, Custom resources, Module types

### Developer Experience
- **Instant feedback** - Sub-second validation on large templates
- **Helpful suggestions** - "Did you mean X?" for typos
- **Accurate error locations** - Precise line/column diagnostics

---

## 🚀 Getting Started

### Installation

Install from VS Code Marketplace:
1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
3. Search for "WA2"
4. Click Install

**Or** install from `.vsix`:
```bash
code --install-extension wa2-*.vsix
```

### Usage

1. Open any CloudFormation template (`.yaml`, `.yml`, `.json`)
2. WA2 validates automatically as you type
3. Errors and warnings appear inline

**File detection**: WA2 validates files containing:
- `AWSTemplateFormatVersion` declaration, OR
- `Resources:` section

---

## 📊 Validation Coverage

**89% of valid CloudFormation templates pass validation**
- 71% overall (130/183 cfn-lint test fixtures)
- Handles standard CloudFormation + AWS::LanguageExtensions
- SAM/Serverless Transform support

**What's validated:**
- ✅ Resource types (AWS::S3::Bucket, AWS::Lambda::Function, etc.)
- ✅ Required properties
- ✅ Property types (String, Number, Boolean, Arrays, Objects)
- ✅ Intrinsic functions (all 16+ standard functions)
- ✅ Ref/GetAtt target existence
- ✅ Condition references
- ✅ Logical ID format
- ✅ FindInMap + Mappings section
- ✅ Rules section
- ✅ ForEach loops

**Not yet implemented:**
- ❌ Cross-stack references validation
- ❌ Property value patterns (regex validation)
- ❌ Semantic rules (NoEcho security, hardcoded ARNs)

---

## ⚡ Performance

**9× faster than AWS Toolkit** on typical templates:
- WA2: ~0.37 seconds
- AWS Toolkit: ~3.3 seconds

Powered by Rust + optimized parser architecture.

---

## 🛠️ Development

### Building from source
```bash
git clone https://github.com/unremarkable-technology/wa2-vscode-extension
cd wa2-vscode-extension

# Build LSP server
cd server/wa2lsp
cargo build --release

# Package extension
cd ../..
npm install
npx @vscode/vsce package
```

### Project Structure
```
wa2-vscode-extension/
├── server/wa2lsp/       # Rust LSP server
│   ├── src/
│   │   ├── spec/        # CloudFormation validation engine
│   │   └── lsp.rs       # LSP protocol implementation
│   └── Cargo.toml
├── client/              # VS Code extension (TypeScript)
└── package.json
```

---

## 🤝 Contributing

WA2 is in active development. Contributions welcome!

**Roadmap:**
- [ ] Well-Architected Framework rules
- [ ] Go-to-definition for Ref/GetAtt
- [ ] Hover documentation
- [ ] Multi-file/cross-stack validation
- [ ] CodeActions (quick fixes)

---

## 📝 License

[Your License]

---

## 🙋 Support

- **Issues**: [GitHub Issues](https://github.com/unremarkable-technology/wa2-vscode-extension/issues)
- **Discussions**: [GitHub Discussions](https://github.com/unremarkable-technology/wa2-vscode-extension/discussions)

---