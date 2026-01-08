# WA2 — Fast CloudFormation Validation for VS Code

Ultra-fast CloudFormation validation powered by Rust. A VS Code extension with 71% test coverage (89% on valid templates) that's **9× faster than AWS Toolkit**.

[![VS Code Marketplace](https://img.shields.io/visual-studio-marketplace/v/FigmentEngineLtd.wa2)](https://marketplace.visualstudio.com/items?itemName=FigmentEngineLtd.wa2)
[![GitHub Release](https://img.shields.io/github/v/release/unremarkable-technology/wa2-vscode-extension)](https://github.com/unremarkable-technology/wa2-vscode-extension/releases)

---

## 🎯 Vision

WA2 is being built in three phases:

1. **Syntax** ✅ (Current - Days 1-38) - Parse CloudFormation, validate structure, resource types, and properties
2. **Semantics** (Next) - Cross-resource relationships, dependencies, runtime behavior
3. **Intent** (Goal) - Well-Architected Framework rules and architectural best practices

Built over 45 days as an open-source project.

---

## ✨ Features (Day 38)

### Comprehensive Validation (71% Coverage)
- 1000+ AWS resource types from official CloudFormation schemas
- All 16+ intrinsic functions (Ref, GetAtt, Sub, Join, If, FindInMap, etc.)
- AWS::LanguageExtensions (Fn::ForEach, Transform)
- SAM/Serverless transform support
- Smart type checking mirroring CloudFormation's coercion rules
- Custom resources and third-party types

### Developer Experience
- ⚡ Sub-second validation on large templates
- 🎯 Precise error locations with helpful suggestions
- 🚀 9× faster than AWS Toolkit (0.37s vs 3.3s)

**89% of valid CloudFormation templates pass validation** (130/183 cfn-lint fixtures)

---

## 🚀 Installation

### For Users

Install from [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=FigmentEngineLtd.wa2):

1. Open VS Code
2. Search "WA2" in Extensions
3. Click Install

### For Developers
```bash
# Clone repository
git clone https://github.com/unremarkable-technology/wa2-vscode-extension
cd wa2-vscode-extension

# Build LSP server
cd server/wa2lsp
cargo build --release

# Build extension
cd ../../client/wa2
npm install
npm run compile

# Package
npx @vscode/vsce package
```

---

## 🏗️ Architecture
```
VS Code Extension (TypeScript)
      ↓ LSP Protocol
Rust Language Server (wa2lsp)
      ↓ Parses with saphyr/jsonc-parser
CloudFormation YAML/JSON
      ↓ Builds IR
Intermediate Representation
      ↓ Validates against
AWS CloudFormation Schemas
      ↓ Produces
Diagnostics + Suggestions
```

### Key Technologies
- **tower-lsp** - LSP protocol framework
- **saphyr** - YAML parsing with position tracking
- **jsonc-parser** - JSON parsing with comments
- **CloudFormation Registry** - Official AWS resource schemas

---

## 📊 Current Coverage

**What's Validated:**
- ✅ Resource types (AWS::S3::Bucket, AWS::Lambda::Function, etc.)
- ✅ Required properties
- ✅ Property types (String, Number, Boolean, Arrays, Objects)
- ✅ All intrinsic functions (Ref, GetAtt, Sub, Join, Select, If, etc.)
- ✅ Ref/GetAtt target existence
- ✅ Condition references
- ✅ Logical ID format
- ✅ FindInMap + Mappings section
- ✅ Fn::ForEach loops
- ✅ Transform requirements

**Not Yet Implemented:**
- ❌ Cross-stack references
- ❌ Well-Architected best practices
- ❌ Property value regex patterns
- ❌ Advanced semantic rules

---

## 🤝 Contributing

Contributions welcome! This is an active open-source project.

**Development workflow:**
```bash
# Build server
cd server/wa2lsp && cargo build --release

# Run tests
cargo test

# Package extension
cd ../../client/wa2
npm run compile
npx @vscode/vsce package
```

**Roadmap:**
- [ ] Well-Architected Framework rules
- [ ] Go-to-definition for Ref/GetAtt
- [ ] Hover documentation
- [ ] Multi-file/cross-stack validation
- [ ] CodeActions (quick fixes)

---

## 📝 License

Apache 2.0 License - See [LICENSE](LICENSE)

---

## 🙋 Support

- **Issues**: [GitHub Issues](https://github.com/unremarkable-technology/wa2-vscode-extension/issues)
- **Discussions**: [GitHub Discussions](https://github.com/unremarkable-technology/wa2-vscode-extension/discussions)

---