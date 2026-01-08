# WA2 — Fast CloudFormation Validation for VS Code

Ultra-fast, accurate CloudFormation template validation powered by Rust. **9× faster than AWS Toolkit** with real-time error detection as you type.

---

## ✨ Features

### Comprehensive Validation (71% Coverage)
- **1000+ AWS resource types** - Official CloudFormation schemas
- **Property validation** - Required properties, types, allowed values
- **All intrinsic functions** - Ref, GetAtt, Sub, Join, If, FindInMap, and 10+ more
- **AWS::LanguageExtensions** - Fn::ForEach, Transform support
- **SAM/Serverless** - Transform-based resources
- **Smart type checking** - Mirrors CloudFormation's coercion rules

### Developer Experience
- ⚡ **Instant feedback** - Sub-second validation on large templates
- 🎯 **Accurate errors** - Precise line/column diagnostics
- 💡 **Helpful suggestions** - "Did you mean X?" for typos
- 🚀 **9× faster** - 0.37s vs 3.3s (AWS Toolkit)

---

## 🚀 Quick Start

1. **Install** - Search "WA2" in VS Code Extensions
2. **Open** - Any CloudFormation `.yaml`, `.yml`, or `.json` file
3. **Validate** - Errors and warnings appear automatically

**File detection**: Validates files with `AWSTemplateFormatVersion` or `Resources:` section.

---

## 📊 What's Validated

✅ Resource types & properties  
✅ Intrinsic functions (16+ functions)  
✅ Ref/GetAtt targets  
✅ Type checking with CloudFormation coercion  
✅ Fn::ForEach loops  
✅ Transform requirements  

**89% of valid CloudFormation templates pass validation**

---

## ⚡ Performance
```
WA2:         0.37 seconds  ████
AWS Toolkit: 3.33 seconds  ████████████████████████████████████
```

**9× faster** on typical templates.

---

## 🛠️ Support

- **Issues**: [GitHub Issues](https://github.com/unremarkable-technology/wa2-vscode-extension/issues)
- **Source**: [GitHub](https://github.com/unremarkable-technology/wa2-vscode-extension)

---