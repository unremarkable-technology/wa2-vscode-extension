# WA2 — AWS Well-Architected Guidance for VS Code

**Real-time CloudFormation validation + Well-Architected best practices** as you build infrastructure. Get instant feedback on both syntax errors and architectural decisions.

Built by the creator of the AWS Well-Architected Framework.

---

## ✨ Features

### Well-Architected Guidance
- 🎯 **Intent-based validation** - Tag resources with sensitivity and criticality to unlock architectural guidance
- 📚 **Educational tooltips** - Understand the "why" behind best practices, not just the "what"
- ⚡ **Sub-second feedback** - No waiting for reviews or scans
- 🔄 **Context-aware** - Recommendations adapt to your declared intent

### Comprehensive CloudFormation Validation
- **1000+ AWS resource types** - Official CloudFormation Registry schemas
- **Property validation** - Required properties, types, allowed values  
- **All intrinsic functions** - Ref, GetAtt, Sub, Join, If, FindInMap, and 10+ more
- **AWS::LanguageExtensions** - Fn::ForEach, Transform support
- **SAM/Serverless** - Transform-based resources
- **Smart type checking** - Mirrors CloudFormation's coercion rules

### Developer Experience
- ⚡ **9× faster than AWS Toolkit** - 0.37s vs 3.3s validation time
- 🎯 **Accurate errors** - Precise line/column diagnostics
- 💡 **Helpful suggestions** - "Did you mean X?" for typos
- 🔍 **Go-to-definition** - Jump from Ref/GetAtt to resource definitions

---

## 🚀 Quick Start

1. **Install** - Search "WA2" in VS Code Extensions
2. **Open** - Any CloudFormation `.yaml`, `.yml`, or `.json` file
3. **Get guidance** - Errors, warnings, and architectural recommendations appear automatically

**Example**: Start with a simple S3 bucket. WA2 prompts you to tag it with DataSensitivity and DataCriticality. Once tagged, it recommends appropriate protections based on your intent.

---

## 📊 What's Validated

✅ CloudFormation syntax & resource types  
✅ Property requirements & type checking  
✅ Intrinsic functions (Ref, GetAtt, Sub, 13+ more)  
✅ Well-Architected principles (data protection, resilience)  
✅ Intent-based architectural guidance  

---

## 💡 Well-Architected in Action

**Without intent:**
```yaml
Resources:
  DataBucket:
    Type: AWS::S3::Bucket
```
❌ WA2: "Tag this resource for DataSensitivity and DataCriticality"

**With intent:**
```yaml
Resources:
  DataBucket:
    Type: AWS::S3::Bucket
    Properties:
      Tags:
        - Key: DataCriticality
          Value: BusinessCritical
```
💡 Hover for guidance: "Business critical data needs backup protection. Consider enabling versioning or cross-region replication."

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

**Built by the creator of the AWS Well-Architected Framework** • Shift left on architecture reviews