// server/wa2lsp/src/spec/intrinsics.rs
//! Declarative registry of CloudFormation intrinsic functions

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicKind {
	Ref,
	GetAtt,
	Sub,
}

/// Definition of an intrinsic function
#[derive(Debug)]
pub struct IntrinsicDef {
	pub kind: IntrinsicKind,
	pub yaml_tag: &'static str,
	pub json_key: &'static str,
}

/// Global registry of all intrinsics
pub static INTRINSICS: &[IntrinsicDef] = &[
	IntrinsicDef {
		kind: IntrinsicKind::Ref,
		yaml_tag: "Ref",
		json_key: "Ref",
	},
	IntrinsicDef {
		kind: IntrinsicKind::GetAtt,
		yaml_tag: "GetAtt",
		json_key: "Fn::GetAtt",
	},
	IntrinsicDef {
		kind: IntrinsicKind::Sub,
		yaml_tag: "Sub",
		json_key: "Fn::Sub",
	},
];

/// Look up intrinsic by YAML tag
pub fn get_intrinsic_by_tag(tag: &str) -> Option<&'static IntrinsicDef> {
	INTRINSICS.iter().find(|i| i.yaml_tag == tag)
}

/// Look up intrinsic by JSON key
pub fn get_intrinsic_by_json_key(key: &str) -> Option<&'static IntrinsicDef> {
	INTRINSICS.iter().find(|i| i.json_key == key)
}
