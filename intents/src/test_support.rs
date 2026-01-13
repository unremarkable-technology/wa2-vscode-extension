use std::sync::{Arc, OnceLock};

use wa2lsp::spec::{spec_store::SpecStore, spec_source::SpecSource, spec_cache::SpecCacheManager};

/// Loads the real CFN spec (blocking). Cached after first call.
/// Panics if spec can't be loaded.
pub fn use_latest_cfn_spec() -> Arc<SpecStore> {
	static CACHED_SPEC: OnceLock<Arc<SpecStore>> = OnceLock::new();

	CACHED_SPEC
		.get_or_init(|| {
			let rt = tokio::runtime::Runtime::new().expect("tokio runtime");
			rt.block_on(async {


				let source = SpecSource::for_region_schemas("us-east-1").expect("spec source");
				let cache_dir = dirs::cache_dir()
					.unwrap_or_else(std::env::temp_dir)
					.join("wa2")
					.join("cfn-spec");
				let manager = SpecCacheManager::new(source, &cache_dir);
				manager.load_registry_spec_store().await.expect("load spec")
			})
		})
		.clone()
}