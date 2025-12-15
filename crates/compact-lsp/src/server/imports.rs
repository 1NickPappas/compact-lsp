//! Import resolution and cross-file symbol lookup.

use std::path::Path;

/// Resolve an import path relative to the current file.
///
/// Converts relative import paths like "../utils/Utils" to absolute file URIs.
pub fn resolve_import_path(current_uri: &str, import_path: &str) -> Option<String> {
    // Get the directory of the current file
    let current_path = current_uri.strip_prefix("file://")?;
    let current_dir = Path::new(current_path).parent()?;

    // Resolve the relative import path
    let import_with_ext = if import_path.ends_with(".compact") {
        import_path.to_string()
    } else {
        format!("{}.compact", import_path)
    };

    let resolved = current_dir.join(&import_with_ext);
    let normalized = normalize_path(&resolved)?;

    // Return as file:// URI
    Some(format!("file://{}", normalized))
}

/// Normalize a path by resolving .. and . components.
pub fn normalize_path(path: &Path) -> Option<String> {
    // Use canonicalize if the file exists, otherwise do manual normalization
    if path.exists() {
        path.canonicalize().ok()?.to_str().map(|s| s.to_string())
    } else {
        // Manual normalization for non-existent paths
        let mut components = Vec::new();
        for component in path.components() {
            match component {
                std::path::Component::ParentDir => {
                    components.pop();
                }
                std::path::Component::CurDir => {}
                _ => {
                    components.push(component);
                }
            }
        }
        let normalized: std::path::PathBuf = components.iter().collect();
        normalized.to_str().map(|s| s.to_string())
    }
}
