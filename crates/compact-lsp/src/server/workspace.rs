//! Workspace scanning and file discovery.

use std::io;

/// Recursively find all .compact files in a directory.
pub fn find_compact_files(root: &str) -> io::Result<Vec<String>> {
    let mut files = Vec::new();
    find_compact_files_recursive(root, &mut files)?;
    Ok(files)
}

fn find_compact_files_recursive(dir: &str, files: &mut Vec<String>) -> io::Result<()> {
    let entries = std::fs::read_dir(dir)?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Skip hidden directories and common non-source directories
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if !name.starts_with('.') && name != "node_modules" && name != "target" {
                find_compact_files_recursive(path.to_str().unwrap_or(""), files)?;
            }
        } else if path.extension().map(|e| e == "compact").unwrap_or(false) {
            if let Some(path_str) = path.to_str() {
                files.push(path_str.to_string());
            }
        }
    }

    Ok(())
}
