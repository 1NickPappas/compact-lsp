//! Unit tests for workspace file discovery.

use super::*;

fn touch(path: &std::path::Path) {
    std::fs::write(path, "").expect("write file");
}

#[test]
fn test_find_compact_files_discovers_nested() {
    let dir = tempfile::tempdir().expect("tempdir");
    touch(&dir.path().join("a.compact"));
    touch(&dir.path().join("notes.txt"));
    std::fs::create_dir(dir.path().join("nested")).expect("nested dir");
    touch(&dir.path().join("nested").join("c.compact"));

    let mut files = find_compact_files(dir.path().to_str().expect("path")).expect("discover");
    files.sort();
    assert_eq!(files.len(), 2, "{files:?}");
    assert!(files.iter().any(|f| f.ends_with("a.compact")), "{files:?}");
    assert!(files.iter().any(|f| f.ends_with("c.compact")), "{files:?}");
}

#[test]
fn test_find_compact_files_skips_hidden_and_build_dirs() {
    let dir = tempfile::tempdir().expect("tempdir");
    touch(&dir.path().join("visible.compact"));
    for skipped in [".git", "node_modules", "target"] {
        let sub = dir.path().join(skipped);
        std::fs::create_dir(&sub).expect("skipped dir");
        touch(&sub.join("hidden.compact"));
    }

    let files = find_compact_files(dir.path().to_str().expect("path")).expect("discover");
    assert_eq!(files.len(), 1, "{files:?}");
    assert!(files[0].ends_with("visible.compact"), "{files:?}");
}

#[test]
fn test_find_compact_files_missing_root_is_error() {
    assert!(find_compact_files("/definitely/not/a/real/directory-xyz").is_err());
}

/// Temporary compiler-input files live next to the source with a dot prefix;
/// they must never be indexed as workspace symbols or scanned as sources.
#[test]
fn test_find_compact_files_skips_hidden_files() {
    let dir = tempfile::tempdir().expect("tempdir");
    touch(&dir.path().join(".compact-lsp-temp-1234.compact"));

    let files = find_compact_files(dir.path().to_str().expect("path")).expect("discover");
    assert!(
        files.is_empty(),
        "dot-prefixed files must be skipped: {files:?}"
    );
}
