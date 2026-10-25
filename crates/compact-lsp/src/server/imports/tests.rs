//! Unit tests for import-path resolution.

use super::*;

#[test]
fn test_resolve_import_path_relative() {
    let parent = tempfile::tempdir().expect("tempdir");
    let dir = parent.path().join("sub");
    std::fs::create_dir(&dir).expect("sub dir");
    let uri = format!("file://{}", dir.join("main.compact").display());

    // `./util` gains the `.compact` extension.
    assert_eq!(
        resolve_import_path(&uri, "./util").as_deref(),
        Some(format!("file://{}", dir.join("util.compact").display()).as_str())
    );

    // `../top` is normalized against the importing file's directory.
    assert_eq!(
        resolve_import_path(&uri, "../top").as_deref(),
        Some(format!("file://{}", parent.path().join("top.compact").display()).as_str())
    );

    // An explicit extension is not doubled.
    assert_eq!(
        resolve_import_path(&uri, "./util.compact").as_deref(),
        Some(format!("file://{}", dir.join("util.compact").display()).as_str())
    );
}

#[test]
fn test_resolve_import_path_existing_file_is_canonical() {
    let dir = tempfile::tempdir().expect("tempdir");
    let lib = dir.path().join("lib.compact");
    std::fs::write(&lib, "").expect("write lib");
    let uri = format!("file://{}", dir.path().join("main.compact").display());

    let resolved = resolve_import_path(&uri, "./lib").expect("resolve");
    let canonical = std::fs::canonicalize(&lib).expect("canonicalize");
    assert_eq!(resolved, format!("file://{}", canonical.display()));
}

#[test]
fn test_resolve_import_path_non_file_uri() {
    assert!(resolve_import_path("untitled:Untitled-1", "./util").is_none());
}

#[test]
fn test_normalize_path_unknown_file_keeps_physical_path() {
    let dir = tempfile::tempdir().expect("tempdir");
    let missing = dir.path().join("missing").join("..").join("lib.compact");

    assert_eq!(
        normalize_path(&missing).as_deref(),
        Some(dir.path().join("lib.compact").to_str().expect("path"))
    );
}
