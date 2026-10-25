//! Self-tests for the shared end-to-end harness helpers.

mod common;

use common::{apply_text_edit, apply_text_edits, apply_workspace_edit_to_disk, offset_of};
use serde_json::json;

#[test]
fn test_offset_of_ascii() {
    let src = "first\nsecond\nthird";
    assert_eq!(offset_of(src, 0, 0), 0);
    assert_eq!(offset_of(src, 0, 5), 5);
    assert_eq!(offset_of(src, 1, 0), 6);
    assert_eq!(offset_of(src, 1, 3), 9);
    assert_eq!(offset_of(src, 2, 5), src.len());
}

#[test]
fn test_offset_of_utf16() {
    // Each emoji is one char but two UTF-16 code units; `é` is one of each.
    let src = "a😀b\néx";
    assert_eq!(offset_of(src, 0, 0), 0, "before a");
    assert_eq!(offset_of(src, 0, 1), 1, "before the emoji");
    assert_eq!(offset_of(src, 0, 3), 5, "after the emoji (2 UTF-16 units)");
    assert_eq!(offset_of(src, 0, 4), 6, "at b");
    assert_eq!(offset_of(src, 1, 1), 9, "after é (2 UTF-8 bytes)");
}

#[test]
fn test_apply_text_edits_reverse_order() {
    let src = "alpha beta gamma";
    let edits = [
        json!({
            "range": {
                "start": { "line": 0, "character": 0 },
                "end": { "line": 0, "character": 5 }
            },
            "newText": "ALPHA"
        }),
        json!({
            "range": {
                "start": { "line": 0, "character": 11 },
                "end": { "line": 0, "character": 16 }
            },
            "newText": "GAMMA"
        }),
    ];
    assert_eq!(apply_text_edits(src, &edits), "ALPHA beta GAMMA");
}

#[test]
fn test_apply_text_edit_multiline_insert() {
    let src = "a\nc";
    let edit = json!({
        "range": {
            "start": { "line": 1, "character": 0 },
            "end": { "line": 1, "character": 0 }
        },
        "newText": "b\n"
    });
    assert_eq!(apply_text_edit(src, &edit), "a\nb\nc");
}

#[test]
fn test_apply_workspace_edit_to_disk() {
    let dir = tempfile::tempdir().expect("tempdir");
    let file = dir.path().join("edit.compact");
    std::fs::write(&file, "circuit old() {}\n").unwrap();
    let uri = format!("file://{}", file.display());

    let edit = json!({
        "changes": {
            uri.clone(): [{
                "range": {
                    "start": { "line": 0, "character": 8 },
                    "end": { "line": 0, "character": 11 }
                },
                "newText": "renamed"
            }]
        }
    });

    let updated = apply_workspace_edit_to_disk(&edit);
    assert_eq!(updated, vec![(uri, "circuit renamed() {}\n".to_string())]);
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "circuit renamed() {}\n"
    );
}

#[test]
fn test_apply_text_edits_accepts_did_change_text() {
    // `didChange` content changes use `text`, not the `newText` of a TextEdit.
    let base = "a\nb\n";
    let insert = json!({
        "range": {
            "start": { "line": 2, "character": 0 },
            "end": { "line": 2, "character": 0 }
        },
        "text": "c\n"
    });
    assert_eq!(
        apply_text_edits(base, std::slice::from_ref(&insert)),
        "a\nb\nc\n"
    );
}
