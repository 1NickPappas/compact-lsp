//! Unit tests for document state and rope edits.

use super::*;
use lsp_types::Position;

fn doc(text: &str) -> Document {
    Document {
        content: Rope::from_str(text),
        version: 1,
    }
}

fn range(start: (u32, u32), end: (u32, u32)) -> Range {
    Range {
        start: Position {
            line: start.0,
            character: start.1,
        },
        end: Position {
            line: end.0,
            character: end.1,
        },
    }
}

#[test]
fn test_apply_full_replacement() {
    let mut document = doc("old\ncontent\n");
    document.apply_change(None, "new");
    assert_eq!(document.content.to_string(), "new");
}

#[test]
fn test_apply_incremental_insert() {
    let mut document = doc("a\nb\n");
    document.apply_change(Some(&range((1, 0), (1, 0))), "x");
    assert_eq!(document.content.to_string(), "a\nxb\n");
}

#[test]
fn test_apply_incremental_delete() {
    let mut document = doc("alpha\nbeta\ngamma\n");
    // Delete the middle line, including its terminator.
    document.apply_change(Some(&range((1, 0), (2, 0))), "");
    assert_eq!(document.content.to_string(), "alpha\ngamma\n");
}

#[test]
fn test_apply_multi_line_replace() {
    let mut document = doc("one\ntwo\nthree\n");
    document.apply_change(Some(&range((0, 1), (2, 2))), "X");
    assert_eq!(document.content.to_string(), "oXree\n");
}

#[test]
fn test_apply_to_empty_document() {
    let mut document = doc("");
    document.apply_change(Some(&range((0, 0), (0, 0))), "first");
    assert_eq!(document.content.to_string(), "first");
}

#[test]
fn test_apply_change_keeps_other_state() {
    let mut document = doc("x");
    document.version = 7;
    document.apply_change(None, "y");
    assert_eq!(
        document.version, 7,
        "apply_change must not touch the version"
    );
}
