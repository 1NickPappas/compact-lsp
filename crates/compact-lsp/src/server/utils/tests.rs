//! Unit tests for the utils module.

use super::*;

#[test]
fn test_get_word_at_position() {
    let content = "circuit add(a: Field, b: Field): Field {";
    assert_eq!(
        get_word_at_position(content, 0, 10),
        Some("add".to_string())
    );
    assert_eq!(
        get_word_at_position(content, 0, 0),
        Some("circuit".to_string())
    );
    assert_eq!(
        get_word_at_position(content, 0, 15),
        Some("Field".to_string())
    );
}

#[test]
fn test_get_word_at_position_start_of_line() {
    let content = "myFunction()";
    assert_eq!(
        get_word_at_position(content, 0, 0),
        Some("myFunction".to_string())
    );
}

#[test]
fn test_get_word_at_position_end_of_word() {
    let content = "myFunction()";
    assert_eq!(
        get_word_at_position(content, 0, 10),
        Some("myFunction".to_string())
    );
}

#[test]
fn test_get_word_at_position_empty() {
    let content = "a + b";
    assert_eq!(get_word_at_position(content, 0, 2), None);
}

#[test]
fn test_get_word_range_at_position() {
    let content = "circuit add(a: Field): Field {";
    let range = get_word_range_at_position(content, 0, 9).unwrap();
    assert_eq!(range.start.character, 8);
    assert_eq!(range.end.character, 11);
}

#[test]
fn test_parse_params_from_detail() {
    let detail = "(a: Field, b: Field): Field";
    let params = parse_params_from_detail(detail);
    assert_eq!(params, vec!["a: Field", "b: Field"]);
}

#[test]
fn test_parse_params_from_detail_empty() {
    let detail = "(): Void";
    let params = parse_params_from_detail(detail);
    assert!(params.is_empty());
}

#[test]
fn test_get_dot_access_variable() {
    // "round." with cursor at position 6 (after the dot)
    let content = "round.";
    assert_eq!(
        get_dot_access_variable(content, 0, 6),
        Some("round".to_string())
    );
}

#[test]
fn test_get_dot_access_variable_in_context() {
    let content = "    round.";
    assert_eq!(
        get_dot_access_variable(content, 0, 10),
        Some("round".to_string())
    );
}

#[test]
fn test_get_dot_access_variable_no_dot() {
    let content = "round";
    assert_eq!(get_dot_access_variable(content, 0, 5), None);
}

#[test]
fn test_get_dot_access_variable_no_identifier() {
    let content = ".";
    assert_eq!(get_dot_access_variable(content, 0, 1), None);
}

#[test]
fn test_detect_dot_context_right_after_dot() {
    // "round." with cursor at position 6 (after the dot)
    let content = "round.";
    assert_eq!(detect_dot_context(content, 0, 6), Some("round".to_string()));
}

#[test]
fn test_detect_dot_context_partial_identifier() {
    // "round.inc" with cursor at position 9 (after "inc")
    let content = "round.inc";
    assert_eq!(detect_dot_context(content, 0, 9), Some("round".to_string()));
}

#[test]
fn test_detect_dot_context_mid_partial() {
    // "round.increment" with cursor in middle of "increment"
    let content = "round.increment";
    assert_eq!(detect_dot_context(content, 0, 8), Some("round".to_string()));
}

#[test]
fn test_detect_dot_context_indented() {
    let content = "    round.inc";
    assert_eq!(
        detect_dot_context(content, 0, 13),
        Some("round".to_string())
    );
}

#[test]
fn test_detect_dot_context_no_dot() {
    let content = "round";
    assert_eq!(detect_dot_context(content, 0, 5), None);
}

#[test]
fn test_detect_dot_context_no_base() {
    let content = ".inc";
    assert_eq!(detect_dot_context(content, 0, 4), None);
}

#[test]
fn test_detect_dot_context_multiline() {
    let content = "ledger round: Counter;\nexport circuit incr(): [] {\n  round.increment(1);\n}";
    // Line 2: "  round.increment(1);" - cursor on "increment" at col 8
    assert_eq!(detect_dot_context(content, 2, 8), Some("round".to_string()));
}
