//! Unit tests for the server module.

use super::*;

fn sym(name: &str, kind: CompletionSymbolKind) -> CompletionSymbol {
    CompletionSymbol {
        name: name.to_string(),
        kind,
        detail: None,
        location: Some(SymbolLocation {
            start_line: 1,
            start_char: 2,
            end_line: 1,
            end_char: 2 + name.len() as u32,
        }),
        documentation: None,
    }
}

#[test]
fn test_completion_kind_to_symbol_kind() {
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Function),
        SymbolKind::FUNCTION
    );
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Struct),
        SymbolKind::STRUCT
    );
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Enum),
        SymbolKind::ENUM
    );
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Variable),
        SymbolKind::VARIABLE
    );
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Module),
        SymbolKind::MODULE
    );
    assert_eq!(
        completion_kind_to_symbol_kind(CompletionSymbolKind::Contract),
        SymbolKind::CLASS
    );
}

#[test]
fn test_is_after_contract_implements() {
    let partial = "contract implements Tok";
    assert!(utils::is_after_contract_implements(
        partial,
        0,
        partial.len() as u32
    ));

    let complete = "contract implements Token;";
    assert!(!utils::is_after_contract_implements(
        complete,
        0,
        complete.len() as u32
    ));

    assert!(!utils::is_after_contract_implements("circuit f() {", 0, 5));
}

#[test]
fn test_symbol_location_to_range() {
    let range = symbol_location_to_range(&SymbolLocation {
        start_line: 3,
        start_char: 4,
        end_line: 3,
        end_char: 9,
    });
    assert_eq!(
        range.start,
        Position {
            line: 3,
            character: 4
        }
    );
    assert_eq!(
        range.end,
        Position {
            line: 3,
            character: 9
        }
    );
}

#[test]
fn test_symbol_matches_query() {
    assert!(symbol_matches_query("transferTokens", "")); // empty query matches all
    assert!(symbol_matches_query("transferTokens", "transfer"));
    assert!(symbol_matches_query("transferTokens", "tokens")); // case-insensitive substring
    assert!(!symbol_matches_query("transferTokens", "withdraw"));
}

#[test]
fn test_stdlib_hover_and_signature_help_gate_keccak() {
    let mainnet = Some(LanguageVersion::new(0, 23, 0));
    let newer = Some(LanguageVersion::new(0, 23, 103));

    // Gated symbol: hidden on the mainnet compiler, shown elsewhere.
    assert!(stdlib_circuit_hover_text("keccak256", mainnet).is_none());
    assert!(stdlib_signature_help("keccak256", 0, mainnet).is_none());
    assert!(stdlib_circuit_hover_text("keccak256", newer).is_some());
    assert!(stdlib_signature_help("keccak256", 0, newer).is_some());
    // Unknown compiler -> gating disabled.
    assert!(stdlib_circuit_hover_text("keccak256", None).is_some());

    // Stable circuit and struct remain available on the mainnet compiler.
    assert!(stdlib_circuit_hover_text("persistentHash", mainnet).is_some());
    assert!(stdlib_struct_hover_text("Maybe", mainnet).is_some());
    assert!(stdlib_struct_hover_text("Nope", mainnet).is_none());
}

#[test]
fn test_stdlib_completion_items_gate_keccak() {
    // The completion handler builds stdlib items through this helper, so
    // this test guards the actual wiring (not just the registry lookups).
    let mainnet = stdlib_completion_items(Some(LanguageVersion::new(0, 23, 0)));
    assert!(!mainnet.iter().any(|i| i.label == "keccak256"));
    assert!(mainnet.iter().any(|i| i.label == "persistentHash"));
    assert!(mainnet.iter().any(|i| i.label == "Maybe<T>"));

    // Unknown compiler -> gating disabled -> everything is offered.
    let unknown = stdlib_completion_items(None);
    assert!(unknown.iter().any(|i| i.label == "keccak256"));
}

#[test]
fn test_workspace_symbol_filter_and_map() {
    let symbols = [
        sym("addFunds", CompletionSymbolKind::Function),
        sym("Point", CompletionSymbolKind::Struct),
        sym("Color", CompletionSymbolKind::Enum),
    ];

    // Replicates the handler's per-symbol filter + map for query "add".
    let mapped: Vec<_> = symbols
        .iter()
        .filter(|s| symbol_matches_query(&s.name, "add"))
        .filter_map(|s| {
            s.location.as_ref().map(|loc| {
                (
                    s.name.clone(),
                    completion_kind_to_symbol_kind(s.kind),
                    symbol_location_to_range(loc),
                )
            })
        })
        .collect();

    assert_eq!(mapped.len(), 1);
    assert_eq!(mapped[0].0, "addFunds");
    assert_eq!(mapped[0].1, SymbolKind::FUNCTION);
}

/// Apply line-level edits (to verify a diff reconstructs its target).
fn apply_line_edits(original: &str, edits: &[LineEdit]) -> String {
    let mut lines: Vec<String> = original.split('\n').map(str::to_string).collect();
    for e in edits.iter().rev() {
        lines.splice(e.start..e.end, e.replacement.clone());
    }
    lines.join("\n")
}

/// Apply LSP text edits (to verify range-formatting output).
fn apply_text_edits(original: &str, edits: &[TextEdit]) -> String {
    let mut lines: Vec<String> = original.split('\n').map(str::to_string).collect();
    for e in edits.iter().rev() {
        let repl: Vec<String> = if e.new_text.is_empty() {
            Vec::new()
        } else {
            e.new_text
                .strip_suffix('\n')
                .unwrap_or(&e.new_text)
                .split('\n')
                .map(str::to_string)
                .collect()
        };
        lines.splice(e.range.start.line as usize..e.range.end.line as usize, repl);
    }
    lines.join("\n")
}

const ORIGINAL: &str = "circuit a(): Field {\n      return   1;\n}\ncircuit b():Field{return 2;}\n";
const FORMATTED: &str =
    "circuit a(): Field {\n  return 1;\n}\n\ncircuit b(): Field {\n  return 2;\n}\n";

#[test]
fn test_line_edits_roundtrip() {
    let orig: Vec<&str> = ORIGINAL.split('\n').collect();
    let fmt: Vec<&str> = FORMATTED.split('\n').collect();
    let edits = line_edits(&orig, &fmt);
    assert!(!edits.is_empty());
    assert_eq!(apply_line_edits(ORIGINAL, &edits), FORMATTED);
}

#[test]
fn test_range_format_edits_full_selection_matches_formatted() {
    let edits = range_format_edits(ORIGINAL, FORMATTED, 0, u32::MAX);
    assert_eq!(apply_text_edits(ORIGINAL, &edits), FORMATTED);
}

#[test]
fn test_range_format_edits_narrow_selection() {
    // Only line 1 (the `return 1;` line) is selected: the second circuit
    // must be left exactly as it was.
    let edits = range_format_edits(ORIGINAL, FORMATTED, 1, 1);
    assert_eq!(edits.len(), 1, "expected a single in-range edit: {edits:?}");
    assert_eq!(
        apply_text_edits(ORIGINAL, &edits),
        "circuit a(): Field {\n  return 1;\n}\ncircuit b():Field{return 2;}\n"
    );
}

#[test]
fn test_range_format_edits_outside_changes_is_empty() {
    // Line 0 is unchanged, so there is nothing to do.
    let edits = range_format_edits(ORIGINAL, FORMATTED, 0, 0);
    assert!(edits.is_empty(), "unexpected edits: {edits:?}");
}
