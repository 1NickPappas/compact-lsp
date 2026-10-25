//! Unit tests for semantic-token encoding.

use super::*;
use compact_analyzer::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

fn token(
    line: u32,
    character: u32,
    length: u32,
    modifiers: Vec<SemanticTokenModifier>,
) -> SemanticToken {
    SemanticToken {
        range: Range {
            start: Position { line, character },
            end: Position {
                line,
                character: character + length,
            },
        },
        token_type: SemanticTokenType::Function,
        modifiers,
    }
}

#[test]
fn test_encode_semantic_tokens_is_delta_encoded() {
    let tokens = [
        token(0, 2, 3, vec![]),
        token(0, 10, 4, vec![]),
        token(2, 5, 1, vec![]),
    ];
    let data = encode_semantic_tokens(&tokens);

    assert_eq!(data.len(), 3);
    assert_eq!((data[0].delta_line, data[0].delta_start), (0, 2));
    assert_eq!((data[1].delta_line, data[1].delta_start), (0, 8));
    assert_eq!((data[2].delta_line, data[2].delta_start), (2, 5));
    assert_eq!(data[2].length, 1);
}

#[test]
fn test_encode_semantic_tokens_modifier_mask() {
    let tokens = [token(
        0,
        0,
        1,
        vec![
            SemanticTokenModifier::Declaration,
            SemanticTokenModifier::DefaultLibrary,
        ],
    )];
    let data = encode_semantic_tokens(&tokens);
    assert_eq!(data[0].token_modifiers_bitset, 0b101);
}

/// Out-of-order input must not underflow (debug builds panic on `0 - 1`).
#[test]
fn test_encode_semantic_tokens_out_of_order_does_not_panic() {
    let tokens = [token(3, 4, 1, vec![]), token(1, 0, 2, vec![])];
    let data = encode_semantic_tokens(&tokens);

    assert_eq!(data.len(), 2);
    assert_eq!(data[1].delta_line, 0, "backwards line delta saturates");
    assert_eq!(data[1].delta_start, 0, "backwards char delta saturates");
}

use proptest::prelude::*;

fn decode(data: &[lsp_types::SemanticToken]) -> Vec<(u32, u32)> {
    let mut line = 0u32;
    let mut character = 0u32;
    let mut positions = Vec::with_capacity(data.len());
    for entry in data {
        line += entry.delta_line;
        character = if entry.delta_line == 0 {
            character + entry.delta_start
        } else {
            entry.delta_start
        };
        positions.push((line, character));
    }
    positions
}

proptest! {
    /// Encoding arbitrary (even unsorted) tokens must never panic.
    #[test]
    fn prop_encode_tokens_never_panics(
        specs in prop::collection::vec((0u32..60, 0u32..60, 0u32..5), 0..30),
    ) {
        let tokens: Vec<SemanticToken> = specs
            .iter()
            .map(|&(line, character, length)| token(line, character, length, vec![]))
            .collect();
        let data = encode_semantic_tokens(&tokens);
        prop_assert_eq!(data.len(), specs.len());
    }

    /// Sorted tokens decode back to exactly the positions they came from.
    #[test]
    fn prop_sorted_tokens_roundtrip(
        mut specs in prop::collection::vec((0u32..60, 0u32..60, 0u32..5), 0..30),
    ) {
        specs.sort();
        let tokens: Vec<SemanticToken> = specs
            .iter()
            .map(|&(line, character, length)| token(line, character, length, vec![]))
            .collect();
        let decoded = decode(&encode_semantic_tokens(&tokens));
        let expected: Vec<(u32, u32)> = specs.iter().map(|&(line, character, _)| (line, character)).collect();
        prop_assert_eq!(decoded, expected);
    }
}
