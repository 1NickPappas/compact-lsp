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
