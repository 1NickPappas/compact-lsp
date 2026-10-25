//! Deterministic mutation fuzzing.
//!
//! A seeded LCG mutates a small corpus of Compact sources (structural
//! characters, non-ASCII, truncation, insertion) and every parser entry point
//! is called on the result. The assertion is simply that nothing panics:
//! tree-sitter recovery and the AST walks must tolerate arbitrary input.
//!
//! Fully deterministic — no external fuzzing dependency and no flakes.

use super::*;

const CORPUS: &[&str] = &[
    "import CompactStandardLibrary;\n\nexport struct S {\n  a: Uint<8>,\n  b: Field,\n}\n\nexport circuit f(s: S): Uint<8> {\n  return s.a;\n}\n",
    "module m {\n  export circuit g<T>(x: T): T { return x; }\n}\nimport \"./m\";\n",
    "contract C {\n  circuit c(x: Uint<64>): Boolean;\n}\n\nexport circuit useIt(ref: C): Boolean {\n  return ref.c(disclose(1));\n}\n",
    "enum E { a, b }\nexport ledger l: Counter;\nexport witness w(): Field;\nexport circuit h(e: E): Uint<8> {\n  return e == E.a ? 0 : 1;\n}\n",
    "type Amount = Uint<16>;\nnew type Cents = Uint<16>;\nexport circuit i<A, B>(a: A, b: B): [A, B] { return [a, b]; }\n",
];

const CHAOS: [char; 14] = [
    '{', '}', '(', ')', '[', ']', '<', '>', '"', '/', '\n', ';', ':', ',',
];
const WIDE: [char; 3] = ['😀', '日', 'é'];

struct Lcg(u64);

impl Lcg {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.0 >> 33
    }
}

fn mutate(source: &str, rng: &mut Lcg) -> String {
    let mut chars: Vec<char> = source.chars().collect();
    if chars.is_empty() {
        return String::new();
    }
    let index = (rng.next() as usize) % chars.len();
    match rng.next() % 4 {
        0 => chars[index] = CHAOS[(rng.next() as usize) % CHAOS.len()],
        1 => chars[index] = WIDE[(rng.next() as usize) % WIDE.len()],
        2 => chars.truncate(index.max(1)),
        _ => chars.insert(index, CHAOS[(rng.next() as usize) % CHAOS.len()]),
    }
    // Keep the corpus bounded so a long chain of insertions stays fast.
    if chars.len() > 2_000 {
        chars.truncate(2_000);
    }
    chars.into_iter().collect()
}

/// Call every parser entry point on `source`; none may panic.
fn exercise(parser: &mut ParserEngine, source: &str) {
    let _ = parser.get_syntax_errors(source);
    let _ = parser.document_symbols(source);
    let _ = parser.folding_ranges(source);
    let _ = parser.get_completion_symbols(source);
    let _ = parser.get_imports(source);
    let _ = parser.get_semantic_tokens(source);
    let _ = parser.find_references(source, "x");

    for (line, character) in [(0, 0), (1, 5), (3, 3), (10, 10)] {
        let _ = parser.hover_info(source, line, character);
        let _ = parser.goto_definition(source, line, character);
        let _ = parser.signature_help(source, line, character);
        let _ = parser.get_variable_type(source, "x");
        let _ = parser.get_variable_type_raw(source, "s");
        let _ = parser.get_member_access_context(source, line, character);
        let _ = parser.circuit_at_position(source, line, character);
        let _ = parser.type_member_at_position(source, line, character);
    }
}

#[test]
fn test_parser_never_panics_on_mutated_sources() {
    let mut rng = Lcg(0x5eed_5eed);
    let mut parser = ParserEngine::new();

    for base in CORPUS {
        let mut source = (*base).to_string();
        for _ in 0..250 {
            source = mutate(&source, &mut rng);
            exercise(&mut parser, &source);
        }
    }
}
