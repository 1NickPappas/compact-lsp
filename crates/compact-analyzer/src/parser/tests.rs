//! Unit tests for the parser module.

use super::*;

#[test]
fn test_parse_circuit() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}
"#;
    let symbols = parser.document_symbols(source);
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0].name, "add");
    assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
}

#[test]
fn test_parse_struct() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Point {
    x: Field;
    y: Field;
}
"#;
    let symbols = parser.document_symbols(source);
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0].name, "Point");
    assert_eq!(symbols[0].kind, SymbolKind::STRUCT);
}

#[test]
fn test_folding_ranges() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit test(): Field {
    const x = 1;
    return x;
}
"#;
    let ranges = parser.folding_ranges(source);
    assert!(!ranges.is_empty());
}

#[test]
fn test_hover_keyword() {
    let mut parser = ParserEngine::new();
    let source = "circuit test(): Field { return 1; }";
    // Hover on "circuit" keyword at position (0, 0)
    let info = parser.hover_info(source, 0, 0);
    assert!(info.is_some());
    let info = info.unwrap();
    assert!(info.content.contains("circuit"));
}

#[test]
fn test_hover_builtin_type() {
    let mut parser = ParserEngine::new();
    let source = "circuit test(): Field { return 1; }";
    // Hover on "Field" type at position (0, 16)
    let info = parser.hover_info(source, 0, 16);
    assert!(info.is_some());
    let info = info.unwrap();
    assert!(info.content.contains("Field"));
}

#[test]
fn test_hover_circuit_definition() {
    let mut parser = ParserEngine::new();
    let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
    // Hover on "add" at position (0, 8)
    let info = parser.hover_info(source, 0, 8);
    assert!(info.is_some());
    let info = info.unwrap();
    assert!(info.content.contains("add"));
    assert!(info.content.contains("Circuit function"));
}

#[test]
fn test_goto_definition_circuit() {
    let mut parser = ParserEngine::new();
    let source = r#"circuit helper(): Field { return 1; }
circuit main(): Field { return helper(); }"#;
    // Go to definition of "helper" in main (line 1, around column 32)
    let loc = parser.goto_definition(source, 1, 32);
    assert!(loc.is_some());
    let loc = loc.unwrap();
    // Should point to line 0 where helper is defined
    assert_eq!(loc.selection_range.start.line, 0);
}

#[test]
fn test_goto_definition_struct() {
    let mut parser = ParserEngine::new();
    let source = r#"struct Point { x: Field; y: Field; }
circuit make_point(): Point { return Point { x: 0, y: 0 }; }"#;
    // Go to definition of "Point" in make_point return type (line 1, around column 22)
    let loc = parser.goto_definition(source, 1, 22);
    assert!(loc.is_some());
    let loc = loc.unwrap();
    // Should point to line 0 where Point is defined
    assert_eq!(loc.selection_range.start.line, 0);
}

#[test]
fn test_signature_help() {
    let mut parser = ParserEngine::new();
    let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(1, 2);
}"#;
    // Position inside add() call - after the opening paren (line 5, col 15)
    let info = parser.signature_help(source, 5, 15);
    assert!(info.is_some(), "Should find signature help");
    let info = info.unwrap();
    assert!(
        info.label.contains("add"),
        "Label should contain function name"
    );
    assert_eq!(info.parameters.len(), 2, "Should have 2 parameters");
    assert_eq!(info.active_parameter, 0, "First parameter should be active");
}

#[test]
fn test_signature_help_second_param() {
    let mut parser = ParserEngine::new();
    let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(1, 2);
}"#;
    // Position after the comma (line 5, col 18)
    let info = parser.signature_help(source, 5, 18);
    assert!(info.is_some(), "Should find signature help");
    let info = info.unwrap();
    assert_eq!(
        info.active_parameter, 1,
        "Second parameter should be active"
    );
}

#[test]
fn test_signature_help_incomplete() {
    let mut parser = ParserEngine::new();
    // Incomplete code - user is still typing
    let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(
}"#;
    // Position right after opening paren (line 5, col 15)
    let info = parser.signature_help(source, 5, 15);
    assert!(
        info.is_some(),
        "Should find signature help for incomplete call"
    );
    let info = info.unwrap();
    assert!(
        info.label.contains("add"),
        "Label should contain function name"
    );
}

#[test]
fn test_signature_help_third_param() {
    let mut parser = ParserEngine::new();
    let source = r#"circuit add3(a: Field, b: Field, c: Field): Field {
    return a + b + c;
}

circuit main(): Field {
    return add3(1, 2, 3);
}"#;
    // Position after the second comma (line 5, col 21)
    let info = parser.signature_help(source, 5, 21);
    assert!(info.is_some(), "Should find signature help");
    let info = info.unwrap();
    assert_eq!(info.parameters.len(), 3, "Should have 3 parameters");
    assert_eq!(info.active_parameter, 2, "Third parameter should be active");
}

#[test]
fn test_const_binding_semantic_token() {
    let mut parser = ParserEngine::new();
    let source = r#"circuit f(): Field {
    const x = 1;
    return x;
}"#;
    let tokens = parser.get_semantic_tokens(source);

    // The `const` binding `x` must be reported as a readonly variable
    // declaration. The grammar node is `const_stmt` (not `const_binding`),
    // so this regresses if the match arm references the wrong node kind.
    let var_decl = tokens.iter().find(|t| {
        t.token_type == SemanticTokenType::Variable
            && t.modifiers.contains(&SemanticTokenModifier::Declaration)
            && t.modifiers.contains(&SemanticTokenModifier::Readonly)
    });
    assert!(
        var_decl.is_some(),
        "const binding should produce a readonly Variable declaration token"
    );
}

#[test]
fn test_circuit_hover_includes_return_type() {
    let mut parser = ParserEngine::new();
    let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
    // Hover on "add" at position (0, 8).
    let info = parser.hover_info(source, 0, 8).expect("hover on circuit");
    // The return type comes from the `type` field; if the code reads a
    // non-existent `rtype` field the signature ends with ": " (empty).
    assert!(
        info.content.contains("): Field"),
        "circuit hover should include the return type, got: {}",
        info.content
    );
}

#[test]
fn test_parses_language_0_23_syntax() {
    let mut parser = ParserEngine::new();
    // Constructs that only the 0.23-capable grammar parses: block comment,
    // selective import, `new type` alias, single-quoted Opaque tag,
    // multi-binding const with hex/octal literals, and slice<>.
    let source = r#"/* block comment */
import { Maybe } from CompactStandardLibrary;
export new type Tag = Opaque<'mytag'>;
export circuit demo(data: Bytes<32>): Bytes<16> {
    const a = 0xFF, b = 0o17;
    return slice<16>(data, 0);
}"#;
    let errors = parser.get_syntax_errors(source);
    assert!(
        errors.is_empty(),
        "0.23 syntax should parse without errors, got: {:?}",
        errors
    );
    // The circuit is still discoverable as a symbol.
    let symbols = parser.get_completion_symbols(source);
    assert!(
        symbols
            .iter()
            .any(|s| s.name == "demo" && s.kind == CompletionSymbolKind::Function)
    );
    // The `new type` alias appears in the document-symbol outline.
    let doc_symbols = parser.document_symbols(source);
    assert!(
        doc_symbols.iter().any(|s| s.name == "Tag"),
        "type alias should appear in the outline"
    );
}

/// Validation step for grammar bumps: every node kind the parser compares
/// with `.kind()` (and every `child_by_field_name`) must exist in the linked
/// `tree-sitter-compact` schema. Fails loudly if a grammar rename/removal
/// slips in, instead of silently degrading editor features.
///
/// A few **legacy** node kinds are intentionally kept so the parser works
/// with both the old and new grammar schemes; they are listed below.
#[test]
fn test_parser_references_exist_in_grammar_schema() {
    use std::collections::HashSet;

    // Names kept for the older grammar scheme; not in the current schema.
    const LEGACY_NODE_KINDS: &[&str] = &["cbinding", "type_decl"];

    let schema: serde_json::Value =
        serde_json::from_str(tree_sitter_compact::NODE_TYPES).expect("valid node-types.json");
    let nodes = schema.as_array().expect("node-types.json is an array");

    let named: HashSet<&str> = nodes
        .iter()
        .filter_map(|n| n.get("type").and_then(|t| t.as_str()))
        .collect();
    let mut fields: HashSet<&str> = HashSet::new();
    for n in nodes {
        if let Some(obj) = n.get("fields").and_then(|f| f.as_object()) {
            fields.extend(obj.keys().map(String::as_str));
        }
    }

    let src = include_str!("mod.rs");

    // Node kinds compared with `.kind() ==` or `matches!(... .kind(), …)`.
    let re_eq = regex::Regex::new(r#"\.kind\(\)\s*==\s*"([a-z_][a-z0-9_]*)""#).unwrap();
    let re_matches = regex::Regex::new(r"(?s)matches!\([^;]*?\.kind\(\),\s*([^)]*)\)").unwrap();
    let re_quoted = regex::Regex::new(r#""([a-z_][a-z0-9_]*)""#).unwrap();

    let mut kinds: HashSet<String> = re_eq.captures_iter(src).map(|c| c[1].to_string()).collect();
    for cap in re_matches.captures_iter(src) {
        kinds.extend(re_quoted.captures_iter(&cap[1]).map(|c| c[1].to_string()));
    }

    let missing_kinds: Vec<&String> = kinds
        .iter()
        .filter(|k| !named.contains(k.as_str()) && !LEGACY_NODE_KINDS.contains(&k.as_str()))
        .collect();
    assert!(
        missing_kinds.is_empty(),
        "parser references node kinds absent from the grammar schema: {missing_kinds:?}"
    );

    // Field names read via `child_by_field_name(...)`.
    let re_field = regex::Regex::new(r#"child_by_field_name\("([a-z_][a-z0-9_]*)"\)"#).unwrap();
    let field_refs: HashSet<String> = re_field
        .captures_iter(src)
        .map(|c| c[1].to_string())
        .collect();
    let missing_fields: Vec<&String> = field_refs
        .iter()
        .filter(|f| !fields.contains(f.as_str()))
        .collect();
    assert!(
        missing_fields.is_empty(),
        "parser references fields absent from the grammar schema: {missing_fields:?}"
    );
}

#[test]
fn test_completion_symbols() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

struct Point {
    x: Field;
    y: Field;
}

enum Color {
    Red,
    Green,
    Blue,
}
"#;
    let symbols = parser.get_completion_symbols(source);

    // Should find circuit, struct, and enum
    assert!(
        symbols
            .iter()
            .any(|s| s.name == "add" && s.kind == CompletionSymbolKind::Function)
    );
    assert!(
        symbols
            .iter()
            .any(|s| s.name == "Point" && s.kind == CompletionSymbolKind::Struct)
    );
    assert!(
        symbols
            .iter()
            .any(|s| s.name == "Color" && s.kind == CompletionSymbolKind::Enum)
    );
}

#[test]
fn test_get_imports() {
    let mut parser = ParserEngine::new();
    let source = r#"
import CompactStandardLibrary;
import "../utils/Utils" prefix Utils_;
import "../security/Initializable" prefix Init_;
import "no_prefix_file";

circuit main(): Field {
    return 1;
}
"#;
    let imports = parser.get_imports(source);

    // Should find all 4 imports
    assert_eq!(imports.len(), 4, "Should find 4 imports");

    // Standard library import (no prefix, not a file)
    let stdlib = imports.iter().find(|i| i.path == "CompactStandardLibrary");
    assert!(
        stdlib.is_some(),
        "Should find CompactStandardLibrary import"
    );
    let stdlib = stdlib.unwrap();
    assert!(!stdlib.is_file, "Should not be a file import");
    assert!(stdlib.prefix.is_none(), "Should have no prefix");

    // Utils import with prefix
    let utils = imports.iter().find(|i| i.path == "../utils/Utils");
    assert!(utils.is_some(), "Should find Utils import");
    let utils = utils.unwrap();
    assert!(utils.is_file, "Should be a file import");
    assert_eq!(
        utils.prefix.as_deref(),
        Some("Utils_"),
        "Should have Utils_ prefix"
    );

    // Initializable import with prefix
    let init = imports
        .iter()
        .find(|i| i.path == "../security/Initializable");
    assert!(init.is_some(), "Should find Initializable import");
    let init = init.unwrap();
    assert!(init.is_file, "Should be a file import");
    assert_eq!(
        init.prefix.as_deref(),
        Some("Init_"),
        "Should have Init_ prefix"
    );

    // No prefix file import
    let no_prefix = imports.iter().find(|i| i.path == "no_prefix_file");
    assert!(no_prefix.is_some(), "Should find no_prefix_file import");
    let no_prefix = no_prefix.unwrap();
    assert!(no_prefix.is_file, "Should be a file import");
    assert!(no_prefix.prefix.is_none(), "Should have no prefix");
}

#[test]
fn test_get_imports_selective() {
    let mut parser = ParserEngine::new();
    let source = r#"
import { Token, Wallet as W } from "./mod";
import { A } from "./other";
"#;
    let imports = parser.get_imports(source);
    assert_eq!(imports.len(), 2);

    let first = imports.iter().find(|i| i.path == "./mod").unwrap();
    assert!(first.is_file);
    assert!(first.prefix.is_none());
    assert_eq!(
        first.elements,
        vec![
            ("Token".to_string(), None),
            ("Wallet".to_string(), Some("W".to_string())),
        ]
    );

    let second = imports.iter().find(|i| i.path == "./other").unwrap();
    assert_eq!(second.elements, vec![("A".to_string(), None)]);
}

#[test]
fn test_module_scoped_completion() {
    let mut parser = ParserEngine::new();
    // This is exactly what Utils.compact contains
    let source = r#"
pragma language_version >= 0.16;

module Utils {
  export circuit add(a: Field, b: Field): Field {
    return a + b;
  }
}
"#;
    let symbols = parser.get_completion_symbols(source);

    // Should find "add" circuit inside the module
    let add_symbol = symbols.iter().find(|s| s.name == "add");
    assert!(
        add_symbol.is_some(),
        "Should find 'add' circuit inside module. Found: {:?}",
        symbols
    );

    let add_symbol = add_symbol.unwrap();
    assert_eq!(
        add_symbol.kind,
        CompletionSymbolKind::Function,
        "Should be a Function"
    );
}

#[test]
fn test_syntax_errors_valid_code() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}
"#;
    let errors = parser.get_syntax_errors(source);
    assert!(errors.is_empty(), "Valid code should have no syntax errors");
}

#[test]
fn test_syntax_errors_missing_brace() {
    let mut parser = ParserEngine::new();
    // Missing closing brace
    let source = r#"circuit broken(): Field {
    return 1;
"#;
    let errors = parser.get_syntax_errors(source);
    assert!(
        !errors.is_empty(),
        "Missing brace should produce syntax error"
    );
}

#[test]
fn test_syntax_errors_unexpected_token() {
    let mut parser = ParserEngine::new();
    // Invalid syntax - unexpected token
    let source = r#"circuit !!!invalid(): Field { return 1; }"#;
    let errors = parser.get_syntax_errors(source);
    assert!(
        !errors.is_empty(),
        "Invalid identifier should produce syntax error"
    );
}

#[test]
fn test_syntax_errors_multiple() {
    let mut parser = ParserEngine::new();
    // Multiple syntax errors in different circuits
    let source = r#"
circuit broken1( {
    return 1;
}

circuit broken2(): Field
    return 2;
}

circuit broken3 {
    return 3;
}
"#;
    let errors = parser.get_syntax_errors(source);
    // Should find multiple syntax errors, not just the first one
    println!("Found {} syntax errors:", errors.len());
    for (i, err) in errors.iter().enumerate() {
        println!(
            "  {}: {} at line {}",
            i + 1,
            err.message,
            err.range.start.line + 1
        );
    }
    assert!(
        errors.len() >= 2,
        "Should find multiple syntax errors, found {}",
        errors.len()
    );
}

#[test]
fn test_contract_type_extraction() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let info = parser
        .contract_type(source, "Token")
        .expect("contract type");
    assert_eq!(info.name, "Token");
    assert_eq!(info.circuits.len(), 2);
    let transfer = &info.circuits[0];
    assert_eq!(transfer.name, "transfer");
    assert_eq!(transfer.detail, "(to: Address, amount: Uint<64>): Boolean");
    assert_eq!(
        transfer.signature,
        "circuit transfer(to: Address, amount: Uint<64>): Boolean"
    );
    assert!(parser.contract_type(source, "Missing").is_none());
}

#[test]
fn test_variable_type_resolves_contract_references() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit useParam(ref: Token): Boolean {
    return ref.transfer(0);
}

circuit useConst(): Boolean {
    const ref: Token = default<Token>;
    return ref.transfer(0);
}

ledger stored: Token;

circuit useLedger(): Boolean {
    return stored.transfer(0);
}
"#;
    assert_eq!(
        parser.get_variable_type(source, "ref").as_deref(),
        Some("Token")
    );
    assert_eq!(
        parser.get_variable_type(source, "stored").as_deref(),
        Some("Token")
    );
    assert!(parser.get_variable_type(source, "nope").is_none());
}

#[test]
fn test_variable_type_follows_aliases() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

type MyToken = Token;
type Chain = MyToken;

circuit viaAlias(ref: MyToken): Boolean {
    return ref.transfer(0);
}

const stored: Chain = default<Chain>;

circuit viaChainedAlias(): Boolean {
    return stored.transfer(0);
}
"#;
    assert_eq!(
        parser.get_variable_type(source, "ref").as_deref(),
        Some("Token")
    );
    assert_eq!(
        parser.get_variable_type(source, "stored").as_deref(),
        Some("Token")
    );
}

#[test]
fn test_variable_type_inferred_from_call_return() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit make(): Token {
    return ref;
}

circuit useInferred(): Boolean {
    const t = make();
    const u = (make());
    return t.transfer(0);
}
"#;
    assert_eq!(
        parser.get_variable_type(source, "t").as_deref(),
        Some("Token")
    );
    assert_eq!(
        parser.get_variable_type(source, "u").as_deref(),
        Some("Token")
    );
}

#[test]
fn test_variable_type_annotation_wins_over_inference() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
contract Other {
    circuit nop(): Boolean;
}

type MyToken = Token;

circuit make(): Other {
    return other;
}

circuit useAnnotated(): Boolean {
    const t: MyToken = make();
    return t.transfer(0);
}
"#;
    // The annotation `MyToken` is resolved through the alias to `Token`,
    // not the callee's (unrelated) return type `Other`.
    assert_eq!(
        parser.get_variable_type(source, "t").as_deref(),
        Some("Token")
    );
}

#[test]
fn test_variable_type_no_inference_for_non_call() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit f(a: Uint<64>, b: Uint<64>): Uint<64> {
    return a;
}

circuit g(): Uint<64> {
    const x = f(1, 2);
    const y = 1 + 2;
    return x;
}
"#;
    // A call to a circuit whose return type is a builtin resolves to its
    // head type name (matching `extract_base_type`)...
    assert_eq!(
        parser.get_variable_type(source, "x").as_deref(),
        Some("Uint")
    );
    // ...but a non-call initializer yields nothing.
    assert!(parser.get_variable_type(source, "y").is_none());
}

#[test]
fn test_variable_type_alias_cycle_terminates() {
    let mut parser = ParserEngine::new();
    let source = r#"
type A = B;
type B = A;

circuit f(x: A): Boolean {
    return x;
}
"#;
    // A cyclic alias chain must terminate rather than loop forever.
    assert!(parser.get_variable_type(source, "x").is_some());
}

#[test]
fn test_variable_type_generic_alias_skipped() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

type Box<T> = Token;

circuit f(ref: Box<Uint<64>>): Boolean {
    return ref.transfer(0);
}
"#;
    // Generic aliases are not substituted; the head name is returned as-is.
    assert_eq!(
        parser.get_variable_type(source, "ref").as_deref(),
        Some("Box")
    );
}

#[test]
fn test_variable_type_inferred_from_witness_return() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

witness make(): Token;

circuit f(): Boolean {
    const t = make();
    return t.transfer(0);
}
"#;
    assert_eq!(
        parser.get_variable_type(source, "t").as_deref(),
        Some("Token")
    );
}

#[test]
fn test_get_callable_return_type_and_initializer_callee() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit make(): Token {
    return ref;
}

circuit useIt(): Boolean {
    const t = make();
    const annotated: Token = make();
    return t.transfer(0);
}
"#;
    assert_eq!(
        parser.get_callable_return_type(source, "make").as_deref(),
        Some("Token")
    );
    assert_eq!(
        parser.get_initializer_callee(source, "t").as_deref(),
        Some("make")
    );
    // Annotated bindings are not unannotated initializer callees.
    assert!(parser.get_initializer_callee(source, "annotated").is_none());
}

#[test]
fn test_collect_member_accesses() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit useIt(a: Token, b: Token): Boolean {
    a.transfer(0);
    b.transfer(1);
    a.balanceOf();
}
"#;
    let uses = parser.collect_member_accesses(source, "transfer");
    assert_eq!(uses.len(), 2);
    assert_eq!(uses[0].base_name, "a");
    assert_eq!(uses[0].base, MemberBase::Path(vec!["a".to_string()]));
    assert_eq!(uses[1].base_name, "b");
    assert!(parser.collect_member_accesses(source, "missing").is_empty());

    // A member-chain base is captured in full.
    let chained = r#"
struct Wallet { token: Token, }
circuit g(w: Wallet): Boolean {
    w.token.transfer(0);
}
"#;
    let uses = parser.collect_member_accesses(chained, "transfer");
    assert_eq!(uses.len(), 1);
    assert_eq!(uses[0].base_name, "w.token");
    assert_eq!(
        uses[0].base,
        MemberBase::Path(vec!["w".to_string(), "token".to_string()])
    );
}

#[test]
fn test_get_struct_field_type() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Wallet {
    token: Token,
    owner: Address,
}

type MyWallet = Wallet;

circuit f(w: Wallet): Boolean {
    return w.token.transfer(0);
}
"#;
    assert_eq!(
        parser
            .get_struct_field_type(source, "Wallet", "token")
            .as_deref(),
        Some("Token")
    );
    assert_eq!(
        parser
            .get_struct_field_type(source, "MyWallet", "owner")
            .as_deref(),
        Some("Address")
    );
    assert!(
        parser
            .get_struct_field_type(source, "Wallet", "missing")
            .is_none()
    );
    assert!(parser.struct_exists(source, "Wallet"));
    assert!(!parser.struct_exists(source, "Nope"));
}

#[test]
fn test_member_access_base_chain() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Wallet { token: Token, }
struct Account { wallet: Wallet, }
circuit f(a: Account): Boolean {
    a.wallet.token.transfer(0, 1);
}
"#;
    let line = 4;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("member access context");
    assert_eq!(ctx.base_name, "a.wallet.token");
    assert_eq!(
        ctx.base,
        MemberBase::Path(vec![
            "a".to_string(),
            "wallet".to_string(),
            "token".to_string()
        ])
    );
    assert_eq!(ctx.member_name, "transfer");
}

#[test]
fn test_member_base_call_and_index() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token { circuit transfer(to: Address): Boolean; }
circuit make(): Token { }
circuit useCall(): Boolean {
    return make().transfer(0);
}
ledger items: Map<Address, Token>;
circuit useIndex(): Boolean {
    return items[0].transfer(0);
}
"#;
    let line = 4;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("call base context");
    assert_eq!(
        ctx.base,
        MemberBase::Call {
            callee: "make".to_string(),
            args: vec![],
            type_args: vec![],
        }
    );
    assert_eq!(ctx.member_name, "transfer");

    let line = 8;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("index base context");
    assert_eq!(
        ctx.base,
        MemberBase::Index(Box::new(MemberBase::Path(vec!["items".to_string()])))
    );
}

#[test]
fn test_member_base_method_call() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Factory { circuit create(): Token; }
contract Token { circuit transfer(to: Address): Boolean; }
circuit useIt(factory: Factory, tokens: Map<Address, Token>): Boolean {
    factory.create().transfer(0);
    tokens.lookup(0).transfer(0);
    return true;
}
"#;
    let line = 4;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("contract method-call base");
    assert_eq!(
        ctx.base,
        MemberBase::MethodCall(
            Box::new(MemberBase::Path(vec!["factory".to_string()])),
            "create".to_string()
        )
    );
    assert_eq!(ctx.base_name, "factory.create()");
    assert_eq!(ctx.member_name, "transfer");

    let line = 5;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("builtin method-call base");
    assert_eq!(
        ctx.base,
        MemberBase::MethodCall(
            Box::new(MemberBase::Path(vec!["tokens".to_string()])),
            "lookup".to_string()
        )
    );
    assert_eq!(ctx.base_name, "tokens.lookup()");
}

#[test]
fn test_member_base_access_on_call() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit useIt(items: List<Token>): Boolean {
    items.head().value.transfer(0);
    return true;
}
"#;
    let line = 2;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("field-on-method-call base");
    assert_eq!(
        ctx.base,
        MemberBase::Access(
            Box::new(MemberBase::MethodCall(
                Box::new(MemberBase::Path(vec!["items".to_string()])),
                "head".to_string()
            )),
            "value".to_string()
        )
    );
    assert_eq!(ctx.base_name, "items.head().value");
    assert_eq!(ctx.member_name, "transfer");
}

#[test]
fn test_member_base_call_args() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit id<T>(x: T): T { return x; }
circuit useIt(token: Token): Boolean {
    return id(token).transfer(0);
}
"#;
    let line = 3;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("call-with-args base");
    assert_eq!(
        ctx.base,
        MemberBase::Call {
            callee: "id".to_string(),
            args: vec![MemberBase::Path(vec!["token".to_string()])],
            type_args: vec![],
        }
    );
    assert_eq!(ctx.base_name, "id()");
}

#[test]
fn test_member_base_call_type_args() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit id<T>(x: T): T { return x; }
circuit useIt(token: Token): Boolean {
    return id<Uint<64>>(token).transfer(0);
}
"#;
    let line = 3;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("transfer").unwrap() as u32 + 1;
    let ctx = parser
        .get_member_access_context(source, line, col)
        .expect("turbofish base");
    assert_eq!(
        ctx.base,
        MemberBase::Call {
            callee: "id".to_string(),
            args: vec![MemberBase::Path(vec!["token".to_string()])],
            type_args: vec!["Uint<64>".to_string()],
        }
    );
}

#[test]
fn test_callable_signature_generic() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit id<T>(x: T): T { return x; }
circuit plain(a: Field): Boolean { return true; }
"#;
    let sig = parser.get_callable_signature(source, "id").unwrap();
    assert_eq!(sig.param_types, vec!["T".to_string()]);
    assert_eq!(sig.generic_params, "<T>");
    assert_eq!(sig.return_type.as_deref(), Some("T"));

    let plain = parser.get_callable_signature(source, "plain").unwrap();
    assert_eq!(plain.param_types, vec!["Field".to_string()]);
    assert!(plain.generic_params.is_empty());
    assert_eq!(plain.return_type.as_deref(), Some("Boolean"));
}

#[test]
fn test_struct_type_params() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Box<T> { value: T; }
struct Plain { n: Uint<64>; }
"#;
    assert_eq!(
        parser.get_struct_type_params(source, "Box").as_deref(),
        Some("<T>")
    );
    assert_eq!(parser.get_struct_type_params(source, "Plain"), None);
}

#[test]
fn test_enum_exists() {
    let mut parser = ParserEngine::new();
    let source = r#"
enum Phase { day, night }
struct Wallet { balance: Uint<64>; }
"#;
    assert!(parser.enum_exists(source, "Phase"));
    assert!(!parser.enum_exists(source, "Wallet"));
    assert!(parser.struct_exists(source, "Wallet"));
    assert!(!parser.struct_exists(source, "Phase"));
}

#[test]
fn test_type_member_at_position_and_definition() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Wallet {
    balance: Uint<64>,
}
enum Phase { day, night }
"#;
    // Struct field declaration.
    let line = 2;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("balance").unwrap() as u32 + 1;
    let (owner, member, kind) = parser.type_member_at_position(source, line, col).unwrap();
    assert_eq!(owner, "Wallet");
    assert_eq!(member, "balance");
    assert_eq!(kind, TypeMemberKind::Field);
    let field_def = parser
        .find_member_definition_location(source, "Wallet", "balance", TypeMemberKind::Field)
        .expect("field definition range");
    assert_eq!(field_def.start.line, 2);
    assert_eq!(
        field_def.start.character,
        text.find("balance").unwrap() as u32
    );

    // Enum variant declaration.
    let line = 4;
    let text = source.lines().nth(line as usize).unwrap();
    let col = text.find("night").unwrap() as u32 + 1;
    let (owner, member, kind) = parser.type_member_at_position(source, line, col).unwrap();
    assert_eq!(owner, "Phase");
    assert_eq!(member, "night");
    assert_eq!(kind, TypeMemberKind::Variant);
    let variant_def = parser
        .find_member_definition_location(source, "Phase", "night", TypeMemberKind::Variant)
        .expect("variant definition range");
    assert_eq!(variant_def.start.line, 4);
    assert_eq!(
        variant_def.start.character,
        text.find("night").unwrap() as u32
    );
}

#[test]
fn test_get_variable_type_raw_preserves_generics() {
    let mut parser = ParserEngine::new();
    let source = r#"
ledger items: Map<Address, Token>;
circuit f(): Boolean { return true; }
"#;
    assert_eq!(
        parser.get_variable_type_raw(source, "items").as_deref(),
        Some("Map<Address, Token>")
    );
    // `get_variable_type` strips generics to the head type name.
    assert_eq!(
        parser.get_variable_type(source, "items").as_deref(),
        Some("Map")
    );

    // Parameter (`parg`) types preserve generics too.
    let parg_src = r#"
circuit useIt(items: Vector<10, T_Token>): Boolean {
    return true;
}
"#;
    assert_eq!(
        parser.get_variable_type_raw(parg_src, "items").as_deref(),
        Some("Vector<10, T_Token>")
    );
}

#[test]
fn test_circuit_at_position() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
    // On the circuit name.
    assert_eq!(
        parser.circuit_at_position(source, 2, 15),
        Some(("Token".to_string(), "transfer".to_string()))
    );
    // On a type inside the signature.
    assert!(parser.circuit_at_position(source, 2, 25).is_none());
    // On the contract name.
    assert!(parser.circuit_at_position(source, 1, 10).is_none());
}

#[test]
fn test_enclosing_member_call() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}

circuit useParam(ref: Token): Boolean {
    return ref.transfer(0, 1);
}
"#;
    let line = 6;
    let line_text = source.lines().nth(line as usize).unwrap();
    let char = line_text.find(',').unwrap() as u32 + 1;
    let ctx = parser
        .get_enclosing_member_call(source, line, char)
        .expect("enclosing member call");
    assert_eq!(ctx.base_name, "ref");
    assert_eq!(ctx.member_name, "transfer");
}

#[test]
fn test_ecdecl_hover() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
    let info = parser
        .hover_info(source, 1, 9)
        .expect("hover for contract type");
    assert!(info.content.contains("Contract type"));
    assert!(info.content.contains("Token"));
    assert!(
        info.content
            .contains("circuit transfer(to: Address): Boolean")
    );
}

#[test]
fn test_contract_type_in_completion_symbols() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
    let symbols = parser.get_completion_symbols(source);
    let contract = symbols
        .iter()
        .find(|s| s.name == "Token")
        .expect("contract type in completion symbols");
    assert_eq!(contract.kind, CompletionSymbolKind::Contract);
    assert!(
        contract
            .documentation
            .as_deref()
            .unwrap_or("")
            .contains("transfer")
    );
}

#[test]
fn test_goto_definition_contract_implements() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

contract implements Token;
"#;
    let loc = parser
        .goto_definition(source, 5, 21)
        .expect("definition of the implemented contract type");
    assert_eq!(loc.selection_range.start.line, 1);
}

#[test]
fn test_semantic_tokens_basic() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

struct Point {
    x: Field;
    y: Field;
}
"#;
    let tokens = parser.get_semantic_tokens(source);

    // Should find tokens for: circuit name, params, types, struct name, fields
    assert!(!tokens.is_empty(), "Should find semantic tokens");

    // Check for function token (circuit name)
    let function_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Function)
        .collect();
    assert!(!function_tokens.is_empty(), "Should find function tokens");
    assert!(
        function_tokens
            .iter()
            .any(|t| t.modifiers.contains(&SemanticTokenModifier::Declaration)),
        "Function should have Declaration modifier"
    );

    // Check for type tokens (Field)
    let type_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Type)
        .collect();
    assert!(!type_tokens.is_empty(), "Should find type tokens");
    assert!(
        type_tokens
            .iter()
            .any(|t| t.modifiers.contains(&SemanticTokenModifier::DefaultLibrary)),
        "Field type should have DefaultLibrary modifier"
    );

    // Check for parameter tokens
    let param_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Parameter)
        .collect();
    assert!(!param_tokens.is_empty(), "Should find parameter tokens");

    // Check for struct token
    let struct_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Struct)
        .collect();
    assert!(!struct_tokens.is_empty(), "Should find struct tokens");

    // Check for property tokens (struct fields)
    let property_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Property)
        .collect();
    assert!(
        !property_tokens.is_empty(),
        "Should find property tokens for struct fields"
    );
}

#[test]
fn test_semantic_tokens_new_keywords() {
    let mut parser = ParserEngine::new();
    let source = r#"
contract Token {
    circuit transfer(to: Field): Boolean;
}

contract implements Token;

circuit f(): [] {
    emit([1, 2]);
    return [];
}
"#;
    let tokens = parser.get_semantic_tokens(source);
    let keywords: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Keyword)
        .collect();
    assert!(
        keywords.len() >= 2,
        "expected keyword tokens for emit/implements, got {keywords:?}"
    );
    assert!(
        keywords.iter().any(|t| t.range.start.line == 5),
        "expected keyword token(s) on the `contract implements` line"
    );
    assert!(
        keywords.iter().any(|t| t.range.start.line == 8),
        "expected the `emit` keyword token"
    );
}

#[test]
fn test_semantic_tokens_member_access() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit f(ref: Field): Field {
    return ref.value;
}
"#;
    let tokens = parser.get_semantic_tokens(source);
    assert!(
        tokens
            .iter()
            .any(|t| t.token_type == SemanticTokenType::Property && t.range.start.line == 2),
        "member field access should be a Property token"
    );
}

#[test]
fn test_semantic_tokens_enum() {
    let mut parser = ParserEngine::new();
    let source = r#"
enum Color {
    Red,
    Green,
    Blue,
}
"#;
    let tokens = parser.get_semantic_tokens(source);

    // Check for enum token
    let enum_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Enum)
        .collect();
    assert!(!enum_tokens.is_empty(), "Should find enum token");

    // Check for enum member tokens
    let member_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::EnumMember)
        .collect();
    assert_eq!(member_tokens.len(), 3, "Should find 3 enum member tokens");
}

#[test]
fn test_semantic_tokens_sorted() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit a(): Field { return 1; }
circuit b(): Field { return 2; }
"#;
    let tokens = parser.get_semantic_tokens(source);

    // Verify tokens are sorted by position
    for window in tokens.windows(2) {
        let a = &window[0];
        let b = &window[1];
        let a_pos = (a.range.start.line, a.range.start.character);
        let b_pos = (b.range.start.line, b.range.start.character);
        assert!(a_pos <= b_pos, "Tokens should be sorted by position");
    }
}

#[test]
fn test_find_references_circuit() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    let x = add(1, 2);
    let y = add(3, 4);
    return add(x, y);
}
"#;
    let refs = parser.find_references(source, "add");

    // Should find: 1 definition + 3 calls = 4 references
    assert_eq!(refs.len(), 4, "Should find 4 references to 'add'");

    // Check that exactly one is a definition
    let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
    assert_eq!(definitions.len(), 1, "Should find exactly 1 definition");

    // Check that three are usages
    let usages: Vec<_> = refs.iter().filter(|r| !r.is_definition).collect();
    assert_eq!(usages.len(), 3, "Should find 3 usages");
}

#[test]
fn test_find_references_struct() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Point {
    x: Field;
    y: Field;
}

circuit make_point(): Point {
    return Point { x: 0, y: 0 };
}

circuit use_point(p: Point): Field {
    return p.x;
}
"#;
    let refs = parser.find_references(source, "Point");

    // Should find: 1 definition + usages in return type, function param type
    assert!(
        refs.len() >= 3,
        "Should find at least 3 references to 'Point'"
    );

    // Check that exactly one is a definition
    let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
    assert_eq!(definitions.len(), 1, "Should find exactly 1 definition");
}

#[test]
fn test_find_references_no_match() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit foo(): Field {
    return 1;
}
"#;
    let refs = parser.find_references(source, "bar");

    // Should find nothing
    assert!(refs.is_empty(), "Should find no references to 'bar'");
}

// ========== New edge case tests ==========

#[test]
fn test_find_references_ledger() {
    let mut parser = ParserEngine::new();
    let source = r#"
ledger balance: Map<Address, Uint<64>>;

circuit deposit(amount: Uint<64>): Void {
    balance.insert(sender(), amount);
}

circuit withdraw(amount: Uint<64>): Uint<64> {
    return balance.lookup(sender());
}
"#;
    let refs = parser.find_references(source, "balance");

    // Should find at least the definition
    assert!(!refs.is_empty(), "Should find references to 'balance'");

    // Check that exactly one is a definition
    let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
    assert_eq!(
        definitions.len(),
        1,
        "Should find exactly 1 definition of 'balance'"
    );
}

#[test]
fn test_goto_definition_ledger() {
    let mut parser = ParserEngine::new();
    let source = r#"ledger counter: Counter;

circuit increment(): Void {
    counter.increment(1);
}"#;
    // Go to definition of "counter" on line 0 (the ledger declaration itself)
    let loc = parser.goto_definition(source, 0, 7);
    assert!(loc.is_some(), "Should find ledger definition");
    let loc = loc.unwrap();
    // Should point to line 0 where counter is defined
    assert_eq!(loc.selection_range.start.line, 0);
}

#[test]
fn test_semantic_tokens_ledger() {
    let mut parser = ParserEngine::new();
    let source = r#"
ledger balance: Map<Address, Uint<64>>;
ledger counter: Counter;
"#;
    let tokens = parser.get_semantic_tokens(source);

    // Check for property tokens (ledger names)
    let property_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Property)
        .collect();
    assert!(
        property_tokens.len() >= 2,
        "Should find at least 2 property tokens for ledgers"
    );

    // Check that ledger tokens have readonly modifier
    assert!(
        property_tokens
            .iter()
            .any(|t| t.modifiers.contains(&SemanticTokenModifier::Readonly)),
        "Ledger should have Readonly modifier"
    );
}

#[test]
fn test_semantic_tokens_witness() {
    let mut parser = ParserEngine::new();
    let source = r#"
witness get_secret(): Field;
witness get_private_key(id: Uint<32>): Bytes<32>;
"#;
    let tokens = parser.get_semantic_tokens(source);

    // Check for function tokens (witness names)
    let function_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.token_type == SemanticTokenType::Function)
        .collect();
    assert!(
        function_tokens.len() >= 2,
        "Should find at least 2 function tokens for witnesses"
    );

    // Check that witness tokens have declaration modifier
    assert!(
        function_tokens
            .iter()
            .all(|t| t.modifiers.contains(&SemanticTokenModifier::Declaration)),
        "Witness functions should have Declaration modifier"
    );
}

#[test]
fn test_folding_ranges_nested() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit test(x: Field): Field {
    if (x > 0) {
        const a = 1;
        if (x > 10) {
            const b = 2;
            return b;
        }
        return a;
    }
    return 0;
}
"#;
    let ranges = parser.folding_ranges(source);

    // Should find at least 3 folding ranges: circuit body + 2 if statements
    assert!(
        ranges.len() >= 3,
        "Should find at least 3 folding ranges for nested blocks, found {}",
        ranges.len()
    );

    // Check that we have different starting lines for nested structures
    let start_lines: Vec<_> = ranges.iter().map(|r| r.start_line).collect();
    let unique_lines: std::collections::HashSet<_> = start_lines.iter().collect();
    assert!(
        unique_lines.len() >= 2,
        "Should have folding ranges on different lines"
    );
}

#[test]
fn test_document_symbols_enum_variants() {
    let mut parser = ParserEngine::new();
    let source = r#"
enum Status {
    Pending,
    Active,
    Completed,
}
"#;
    let symbols = parser.document_symbols(source);

    assert_eq!(symbols.len(), 1, "Should find 1 enum");
    assert_eq!(symbols[0].name, "Status");
    assert_eq!(symbols[0].kind, SymbolKind::ENUM);

    // Check that enum has variant children
    let children = symbols[0].children.as_ref();
    assert!(children.is_some(), "Enum should have children");
    let children = children.unwrap();
    assert_eq!(children.len(), 3, "Should find 3 enum variants");

    // Verify variant names
    let variant_names: Vec<_> = children.iter().map(|c| c.name.as_str()).collect();
    assert!(
        variant_names.contains(&"Pending"),
        "Should have Pending variant"
    );
    assert!(
        variant_names.contains(&"Active"),
        "Should have Active variant"
    );
    assert!(
        variant_names.contains(&"Completed"),
        "Should have Completed variant"
    );
}

#[test]
fn test_document_symbols_struct_fields() {
    let mut parser = ParserEngine::new();
    let source = r#"
struct Transaction {
    sender: Address;
    recipient: Address;
    amount: Uint<64>;
    nonce: Field;
}
"#;
    let symbols = parser.document_symbols(source);

    assert_eq!(symbols.len(), 1, "Should find 1 struct");
    assert_eq!(symbols[0].name, "Transaction");
    assert_eq!(symbols[0].kind, SymbolKind::STRUCT);

    // Check that struct has field children
    let children = symbols[0].children.as_ref();
    assert!(children.is_some(), "Struct should have children");
    let children = children.unwrap();
    assert_eq!(children.len(), 4, "Should find 4 struct fields");

    // All children should be fields
    assert!(
        children.iter().all(|c| c.kind == SymbolKind::FIELD),
        "All struct children should be fields"
    );
}

#[test]
fn test_document_symbols_module() {
    let mut parser = ParserEngine::new();
    let source = r#"
module Math {
    export circuit add(a: Field, b: Field): Field {
        return a + b;
    }

    export circuit multiply(a: Field, b: Field): Field {
        return a * b;
    }
}
"#;
    let symbols = parser.document_symbols(source);

    assert_eq!(symbols.len(), 1, "Should find 1 module");
    assert_eq!(symbols[0].name, "Math");
    assert_eq!(symbols[0].kind, SymbolKind::MODULE);

    // Check that module has circuit children
    let children = symbols[0].children.as_ref();
    assert!(children.is_some(), "Module should have children");
    let children = children.unwrap();
    assert!(
        children.len() >= 2,
        "Should find at least 2 circuits in module"
    );
}

#[test]
fn test_hover_ledger_declaration() {
    let mut parser = ParserEngine::new();
    let source = r#"ledger balance: Map<Address, Uint<64>>;"#;
    // Hover on "balance" at position (0, 7)
    let info = parser.hover_info(source, 0, 7);
    assert!(info.is_some(), "Should find hover info for ledger");
    let info = info.unwrap();
    assert!(
        info.content.contains("ledger"),
        "Hover should mention 'ledger'"
    );
    assert!(
        info.content.contains("balance"),
        "Hover should contain name 'balance'"
    );
}

#[test]
fn test_hover_witness_declaration() {
    let mut parser = ParserEngine::new();
    let source = r#"witness get_secret(): Field;"#;
    // Hover on "get_secret" at position (0, 8)
    let info = parser.hover_info(source, 0, 8);
    assert!(info.is_some(), "Should find hover info for witness");
    let info = info.unwrap();
    assert!(
        info.content.contains("witness") || info.content.contains("Witness"),
        "Hover should mention 'witness'"
    );
}

#[test]
fn test_completion_symbols_with_documentation() {
    let mut parser = ParserEngine::new();
    let source = r#"
circuit calculate(x: Field, y: Field): Field {
    return x * y + 1;
}

struct Config {
    threshold: Uint<64>;
    enabled: Boolean;
}
"#;
    let symbols = parser.get_completion_symbols(source);

    // Check circuit has documentation
    let circuit_sym = symbols.iter().find(|s| s.name == "calculate");
    assert!(circuit_sym.is_some(), "Should find calculate circuit");
    let circuit_sym = circuit_sym.unwrap();
    assert!(
        circuit_sym.documentation.is_some(),
        "Circuit should have documentation"
    );
    assert!(
        circuit_sym
            .documentation
            .as_ref()
            .unwrap()
            .contains("Circuit function"),
        "Documentation should mention Circuit function"
    );

    // Check struct has documentation
    let struct_sym = symbols.iter().find(|s| s.name == "Config");
    assert!(struct_sym.is_some(), "Should find Config struct");
    let struct_sym = struct_sym.unwrap();
    assert!(
        struct_sym.documentation.is_some(),
        "Struct should have documentation"
    );
    assert!(
        struct_sym
            .documentation
            .as_ref()
            .unwrap()
            .contains("Struct type"),
        "Documentation should mention Struct type"
    );
}

#[test]
fn test_import_with_complex_path() {
    let mut parser = ParserEngine::new();
    let source = r#"
import "../../contracts/token/ERC20" prefix Token_;
import "../../../shared/utils/Helpers" prefix Help_;
import CompactStandardLibrary;

circuit main(): Field {
    return 1;
}
"#;
    let imports = parser.get_imports(source);

    assert_eq!(imports.len(), 3, "Should find 3 imports");

    // Check complex relative paths are handled
    let token_import = imports.iter().find(|i| i.path.contains("ERC20"));
    assert!(token_import.is_some(), "Should find ERC20 import");
    assert!(token_import.unwrap().is_file, "Should be a file import");

    let helpers_import = imports.iter().find(|i| i.path.contains("Helpers"));
    assert!(helpers_import.is_some(), "Should find Helpers import");
    assert_eq!(
        helpers_import.unwrap().prefix.as_deref(),
        Some("Help_"),
        "Should have Help_ prefix"
    );
}

// ========== Doc comment tests ==========

#[test]
fn test_clean_comment_text_single_line() {
    let parser = ParserEngine::new();
    assert_eq!(parser.clean_comment_text("// hello world"), "hello world");
    assert_eq!(parser.clean_comment_text("//hello"), "hello");
    assert_eq!(parser.clean_comment_text("//   spaced   "), "spaced");
}

#[test]
fn test_clean_comment_text_block() {
    let parser = ParserEngine::new();
    assert_eq!(parser.clean_comment_text("/* simple */"), "simple");
    assert_eq!(parser.clean_comment_text("/*multi\nline*/"), "multi\nline");
}

#[test]
fn test_clean_comment_text_jsdoc() {
    let parser = ParserEngine::new();
    // JSDoc-style with leading asterisks on each line
    let jsdoc = r#"/**
 * This is a JSDoc comment.
 * It has multiple lines.
 */"#;
    let cleaned = parser.clean_comment_text(jsdoc);
    assert!(
        cleaned.contains("This is a JSDoc comment"),
        "Should contain first line"
    );
    assert!(
        cleaned.contains("It has multiple lines"),
        "Should contain second line"
    );
}

#[test]
fn test_hover_with_doc_comment() {
    let mut parser = ParserEngine::new();
    let source = r#"// Adds two field elements together.
// Returns the sum.
circuit add(a: Field, b: Field): Field {
    return a + b;
}"#;
    // Hover on "add" at position (2, 8)
    let info = parser.hover_info(source, 2, 8);
    assert!(info.is_some(), "Should find hover info");
    let info = info.unwrap();
    assert!(
        info.content.contains("Circuit function"),
        "Should contain type description"
    );
    assert!(
        info.content.contains("Adds two field elements together"),
        "Should contain doc comment: {}",
        info.content
    );
    assert!(
        info.content.contains("Returns the sum"),
        "Should contain second line of doc: {}",
        info.content
    );
}

#[test]
fn test_hover_with_block_doc_comment() {
    let mut parser = ParserEngine::new();
    let source = r#"/* A point in 2D space */
struct Point {
    x: Field;
    y: Field;
}"#;
    // Hover on "Point" at position (1, 7)
    let info = parser.hover_info(source, 1, 7);
    assert!(info.is_some(), "Should find hover info");
    let info = info.unwrap();
    assert!(
        info.content.contains("Struct type"),
        "Should contain type description"
    );
    assert!(
        info.content.contains("A point in 2D space"),
        "Should contain block doc comment: {}",
        info.content
    );
}

#[test]
fn test_completion_with_doc_comment() {
    let mut parser = ParserEngine::new();
    let source = r#"// Calculates the sum of two numbers.
circuit add(a: Field, b: Field): Field {
    return a + b;
}

/**
 * Represents a 2D coordinate.
 * Used for geometric calculations.
 */
struct Point {
    x: Field;
    y: Field;
}"#;
    let symbols = parser.get_completion_symbols(source);

    // Check circuit has doc comment
    let add_sym = symbols.iter().find(|s| s.name == "add");
    assert!(add_sym.is_some(), "Should find add circuit");
    let add_doc = add_sym.unwrap().documentation.as_ref().unwrap();
    assert!(
        add_doc.contains("Calculates the sum"),
        "Circuit docs should contain user comment: {}",
        add_doc
    );

    // Check struct has JSDoc comment
    let point_sym = symbols.iter().find(|s| s.name == "Point");
    assert!(point_sym.is_some(), "Should find Point struct");
    let point_doc = point_sym.unwrap().documentation.as_ref().unwrap();
    assert!(
        point_doc.contains("Represents a 2D coordinate"),
        "Struct docs should contain JSDoc comment: {}",
        point_doc
    );
}

#[test]
fn test_hover_no_doc_comment() {
    let mut parser = ParserEngine::new();
    // No doc comment, just the circuit
    let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
    let info = parser.hover_info(source, 0, 8);
    assert!(info.is_some(), "Should find hover info");
    let info = info.unwrap();
    assert!(
        info.content.contains("Circuit function"),
        "Should contain type description"
    );
    // Should not have the separator since there's no doc comment
    assert!(
        !info.content.contains("---\n\n"),
        "Should not have doc separator without doc: {}",
        info.content
    );
}

#[test]
fn test_find_references_selective_import_element() {
    let mut parser = ParserEngine::new();
    let source = r#"import { Order, total as totalOf } from "./lib";

circuit useIt(order: Order): Uint<64> {
    return totalOf(order);
}
"#;
    let refs = parser.find_references(source, "Order");

    // The import element and the parameter type are both references; the
    // selective import does not define `Order` locally.
    assert_eq!(refs.len(), 2, "expected import element + type reference");
    assert!(refs.iter().all(|r| !r.is_definition));
    assert_eq!(
        refs[0].range.start.line, 0,
        "first ref is the import element"
    );
    assert_eq!(
        refs[1].range.start.line, 2,
        "second ref is the parameter type"
    );
}

#[test]
fn test_utf16_point_conversions() {
    let mut parser = ParserEngine::new();
    parser.parse("/* 😀 */ x\né y\n");

    // Byte column 11 (after the emoji block comment) is UTF-16 column 9.
    assert_eq!(
        parser
            .point_to_position(tree_sitter::Point { row: 0, column: 11 })
            .character,
        9
    );
    assert_eq!(parser.point_from_utf16(0, 9).column, 11);

    // `é` is two bytes but one UTF-16 code unit.
    assert_eq!(
        parser
            .point_to_position(tree_sitter::Point { row: 1, column: 2 })
            .character,
        1
    );
    assert_eq!(parser.point_from_utf16(1, 2).column, 3);
}
