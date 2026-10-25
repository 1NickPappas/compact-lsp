//! End-to-end tests for cross-contract-call intelligence.

mod common;

use common::{LspClient, initialize, line_col, location_pairs, open};
use serde_json::json;

/// Assert the full member-feature set for `<base>.transfer(...)` in `main_src`.
///
/// The base can be any expression (`ref`, `w.token`, `make()`, `items[0]`,
/// `factory.create()`, …); the completion position is located from the dot
/// preceding `transfer` so the base text never needs to be reconstructed.
fn assert_member_features(
    client: &mut LspClient,
    main_uri: &str,
    main_src: &str,
    call_line: u32,
    def_uri_marker: &str,
    def_line: u32,
) {
    // Member completion right after the dot preceding `transfer`.
    let dot = line_col(main_src, call_line, ".transfer", 1);
    let completion = client.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": main_uri },
            "position": { "line": call_line, "character": dot },
            "context": { "triggerKind": 2, "triggerCharacter": "." }
        }),
    );
    let completion_text = completion.to_string();
    assert!(
        completion_text.contains("transfer"),
        "member completion missing `transfer`: {completion_text}"
    );
    assert!(
        completion_text.contains("balanceOf"),
        "member completion missing `balanceOf`: {completion_text}"
    );

    // Hover on the called circuit.
    let member = line_col(main_src, call_line, "transfer", 1);
    let hover = client.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": main_uri },
            "position": { "line": call_line, "character": member }
        }),
    );
    let hover_text = hover.to_string();
    assert!(
        hover_text.contains("transfer"),
        "hover missing `transfer`: {hover_text}"
    );

    // Signature help inside `ref.transfer(`.
    let paren = line_col(main_src, call_line, "transfer(", 9);
    let signature = client.request(
        "textDocument/signatureHelp",
        json!({
            "textDocument": { "uri": main_uri },
            "position": { "line": call_line, "character": paren }
        }),
    );
    let signature_text = signature.to_string();
    assert!(
        signature_text.contains("amount"),
        "signature help missing parameter `amount`: {signature_text}"
    );

    // Definition points at the `ecdecl_circuit` in the defining file.
    let definition = client.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": main_uri },
            "position": { "line": call_line, "character": member }
        }),
    );
    let definition_text = definition.to_string();
    assert!(
        definition_text.contains(def_uri_marker),
        "definition should point into `{def_uri_marker}`: {definition_text}"
    );
    assert!(
        definition_text.contains(&format!("\"line\":{def_line}")),
        "definition should be on line {def_line}: {definition_text}"
    );
}

#[test]
fn test_cross_contract_call_lsp_features() {
    let dir = tempfile::tempdir().expect("tempdir");
    let file = dir.path().join("main.compact");

    let source = r#"
contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}

contract implements Token;

circuit useIt(ref: Token): Boolean {
    return ref.transfer(0, 1);
}
"#;
    std::fs::write(&file, source).expect("write source");

    let root_uri = format!("file://{}", dir.path().display());
    let file_uri = format!("file://{}", file.display());

    let mut client = LspClient::start();

    let init = client.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": root_uri,
            "capabilities": {}
        }),
    );
    assert!(init.get("result").is_some(), "initialize failed: {init}");
    client.notify("initialized", json!({}));
    client.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": file_uri,
                "languageId": "compact",
                "version": 1,
                "text": source
            }
        }),
    );

    let call_line = 9u32; // "    return ref.transfer(0, 1);"

    // 1. Member completion after `ref.` offers the contract type's circuits.
    let dot = line_col(source, call_line, "ref.", 4);
    let completion = client.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": file_uri },
            "position": { "line": call_line, "character": dot },
            "context": { "triggerKind": 2, "triggerCharacter": "." }
        }),
    );
    let completion_text = completion.to_string();
    assert!(
        completion_text.contains("transfer"),
        "member completion missing `transfer`: {completion_text}"
    );
    assert!(
        completion_text.contains("balanceOf"),
        "member completion missing `balanceOf`: {completion_text}"
    );

    // 2. Hover on the called circuit describes it in terms of the contract type.
    let member = line_col(source, call_line, "transfer", 1);
    let hover = client.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": file_uri },
            "position": { "line": call_line, "character": member }
        }),
    );
    let hover_text = hover.to_string();
    assert!(
        hover_text.contains("transfer"),
        "hover missing `transfer`: {hover_text}"
    );
    assert!(
        hover_text.contains("Token"),
        "hover missing contract type `Token`: {hover_text}"
    );

    // 3. Signature help inside `ref.transfer(`.
    let paren = line_col(source, call_line, "transfer(", 9);
    let signature = client.request(
        "textDocument/signatureHelp",
        json!({
            "textDocument": { "uri": file_uri },
            "position": { "line": call_line, "character": paren }
        }),
    );
    let signature_text = signature.to_string();
    assert!(
        signature_text.contains("transfer"),
        "signature help missing `transfer`: {signature_text}"
    );
    assert!(
        signature_text.contains("amount"),
        "signature help missing parameter `amount`: {signature_text}"
    );

    // 4. Go-to-definition on `ref.transfer` jumps into the contract type.
    let definition = client.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": file_uri },
            "position": { "line": call_line, "character": member }
        }),
    );
    let definition_text = definition.to_string();
    assert!(
        definition_text.contains("\"line\":2"),
        "definition should point at the `ecdecl_circuit` on line 2: {definition_text}"
    );

    // 5. `contract implements ` offers contract types.
    let implements_line = 6u32; // "contract implements Token;"
    let after_implements = line_col(source, implements_line, "implements ", 11);
    let types = client.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": file_uri },
            "position": { "line": implements_line, "character": after_implements }
        }),
    );
    let types_text = types.to_string();
    assert!(
        types_text.contains("Token"),
        "`contract implements` completion missing `Token`: {types_text}"
    );

    client.shutdown();
}

/// Real-world layout: the contract type lives in another file and is imported
/// without a prefix. Resolution uses the workspace symbol/source caches.
#[test]
fn test_cross_file_contract_type_unprefixed_import() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token";

circuit useIt(ref: Token): Boolean {
    return ref.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    // Open the defining file first so its symbols are cached even without a
    // completed workspace scan.
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// Realistic editor case: the defining `token.compact` is **not open**. The
/// contract type must be resolved from the workspace scan's symbol/source caches.
#[test]
fn test_cross_file_contract_type_workspace_scan() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token";

circuit useIt(ref: Token): Boolean {
    return ref.transfer(0, 1);
}
"#;

    let main_file = dir.path().join("caller.compact");
    std::fs::write(dir.path().join("token.compact"), token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    // `initialized` awaits scan_workspace before logging "server ready".
    client.wait_for_log_message("server ready");

    // Only the caller is open; `token.compact` is resolved from the scan cache.
    open(&mut client, &main_uri, main_src);
    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// A local `type` alias pointing at an imported contract type is followed.
#[test]
fn test_cross_file_contract_type_via_alias() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

type MyToken = T_Token;

circuit useIt(ref: MyToken): Boolean {
    return ref.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// `const t = make();` infers `t`'s type from the callee's declared return type.
#[test]
fn test_cross_file_contract_type_inferred_from_call() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit make(): T_Token {
}

circuit useIt(): Boolean {
    const t = make();
    return t.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 8, "token.compact", 2);
    client.shutdown();
}

/// Same, but with a `prefix` import: the type is referenced as `T_Token` and
/// resolution goes through the import-prefix path.
#[test]
fn test_cross_file_contract_type_prefixed_import() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit useIt(ref: T_Token): Boolean {
    return ref.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// `const t = F_make();` where `make` is defined in another file: infer `t`'s
/// type from the callee's return type, resolved in the *callee's* file scope.
#[test]
fn test_cross_file_inferred_from_imported_callee() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let factory_src = r#"
import "./token" prefix T_;

export circuit make(): T_Token {
}
"#;
    let main_src = r#"
import "./factory" prefix F_;

circuit useIt(): Boolean {
    const t = F_make();
    return t.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let factory_file = dir.path().join("factory.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&factory_file, factory_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let factory_uri = format!("file://{}", factory_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &factory_uri, factory_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 5, "token.compact", 2);
    client.shutdown();
}

/// Find References for a contract circuit: from its declaration and from a use,
/// across files, gated by the base's resolved contract type.
#[test]
fn test_contract_circuit_references() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit useIt(a: T_Token, b: T_Token): Boolean {
    a.transfer(0, 1);
    b.transfer(2, 3);
    return true;
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    // From the declaration in token.compact: def + two uses.
    let from_decl = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": token_uri },
            "position": { "line": 2, "character": 15 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&from_decl);
    assert_eq!(pairs.len(), 3, "expected def + 2 uses: {from_decl}");
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("token.compact") && *l == 2),
        "missing declaration: {from_decl}"
    );
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("caller.compact") && *l == 4),
        "missing first use: {from_decl}"
    );
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("caller.compact") && *l == 5),
        "missing second use: {from_decl}"
    );

    // From a use in caller.compact: same set.
    let from_use = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": main_uri },
            "position": { "line": 4, "character": 7 },
            "context": { "includeDeclaration": true }
        }),
    );
    assert_eq!(
        location_pairs(&from_use).len(),
        3,
        "expected def + 2 uses: {from_use}"
    );

    client.shutdown();
}

/// Rename a contract circuit: edits its declaration and every gated use.
#[test]
fn test_contract_circuit_rename() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit useIt(a: T_Token, b: T_Token): Boolean {
    a.transfer(0, 1);
    b.transfer(2, 3);
    return true;
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    let edit = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": token_uri },
            "position": { "line": 2, "character": 15 },
            "newName": "send"
        }),
    );
    let changes = edit
        .pointer("/result/changes")
        .and_then(|c| c.as_object())
        .unwrap_or_else(|| panic!("expected workspace edit changes: {edit}"));

    let mut token_edits = 0;
    let mut caller_edits = 0;
    for (uri, edits) in changes {
        let arr = edits.as_array().expect("edits array");
        for e in arr {
            assert_eq!(e.get("newText").and_then(|v| v.as_str()), Some("send"));
        }
        if uri.contains("token.compact") {
            token_edits += arr.len();
        }
        if uri.contains("caller.compact") {
            caller_edits += arr.len();
        }
    }
    assert_eq!(token_edits, 1, "declaration edit: {edit}");
    assert_eq!(caller_edits, 2, "usage edits: {edit}");

    client.shutdown();
}

/// Member-chain base in the same file: `w.token.transfer(...)` where `Wallet`
/// is a local struct whose field is the contract type.
#[test]
fn test_member_chain_struct_field() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

struct Wallet {
    token: T_Token,
}

circuit useIt(w: Wallet): Boolean {
    return w.token.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 8, "token.compact", 2);
    client.shutdown();
}

/// Member-chain base whose struct is defined in another file.
#[test]
fn test_member_chain_imported_struct() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let wallet_src = r#"
import "./token" prefix T_;

export struct Wallet {
    token: T_Token,
}
"#;
    let main_src = r#"
import "./wallet" prefix W_;

circuit useIt(w: W_Wallet): Boolean {
    return w.token.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let wallet_file = dir.path().join("wallet.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&wallet_file, wallet_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let wallet_uri = format!("file://{}", wallet_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &wallet_uri, wallet_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// Find References must also find a contract-circuit use behind a struct field.
#[test]
fn test_member_chain_references() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

struct Wallet { token: T_Token, }

circuit useIt(w: Wallet): Boolean {
    w.token.transfer(0, 1);
    return true;
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": token_uri },
            "position": { "line": 2, "character": 15 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&refs);
    assert_eq!(pairs.len(), 2, "expected def + 1 chained use: {refs}");
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("caller.compact") && *l == 6),
        "missing chained use: {refs}"
    );

    client.shutdown();
}

/// Expression base: `make().transfer(...)` where `make()` returns the contract
/// type. Covers completion (text-extracted base) + AST-based member features.
#[test]
fn test_member_call_base() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit make(): T_Token {
}

circuit useIt(): Boolean {
    return make().transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 7, "token.compact", 2);
    client.shutdown();
}

/// Index base: `items[0].transfer(...)` where `items: Vector<Token>`.
#[test]
fn test_member_index_base() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit useIt(items: Vector<10, T_Token>): Boolean {
    return items[0].transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// Find References must find a contract-circuit use behind a call base.
#[test]
fn test_member_expression_references() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit make(): T_Token {
}

circuit useIt(): Boolean {
    make().transfer(0, 1);
    return true;
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": token_uri },
            "position": { "line": 2, "character": 15 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&refs);
    assert_eq!(pairs.len(), 2, "expected def + 1 call-base use: {refs}");
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("caller.compact") && *l == 7),
        "missing call-base use: {refs}"
    );

    client.shutdown();
}

/// Method-call base via a contract circuit: `factory.create().transfer(...)`,
/// where `factory.create()` returns the contract type. Cross-file.
#[test]
fn test_member_method_call_base() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let factory_src = r#"
import "./token" prefix T_;

export contract Factory {
    circuit create(): T_Token;
}
"#;
    let main_src = r#"
import "./factory" prefix F_;

circuit useIt(factory: F_Factory): Boolean {
    return factory.create().transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let factory_file = dir.path().join("factory.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&factory_file, factory_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let factory_uri = format!("file://{}", factory_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &factory_uri, factory_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// Method-call base via a built-in container: `tokens.lookup(k).transfer(...)`,
/// where `Map.lookup` returns the map's value type (generic substitution).
#[test]
fn test_member_builtin_method_call_base() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

ledger tokens: Map<Address, T_Token>;

circuit useIt(): Boolean {
    return tokens.lookup(0).transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// Field access on a method result:
/// `items.head().value.transfer(...)` — `List<T>.head()` returns `Maybe<T>`,
/// whose `value` field is `T`. Exercises stdlib field resolution + generic
/// substitution on top of a method-call base.
#[test]
fn test_member_access_on_method_result() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

ledger items: List<T_Token>;

circuit useIt(): Boolean {
    return items.head().value.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// Generic type parameters, instantiated at the call site:
/// `id(token).transfer(...)` where `circuit id<T>(x: T): T` and `token` is the
/// contract type. The return type `T` is substituted from the argument.
#[test]
fn test_member_generic_call_base() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit id<T>(x: T): T { return x; }

circuit useIt(token: T_Token): Boolean {
    return id(token).transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// Generic struct field substitution: `box.value.transfer(...)` where
/// `struct Box<T> { value: T }` and `box: Box<Token>`.
#[test]
fn test_member_generic_struct_field() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

struct Box<T> { value: T; }

ledger box: Box<T_Token>;

circuit useIt(): Boolean {
    return box.value.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 8, "token.compact", 2);
    client.shutdown();
}

/// Find References for a struct field, cross-file, gated by the owner struct.
#[test]
fn test_struct_field_references() {
    let dir = tempfile::tempdir().expect("tempdir");
    let wallet_src = r#"
export struct Wallet {
    balance: Uint<64>,
}
"#;
    let main_src = r#"
import "./wallet" prefix W_;

circuit useIt(w: W_Wallet): Uint<64> {
    const x = w.balance;
    return w.balance + x;
}
"#;

    let wallet_file = dir.path().join("wallet.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&wallet_file, wallet_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let wallet_uri = format!("file://{}", wallet_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &wallet_uri, wallet_src);
    open(&mut client, &main_uri, main_src);

    // From the field declaration.
    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": wallet_uri },
            "position": { "line": 2, "character": 5 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&refs);
    assert_eq!(pairs.len(), 3, "expected def + 2 uses: {refs}");
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("wallet.compact") && *l == 2),
        "missing declaration: {refs}"
    );
    assert!(
        pairs
            .iter()
            .filter(|(u, _)| u.contains("caller.compact"))
            .count()
            == 2,
        "expected 2 caller uses: {refs}"
    );

    client.shutdown();
}

/// Rename a struct field, cross-file, gated by the owner struct.
#[test]
fn test_struct_field_rename() {
    let dir = tempfile::tempdir().expect("tempdir");
    let wallet_src = r#"
export struct Wallet {
    balance: Uint<64>,
}
"#;
    let main_src = r#"
import "./wallet" prefix W_;

circuit useIt(w: W_Wallet): Uint<64> {
    const x = w.balance;
    return w.balance + x;
}
"#;

    let wallet_file = dir.path().join("wallet.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&wallet_file, wallet_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let wallet_uri = format!("file://{}", wallet_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &wallet_uri, wallet_src);
    open(&mut client, &main_uri, main_src);

    let edit = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": wallet_uri },
            "position": { "line": 2, "character": 5 },
            "newName": "amount"
        }),
    );
    let changes = edit
        .pointer("/result/changes")
        .and_then(|c| c.as_object())
        .unwrap_or_else(|| panic!("expected workspace edit changes: {edit}"));
    let mut wallet_edits = 0;
    let mut caller_edits = 0;
    for (uri, edits) in changes {
        let arr = edits.as_array().expect("edits array");
        for e in arr {
            assert_eq!(e.get("newText").and_then(|v| v.as_str()), Some("amount"));
        }
        if uri.contains("wallet.compact") {
            wallet_edits += arr.len();
        }
        if uri.contains("caller.compact") {
            caller_edits += arr.len();
        }
    }
    assert_eq!(wallet_edits, 1, "declaration edit: {edit}");
    assert_eq!(caller_edits, 2, "usage edits: {edit}");

    client.shutdown();
}

/// Find References for an enum variant, cross-file, gated by the owner enum.
#[test]
fn test_enum_variant_references() {
    let dir = tempfile::tempdir().expect("tempdir");
    let phase_src = r#"
export enum Phase { day, night }
"#;
    let main_src = r#"
import "./phase" prefix P_;

circuit useIt(p: P_Phase): Boolean {
    const d = P_Phase.day;
    return p == P_Phase.day && d == P_Phase.day;
}
"#;

    let phase_file = dir.path().join("phase.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&phase_file, phase_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let phase_uri = format!("file://{}", phase_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &phase_uri, phase_src);
    open(&mut client, &main_uri, main_src);

    // From the variant declaration (`day` starts at char 19).
    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": phase_uri },
            "position": { "line": 1, "character": 20 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&refs);
    let caller_uses = pairs
        .iter()
        .filter(|(u, _)| u.contains("caller.compact"))
        .count();
    assert!(
        pairs
            .iter()
            .any(|(u, l)| u.contains("phase.compact") && *l == 1),
        "missing declaration: {refs}"
    );
    assert_eq!(caller_uses, 3, "expected 3 variant uses: {refs}");

    client.shutdown();
}

/// Rename an enum variant, cross-file, gated by the owner enum.
#[test]
fn test_enum_variant_rename() {
    let dir = tempfile::tempdir().expect("tempdir");
    let phase_src = r#"
export enum Phase { day, night }
"#;
    let main_src = r#"
import "./phase" prefix P_;

circuit useIt(p: P_Phase): Boolean {
    const d = P_Phase.day;
    return p == P_Phase.day && d == P_Phase.day;
}
"#;

    let phase_file = dir.path().join("phase.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&phase_file, phase_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let phase_uri = format!("file://{}", phase_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &phase_uri, phase_src);
    open(&mut client, &main_uri, main_src);

    let edit = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": phase_uri },
            "position": { "line": 1, "character": 20 },
            "newName": "monday"
        }),
    );
    let changes = edit
        .pointer("/result/changes")
        .and_then(|c| c.as_object())
        .unwrap_or_else(|| panic!("expected workspace edit changes: {edit}"));
    let mut phase_edits = 0;
    let mut caller_edits = 0;
    for (uri, edits) in changes {
        let arr = edits.as_array().expect("edits array");
        for e in arr {
            assert_eq!(e.get("newText").and_then(|v| v.as_str()), Some("monday"));
        }
        if uri.contains("phase.compact") {
            phase_edits += arr.len();
        }
        if uri.contains("caller.compact") {
            caller_edits += arr.len();
        }
    }
    assert_eq!(phase_edits, 1, "declaration edit: {edit}");
    assert_eq!(caller_edits, 3, "usage edits: {edit}");

    client.shutdown();
}

/// Selective import binding with an alias: `import { Token as T } from "./token"`
/// resolves `T` to the imported `Token` contract type. The workspace scan cannot
/// resolve the bare alias `T`, so this exercises selective binding specifically.
#[test]
fn test_selective_import_contract_alias() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import { Token as T } from "./token";

circuit useIt(t: T): Boolean {
    return t.transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 4, "token.compact", 2);
    client.shutdown();
}

/// Selective import binding for a struct under an alias, then a field reference
/// through that aliased type: `import { Wallet as W } from "./wallet"` + `w.balance`.
#[test]
fn test_selective_import_struct_alias_field() {
    let dir = tempfile::tempdir().expect("tempdir");
    let wallet_src = r#"
export struct Wallet {
    balance: Uint<64>,
}
"#;
    let main_src = r#"
import { Wallet as W } from "./wallet";

circuit useIt(w: W): Uint<64> {
    const x = w.balance;
    return w.balance + x;
}
"#;

    let wallet_file = dir.path().join("wallet.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&wallet_file, wallet_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let wallet_uri = format!("file://{}", wallet_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &wallet_uri, wallet_src);
    open(&mut client, &main_uri, main_src);

    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": wallet_uri },
            "position": { "line": 2, "character": 5 },
            "context": { "includeDeclaration": true }
        }),
    );
    let pairs = location_pairs(&refs);
    assert_eq!(pairs.len(), 3, "expected def + 2 uses: {refs}");
    assert!(
        pairs
            .iter()
            .filter(|(u, _)| u.contains("caller.compact"))
            .count()
            == 2,
        "expected 2 caller uses: {refs}"
    );

    client.shutdown();
}

/// Explicit call-site type arguments with a *nested* parameter: the generic
/// function's parameter is `Maybe<T>`, so argument inference can't bind `T`.
/// Compact requires `pick<T_Token>(m)`, and reading the explicit `<T_Token>`
/// resolves the receiver to the `Token` contract.
#[test]
fn test_member_nested_generic_explicit_type_arg() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

circuit pick<T>(m: Maybe<T>): T { return m.value; }

circuit useIt(m: Maybe<T_Token>): Boolean {
    return pick<T_Token>(m).transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// Explicit call-site type arguments for a *return-only* generic parameter:
/// a nullary generic witness `get<T>(): T` has no argument to infer from, so
/// only the explicit `<T_Token>` can resolve the receiver.
#[test]
fn test_member_return_only_generic_explicit_type_arg() {
    let dir = tempfile::tempdir().expect("tempdir");
    let token_src = r#"
export contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
    let main_src = r#"
import "./token" prefix T_;

witness get<T>(): T;

circuit useIt(): Boolean {
    return get<T_Token>().transfer(0, 1);
}
"#;

    let token_file = dir.path().join("token.compact");
    let main_file = dir.path().join("caller.compact");
    std::fs::write(&token_file, token_src).unwrap();
    std::fs::write(&main_file, main_src).unwrap();

    let token_uri = format!("file://{}", token_file.display());
    let main_uri = format!("file://{}", main_file.display());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", dir.path().display()));
    open(&mut client, &token_uri, token_src);
    open(&mut client, &main_uri, main_src);

    assert_member_features(&mut client, &main_uri, main_src, 6, "token.compact", 2);
    client.shutdown();
}

/// The server advertises range formatting support. Deterministic — it does not
/// depend on `format-compact` being installed (the edit logic is unit-tested).
#[test]
fn test_range_formatting_capability() {
    let dir = tempfile::tempdir().expect("tempdir");
    let mut client = LspClient::start();
    let init = client.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": format!("file://{}", dir.path().display()),
            "capabilities": {}
        }),
    );
    let supported = init
        .pointer("/result/capabilities/documentRangeFormattingProvider")
        .and_then(|v| v.as_bool());
    assert_eq!(
        supported,
        Some(true),
        "range formatting not advertised: {init}"
    );
    client.shutdown();
}
