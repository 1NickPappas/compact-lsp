//! End-to-end test for cross-contract-call intelligence.
//!
//! Spawns the real `compact-lsp` binary, speaks JSON-RPC over stdio, and checks
//! that a contract reference (`ref: Token`) gets circuit completion, hover,
//! signature help, and go-to-definition, and that `contract implements` offers
//! contract types.

use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::{Duration, Instant};

use serde_json::{Value, json};

const TIMEOUT: Duration = Duration::from_secs(30);

struct LspClient {
    child: Child,
    stdin: ChildStdin,
    rx: Receiver<Value>,
    next_id: i64,
}

impl LspClient {
    fn start() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_compact-lsp"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn compact-lsp");

        let stdin = child.stdin.take().expect("stdin");
        let stdout = child.stdout.take().expect("stdout");

        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                let mut content_length = None;
                loop {
                    let mut line = String::new();
                    match reader.read_line(&mut line) {
                        Ok(0) | Err(_) => return,
                        Ok(_) => {}
                    }
                    let line = line.trim_end();
                    if line.is_empty() {
                        break;
                    }
                    if let Some(v) = line.strip_prefix("Content-Length:") {
                        content_length = v.trim().parse::<usize>().ok();
                    }
                }
                let Some(len) = content_length else {
                    return;
                };
                let mut buf = vec![0u8; len];
                if reader.read_exact(&mut buf).is_err() {
                    return;
                }
                if let Ok(value) = serde_json::from_slice::<Value>(&buf)
                    && tx.send(value).is_err()
                {
                    return;
                }
            }
        });

        Self {
            child,
            stdin,
            rx,
            next_id: 1,
        }
    }

    fn send(&mut self, message: Value) {
        let body = serde_json::to_string(&message).unwrap();
        write!(self.stdin, "Content-Length: {}\r\n\r\n{}", body.len(), body)
            .expect("write LSP message");
        self.stdin.flush().expect("flush LSP message");
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));

        let deadline = Instant::now() + TIMEOUT;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            assert!(!remaining.is_zero(), "timed out waiting for {method}");
            match self.rx.recv_timeout(remaining) {
                Ok(value) if value.get("id") == Some(&json!(id)) => return value,
                // Skip notifications (e.g. publishDiagnostics) and unrelated messages.
                Ok(_) => continue,
                Err(e) => panic!("timed out waiting for {method}: {e}"),
            }
        }
    }

    /// Wait for a `window/logMessage` notification containing `needle`. The
    /// server sends "server ready" only after the workspace scan completes.
    fn wait_for_log_message(&mut self, needle: &str) {
        let deadline = Instant::now() + TIMEOUT;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            assert!(
                !remaining.is_zero(),
                "timed out waiting for log message containing `{needle}`"
            );
            match self.rx.recv_timeout(remaining) {
                Ok(value)
                    if value.get("method") == Some(&json!("window/logMessage"))
                        && value.to_string().contains(needle) =>
                {
                    return;
                }
                Ok(_) => continue,
                Err(e) => panic!("timed out waiting for log message: {e}"),
            }
        }
    }

    fn shutdown(mut self) {
        self.notify("exit", Value::Null);
        let _ = self.child.wait();
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn line_col(source: &str, line: u32, needle: &str, offset: usize) -> u32 {
    let text = source.lines().nth(line as usize).expect("line exists");
    (text.find(needle).expect("needle exists") + offset) as u32
}

fn initialize(client: &mut LspClient, root_uri: &str) {
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
}

fn open(client: &mut LspClient, uri: &str, text: &str) {
    client.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri,
                "languageId": "compact",
                "version": 1,
                "text": text
            }
        }),
    );
}

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

/// Extract `(uri, start line)` pairs from a `textDocument/references` response.
fn location_pairs(value: &Value) -> Vec<(String, u32)> {
    value
        .get("result")
        .and_then(|r| r.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|loc| {
                    let uri = loc.get("uri")?.as_str()?.to_string();
                    let line = loc.pointer("/range/start/line")?.as_u64()? as u32;
                    Some((uri, line))
                })
                .collect()
        })
        .unwrap_or_default()
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
