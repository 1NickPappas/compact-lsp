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
