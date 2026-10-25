//! Shared harness for end-to-end tests.
//!
//! Spawns the real `compact-lsp` binary, speaks LSP JSON-RPC over stdin/stdout,
//! and offers helpers for the full document lifecycle, diagnostics, workspace
//! edits, and compiler detection.
//!
//! The whole module is `allow(dead_code)` because each integration-test binary
//! compiles its own copy and only uses the subset it needs.

#![allow(dead_code)]

use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;
use std::time::{Duration, Instant};

use serde::Deserialize;
use serde_json::{Value, json};

const TIMEOUT: Duration = Duration::from_secs(30);

/// A running `compact-lsp` subprocess speaking JSON-RPC on stdio.
pub struct LspClient {
    child: Child,
    stdin: ChildStdin,
    rx: Receiver<Value>,
    next_id: i64,
}

impl LspClient {
    pub fn start() -> Self {
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

    pub fn send(&mut self, message: Value) {
        let body = serde_json::to_string(&message).unwrap();
        self.send_raw(&body);
    }

    /// Write an arbitrary body as an LSP frame (for malformed-JSON tests).
    pub fn send_raw(&mut self, body: &str) {
        write!(self.stdin, "Content-Length: {}\r\n\r\n{}", body.len(), body)
            .expect("write LSP message");
        self.stdin.flush().expect("flush LSP message");
    }

    /// Receive the next message within `timeout`, if any.
    pub fn try_recv(&mut self, timeout: Duration) -> Option<Value> {
        self.rx.recv_timeout(timeout).ok()
    }

    pub fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    pub fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));

        self.wait_for_response(id, method)
    }

    /// Send a raw JSON-RPC message and wait for the response with `id`.
    pub fn request_raw(&mut self, message: Value, id: i64, method: &str) -> Value {
        self.send(message);
        self.wait_for_response(id, method)
    }

    fn wait_for_response(&mut self, id: i64, method: &str) -> Value {
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
    pub fn wait_for_log_message(&mut self, needle: &str) {
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

    /// Wait for the next `textDocument/publishDiagnostics` notification for
    /// `uri` and return its `diagnostics` array.
    ///
    /// The server publishes syntax diagnostics immediately and, once the
    /// compiler pass finishes, a second notification that includes the
    /// compiler's diagnostics. Call [`Self::wait_for_diagnostics_until`] when
    /// the later notification is the one under test.
    pub fn wait_for_diagnostics(&mut self, uri: &str) -> Vec<Value> {
        self.wait_for_diagnostics_until(uri, |_| true)
    }

    /// Wait for a `publishDiagnostics` notification for `uri` whose diagnostics
    /// array satisfies `predicate`, and return the array.
    pub fn wait_for_diagnostics_until(
        &mut self,
        uri: &str,
        predicate: impl Fn(&[Value]) -> bool,
    ) -> Vec<Value> {
        let deadline = Instant::now() + TIMEOUT;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            assert!(
                !remaining.is_zero(),
                "timed out waiting for diagnostics for {uri}"
            );
            match self.rx.recv_timeout(remaining) {
                Ok(value)
                    if value.get("method") == Some(&json!("textDocument/publishDiagnostics"))
                        && value.pointer("/params/uri").and_then(Value::as_str) == Some(uri) =>
                {
                    let diagnostics = value
                        .pointer("/params/diagnostics")
                        .and_then(Value::as_array)
                        .cloned()
                        .unwrap_or_default();
                    if predicate(&diagnostics) {
                        return diagnostics;
                    }
                }
                Ok(_) => continue,
                Err(e) => panic!("timed out waiting for diagnostics for {uri}: {e}"),
            }
        }
    }

    /// Assert no `publishDiagnostics` notification for `uri` arrives within
    /// `window` (used to prove a pending debounce was cancelled).
    pub fn assert_no_diagnostics_for(&mut self, uri: &str, window: Duration) {
        let deadline = Instant::now() + window;
        loop {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return;
            }
            match self.rx.recv_timeout(remaining) {
                Ok(value)
                    if value.get("method") == Some(&json!("textDocument/publishDiagnostics"))
                        && value.pointer("/params/uri").and_then(Value::as_str) == Some(uri) =>
                {
                    panic!("unexpected diagnostics for {uri} after close: {value}");
                }
                Ok(_) => continue,
                Err(_) => return,
            }
        }
    }

    /// Wait until the child process exits, returning its status.
    pub fn wait_for_exit(&mut self, timeout: Duration) -> Option<std::process::ExitStatus> {
        let deadline = Instant::now() + timeout;
        loop {
            if let Ok(Some(status)) = self.child.try_wait() {
                return Some(status);
            }
            if Instant::now() >= deadline {
                return None;
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    /// Send `exit` and wait for the process to terminate.
    pub fn shutdown(mut self) {
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

/// Byte offset in `content` for `(line, character)`, where `character` counts
/// UTF-16 code units (the LSP default offset encoding).
pub fn offset_of(content: &str, line: u32, character: u32) -> usize {
    let mut offset = 0usize;
    for _ in 0..line {
        match content[offset..].find('\n') {
            Some(nl) => offset += nl + 1,
            None => return content.len(),
        }
    }
    let rest = &content[offset..];
    let line_end = rest.find('\n').unwrap_or(rest.len());
    let line_text = &rest[..line_end];
    let mut utf16 = 0u32;
    for (byte, ch) in line_text.char_indices() {
        if utf16 >= character {
            return offset + byte;
        }
        utf16 += ch.len_utf16() as u32;
    }
    offset + line_text.len()
}

pub fn line_col(source: &str, line: u32, needle: &str, offset: usize) -> u32 {
    let text = source.lines().nth(line as usize).expect("line exists");
    (text.find(needle).expect("needle exists") + offset) as u32
}

/// Apply a single LSP `TextEdit` to `content`.
pub fn apply_text_edit(content: &str, edit: &Value) -> String {
    apply_text_edits(content, std::slice::from_ref(edit))
}

/// Apply LSP `TextEdit`s to `content`, in reverse document order so indices
/// stay valid.
pub fn apply_text_edits(content: &str, edits: &[Value]) -> String {
    let mut sorted: Vec<&Value> = edits.iter().collect();
    sorted.sort_by_key(|edit| {
        let line = edit
            .pointer("/range/start/line")
            .and_then(Value::as_u64)
            .unwrap_or(0);
        let character = edit
            .pointer("/range/start/character")
            .and_then(Value::as_u64)
            .unwrap_or(0);
        std::cmp::Reverse((line, character))
    });

    let mut out = content.to_string();
    for edit in sorted {
        let start_line = edit
            .pointer("/range/start/line")
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        let start_char = edit
            .pointer("/range/start/character")
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        let end_line = edit
            .pointer("/range/end/line")
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        let end_char = edit
            .pointer("/range/end/character")
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        // `TextEdit`s use `newText`; `didChange` content changes use `text`.
        let new_text = edit
            .get("newText")
            .or_else(|| edit.get("text"))
            .and_then(Value::as_str)
            .unwrap_or("");
        let start = offset_of(&out, start_line, start_char);
        let end = offset_of(&out, end_line, end_char);
        out.replace_range(start..end, new_text);
    }
    out
}

/// Apply a `WorkspaceEdit` (`changes` map) to files on disk, returning the new
/// content keyed by URI.
pub fn apply_workspace_edit_to_disk(edit: &Value) -> Vec<(String, String)> {
    let mut updated = Vec::new();
    let Some(changes) = edit.get("changes").and_then(Value::as_object) else {
        return updated;
    };
    for (uri, edits) in changes {
        let Some(path) = uri.strip_prefix("file://") else {
            continue;
        };
        let Ok(content) = std::fs::read_to_string(path) else {
            continue;
        };
        let edits = edits.as_array().cloned().unwrap_or_default();
        let new_content = apply_text_edits(&content, &edits);
        std::fs::write(path, &new_content).expect("write edited file");
        updated.push((uri.clone(), new_content));
    }
    updated.sort();
    updated
}

/// Whether a Compact compiler is detectable by the same logic the server uses.
pub fn compiler_available() -> bool {
    compact_analyzer::DiagnosticEngine::new().is_available()
}

/// Whether `format-compact` is detectable by the same logic the server uses.
pub fn formatter_available() -> bool {
    compact_analyzer::FormatterEngine::new().is_available()
}

/// Skip a formatter-dependent test when no `format-compact` is present.
///
/// With `COMPACT_LSP_REQUIRE_FORMATTER=1` set, a missing formatter is a hard
/// failure instead of a silent skip.
pub fn require_formatter() -> bool {
    if formatter_available() {
        return true;
    }
    if std::env::var_os("COMPACT_LSP_REQUIRE_FORMATTER").is_some() {
        panic!("COMPACT_LSP_REQUIRE_FORMATTER is set but no format-compact was detected");
    }
    eprintln!("skipping: no format-compact detected");
    false
}

/// Line and character of the first occurrence of `needle` in `source`.
pub fn position_of(source: &str, needle: &str) -> (u32, u32) {
    for (line, text) in source.lines().enumerate() {
        if let Some(character) = text.find(needle) {
            return (line as u32, character as u32);
        }
    }
    panic!("needle `{needle}` not found")
}

/// Skip a compiler-dependent test when no compiler is present.
///
/// Returns `true` when the test should continue. With
/// `COMPACT_LSP_REQUIRE_COMPILER=1` set (CI probe matrix) a missing compiler is
/// a hard failure instead of a silent skip.
pub fn require_compiler() -> bool {
    if compiler_available() {
        return true;
    }
    if std::env::var_os("COMPACT_LSP_REQUIRE_COMPILER").is_some() {
        panic!("COMPACT_LSP_REQUIRE_COMPILER is set but no Compact compiler was detected");
    }
    eprintln!("skipping: no Compact compiler detected");
    false
}

pub fn initialize(client: &mut LspClient, root_uri: &str) {
    let _ = initialize_and_capture(client, root_uri);
}

/// `initialize` + `initialized`, returning the `InitializeResult`.
pub fn initialize_and_capture(client: &mut LspClient, root_uri: &str) -> Value {
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
    init
}

pub fn open(client: &mut LspClient, uri: &str, text: &str) {
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

/// Open a document and wait until the server publishes its diagnostics, which
/// guarantees the `didOpen` notification has been fully processed before the
/// next request (requests and notifications are handled concurrently).
pub fn open_and_wait(client: &mut LspClient, uri: &str, text: &str) {
    open(client, uri, text);
    client.wait_for_diagnostics(uri);
}

pub fn close(client: &mut LspClient, uri: &str) {
    client.notify(
        "textDocument/didClose",
        json!({ "textDocument": { "uri": uri } }),
    );
}

pub fn save(client: &mut LspClient, uri: &str) {
    client.notify(
        "textDocument/didSave",
        json!({ "textDocument": { "uri": uri } }),
    );
}

/// Send a `didChange` with explicit `contentChanges`.
pub fn change(client: &mut LspClient, uri: &str, version: i32, changes: Vec<Value>) {
    client.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": version },
            "contentChanges": changes
        }),
    );
}

/// Send an incremental `didChange` replacing the given range.
#[allow(clippy::too_many_arguments)]
pub fn change_range(
    client: &mut LspClient,
    uri: &str,
    version: i32,
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
    text: &str,
) {
    change(
        client,
        uri,
        version,
        vec![json!({
            "range": {
                "start": { "line": start_line, "character": start_char },
                "end": { "line": end_line, "character": end_char }
            },
            "text": text
        })],
    );
}

/// Send a full-document `didChange`.
pub fn change_full(client: &mut LspClient, uri: &str, version: i32, text: &str) {
    change(client, uri, version, vec![json!({ "text": text })]);
}

// ---------------------------------------------------------------------------
// Fixture corpus helpers
// ---------------------------------------------------------------------------

/// The parsed form of `tests/fixtures/expected.toml`.
#[derive(Debug, Deserialize)]
pub struct FixtureManifest {
    pub compiles: Vec<String>,
    #[serde(default)]
    pub fails: Vec<FixtureFailure>,
    #[serde(default)]
    pub gated: Vec<FixtureGated>,
}

#[derive(Debug, Deserialize)]
pub struct FixtureFailure {
    pub file: String,
    pub line: u32,
    pub character: u32,
    pub message_contains: String,
}

#[derive(Debug, Deserialize)]
pub struct FixtureGated {
    pub file: String,
    pub min_language_version: String,
    pub above: String,
    pub below: String,
    #[serde(default)]
    pub below_message_contains: Option<String>,
}

pub fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

pub fn load_fixture_manifest() -> FixtureManifest {
    let text = std::fs::read_to_string(fixtures_dir().join("expected.toml"))
        .expect("read fixtures/expected.toml");
    toml::from_str(&text).expect("parse fixtures/expected.toml")
}

/// Recursively copy `from` into `to`.
pub fn copy_dir(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create fixture temp dir");
    for entry in std::fs::read_dir(from).expect("read fixtures") {
        let entry = entry.expect("fixture entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("file type").is_dir() {
            copy_dir(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), target).expect("copy fixture");
        }
    }
}

/// Copy the fixture corpus into `temp` and return the copied root.
pub fn copy_fixtures_to(temp: &Path) -> PathBuf {
    let root = temp.join("fixtures");
    copy_dir(&fixtures_dir(), &root);
    root
}

/// Read a fixture relative to `root`, returning `(uri, content)` where `uri`
/// points at the on-disk copy.
pub fn read_fixture(root: &Path, relative: &str) -> (String, String) {
    let path = root.join(relative);
    let content = std::fs::read_to_string(&path).expect("read fixture");
    let uri = format!("file://{}", path.display());
    (uri, content)
}

/// All fixture files referenced by the manifest, in stable order.
pub fn manifest_files(manifest: &FixtureManifest) -> Vec<String> {
    let mut files: Vec<String> = manifest.compiles.clone();
    files.extend(manifest.fails.iter().map(|f| f.file.clone()));
    files.extend(manifest.gated.iter().map(|g| g.file.clone()));
    files
}

/// Extract `(uri, start line)` pairs from a `textDocument/references` response.
pub fn location_pairs(value: &Value) -> Vec<(String, u32)> {
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
