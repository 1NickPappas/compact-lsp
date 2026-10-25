//! Registry version gating, observed through the LSP.
//!
//! The server hides registry symbols whose `min_language_version` is newer
//! than the detected compiler's language (and symbols removed by an exclusive
//! `max_language_version`). This suite drives completion/hover on a probe
//! buffer and asserts the same thresholds the analyzer probe tests use:
//!
//! - `keccak256`  >= 0.23.103
//! - `sha512`     >= 0.26.103
//! - `kernel.caller` >= 0.26.105
//!
//! Skips when no compiler is available; `COMPACT_LSP_REQUIRE_COMPILER=1`
//! turns a missing compiler into a failure.

mod common;

use common::{LspClient, initialize, open, require_compiler};
use compact_analyzer::{DiagnosticEngine, LanguageVersion};
use serde_json::{Value, json};

/// `kernel.caller` exists only on newer compilers; `sha512` and `keccak256`
/// are used for general-completion checks.
const PROBE: &str = r#"import CompactStandardLibrary;

export circuit probe(): Maybe<PublicAddress> {
  return kernel.caller();
}

export circuit hashIt(x: Field): Bytes<64> {
  return sha512<Field>(x);
}
"#;

fn gating_probe(uri: &str) -> String {
    format!("file://{uri}/gating.compact")
}

fn completion(
    client: &mut LspClient,
    uri: &str,
    line: u32,
    character: u32,
    trigger: &str,
) -> String {
    let response = client.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character },
            "context": { "triggerKind": 2, "triggerCharacter": trigger }
        }),
    );
    response.to_string()
}

fn hover(client: &mut LspClient, uri: &str, line: u32, character: u32) -> Value {
    client.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        }),
    )
}

fn has_symbol(completions: &str, symbol: &str) -> bool {
    completions.contains(&format!("\"{symbol}\""))
}

#[tokio::test]
async fn test_registry_gating_matches_detected_language() {
    if !require_compiler() {
        return;
    }
    let language = DiagnosticEngine::new()
        .language_version()
        .expect("detected compiler must report a language version");

    let temp = tempfile::tempdir().expect("tempdir");
    let uri = gating_probe(&temp.path().to_string_lossy());

    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", temp.path().display()));
    open(&mut client, &uri, PROBE);

    // General completion inside the circuit body lists stdlib circuits.
    let general = completion(&mut client, &uri, 3, 9, " ");
    assert!(
        general.contains("probe"),
        "completion should include the local circuit: {general}"
    );
    assert_eq!(
        has_symbol(&general, "keccak256"),
        language >= LanguageVersion::new(0, 23, 103),
        "keccak256 gating wrong on language {language}: {general}"
    );
    assert_eq!(
        has_symbol(&general, "sha512"),
        language >= LanguageVersion::new(0, 26, 103),
        "sha512 gating wrong on language {language}: {general}"
    );

    // Member completion after `kernel.` lists the Kernel ledger methods.
    let kernel_dot = PROBE
        .lines()
        .nth(3)
        .expect("probe line")
        .find("kernel.")
        .expect("kernel member access")
        + "kernel.".len();
    let members = completion(&mut client, &uri, 3, kernel_dot as u32, ".");
    assert!(
        has_symbol(&members, "self"),
        "kernel member completion missing `self`: {members}"
    );
    assert_eq!(
        has_symbol(&members, "caller"),
        language >= LanguageVersion::new(0, 26, 105),
        "kernel.caller gating wrong on language {language}: {members}"
    );

    // Hover on `sha512` resolves only when the registry exposes it.
    let sha = PROBE
        .lines()
        .nth(7)
        .expect("probe line")
        .find("sha512")
        .expect("sha512 use");
    let response = hover(&mut client, &uri, 7, sha as u32);
    let resolved = response.get("result").is_some_and(|r| !r.is_null());
    assert_eq!(
        resolved,
        language >= LanguageVersion::new(0, 26, 103),
        "sha512 hover gating wrong on language {language}: {response}"
    );

    client.shutdown();
}
