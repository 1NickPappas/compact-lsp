//! Helpers helpers for the server.

use super::*;

/// Map a parser symbol kind to the LSP `SymbolKind` used for outline/workspace symbols.
pub(super) fn completion_kind_to_symbol_kind(kind: CompletionSymbolKind) -> SymbolKind {
    match kind {
        CompletionSymbolKind::Function => SymbolKind::FUNCTION,
        CompletionSymbolKind::Struct => SymbolKind::STRUCT,
        CompletionSymbolKind::Enum => SymbolKind::ENUM,
        CompletionSymbolKind::Variable => SymbolKind::VARIABLE,
        CompletionSymbolKind::Module => SymbolKind::MODULE,
        CompletionSymbolKind::Contract => SymbolKind::CLASS,
    }
}
/// Convert a parser `SymbolLocation` (0-based line/char) to an LSP `Range`.
pub(super) fn symbol_location_to_range(loc: &SymbolLocation) -> Range {
    Range {
        start: Position {
            line: loc.start_line,
            character: loc.start_char,
        },
        end: Position {
            line: loc.end_line,
            character: loc.end_char,
        },
    }
}
/// Whether a symbol name matches a workspace-symbol query (case-insensitive
/// substring; an empty query matches everything). `query` must be lowercased.
pub(super) fn symbol_matches_query(name: &str, query: &str) -> bool {
    query.is_empty() || name.to_lowercase().contains(query)
}
/// Build the completion items contributed by the standard-library registry,
/// gated by the detected language version.
///
/// Extracted from `completion` so the gating wiring (the version is actually
/// threaded through to the `_for` lookups) can be unit-tested without a client.
pub(super) fn stdlib_completion_items(language: Option<LanguageVersion>) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for st in stdlib::all_stdlib_structs_for(language) {
        // Show type params in the label (e.g. "Maybe<T>"); empty for
        // non-generic structs. insert_text stays the bare name.
        items.push(CompletionItem {
            label: format!("{}{}", st.name, st.type_params),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some(st.description.to_string()),
            insert_text: Some(st.name.to_string()),
            ..Default::default()
        });
    }

    for circ in stdlib::all_stdlib_circuits_for(language) {
        items.push(CompletionItem {
            label: circ.name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(circ.signature.to_string()),
            documentation: Some(Documentation::String(circ.doc.to_string())),
            insert_text: Some(circ.snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    items
}
/// Markdown hover for a standard-library circuit, gated by language version.
pub(super) fn stdlib_circuit_hover_text(
    name: &str,
    language: Option<LanguageVersion>,
) -> Option<String> {
    let circ = stdlib::find_stdlib_circuit_for(name, language)?;
    let mut text = format!(
        "```compact\ncircuit {}\n```\n\n{}",
        circ.signature, circ.doc
    );
    if !circ.doc_url.is_empty() {
        text.push_str(&format!(
            "\n\n> [Compact Standard Library]({})",
            circ.doc_url
        ));
    }
    Some(text)
}
/// Markdown hover for a standard-library struct, gated by language version.
pub(super) fn stdlib_struct_hover_text(
    name: &str,
    language: Option<LanguageVersion>,
) -> Option<String> {
    let st = stdlib::find_stdlib_struct_for(name, language)?;
    let mut text = if st.type_params.is_empty() {
        format!("**{}**\n\n{}\n\n", st.name, st.description)
    } else {
        format!(
            "**{}{}**\n\n{}\n\n",
            st.name, st.type_params, st.description
        )
    };
    text.push_str("**Fields:**\n");
    for field in &st.fields {
        text.push_str(&format!(
            "- `{}: {}` — {}\n",
            field.name, field.type_str, field.doc
        ));
    }
    if !st.doc_url.is_empty() {
        text.push_str(&format!("\n> [Compact Standard Library]({})", st.doc_url));
    }
    Some(text)
}
/// Signature help for a standard-library circuit, gated by language version.
pub(super) fn stdlib_signature_help(
    func_name: &str,
    active_param: u32,
    language: Option<LanguageVersion>,
) -> Option<SignatureHelp> {
    let circ = stdlib::find_stdlib_circuit_for(func_name, language)?;

    // Build detail from the signature: "sendShielded(input: …, …): ShieldedSendResult"
    // -> "(input: …, …): ShieldedSendResult".
    let detail = circ
        .signature
        .find('(')
        .map(|i| &circ.signature[i..])
        .unwrap_or(circ.signature);
    let params = utils::parse_params_from_detail(detail);

    let parameters: Vec<ParameterInformation> = params
        .iter()
        .map(|p| ParameterInformation {
            label: ParameterLabel::Simple(p.clone()),
            documentation: None,
        })
        .collect();

    let signature = SignatureInformation {
        label: format!("circuit {}", circ.signature),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: circ.doc.to_string(),
        })),
        parameters: Some(parameters),
        active_parameter: Some(active_param),
    };

    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}
/// Canonical filesystem key for a `file://` URI, tolerating canonicalization
/// differences (e.g. macOS `/var` vs `/private/var`).
pub(super) fn canonical_key(uri: &str) -> String {
    match uri.strip_prefix("file://") {
        Some(p) => {
            let path = Path::new(p);
            path.canonicalize()
                .unwrap_or_else(|_| path.to_path_buf())
                .to_string_lossy()
                .into_owned()
        }
        None => uri.to_string(),
    }
}
/// Compare two `file://` URIs, tolerating canonicalization differences (e.g.
/// macOS `/var` vs `/private/var`).
pub(super) fn same_file(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    match (a.strip_prefix("file://"), b.strip_prefix("file://")) {
        (Some(pa), Some(pb)) => {
            let pa = Path::new(pa);
            let pb = Path::new(pb);
            let ca = pa.canonicalize().unwrap_or_else(|_| pa.to_path_buf());
            let cb = pb.canonicalize().unwrap_or_else(|_| pb.to_path_buf());
            ca == cb
        }
        _ => false,
    }
}
