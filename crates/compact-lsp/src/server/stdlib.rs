// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Compact standard library type and circuit registry.
//!
//! Definitions live in `stdlib.toml` (embedded at compile time).
//! This module parses the TOML once on first access and exposes:
//! - Struct/circuit lookup for completions, hover, and signature help
//! - Generated markdown doc files for go-to-definition

use super::builtins::BuiltinDocLocation;
use compact_analyzer::LanguageVersion;
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::OnceLock;

// ── Public data types ───────────────────────────────────────────────

/// A standard library struct type.
pub struct StdlibStruct {
    pub name: &'static str,
    pub type_params: &'static str,
    pub description: &'static str,
    pub doc_url: &'static str,
    /// Minimum language version required to use this symbol.
    /// Empty means it is available in every version.
    pub min_language_version: &'static str,
    /// Exclusive upper bound: the language version in which this symbol was
    /// removed. Empty means it is not removed in any supported version.
    pub max_language_version: &'static str,
    pub fields: Vec<StdlibField>,
}

/// A field on a standard library struct.
pub struct StdlibField {
    pub name: &'static str,
    pub type_str: &'static str,
    pub doc: &'static str,
}

/// A standard library circuit function.
pub struct StdlibCircuit {
    pub name: &'static str,
    #[allow(dead_code)]
    pub type_params: &'static str,
    pub signature: &'static str,
    pub snippet: &'static str,
    pub doc: &'static str,
    pub doc_url: &'static str,
    /// Minimum language version required to use this symbol.
    /// Empty means it is available in every version.
    pub min_language_version: &'static str,
    /// Exclusive upper bound: the language version in which this symbol was
    /// removed. Empty means it is not removed in any supported version.
    pub max_language_version: &'static str,
}

// ── TOML deserialization structures ─────────────────────────────────

#[derive(Deserialize)]
struct StdlibRegistry {
    structs: Vec<StructDef>,
    circuits: Vec<CircuitDef>,
}

#[derive(Deserialize)]
struct StructDef {
    name: String,
    #[serde(default)]
    type_params: String,
    #[serde(default)]
    description: String,
    #[serde(default)]
    doc_url: String,
    #[serde(default)]
    min_language_version: String,
    #[serde(default)]
    max_language_version: String,
    fields: Vec<FieldDef>,
}

#[derive(Deserialize)]
struct FieldDef {
    name: String,
    type_str: String,
    doc: String,
}

#[derive(Deserialize)]
struct CircuitDef {
    name: String,
    #[serde(default)]
    type_params: String,
    signature: String,
    snippet: String,
    doc: String,
    #[serde(default)]
    doc_url: String,
    #[serde(default)]
    min_language_version: String,
    #[serde(default)]
    max_language_version: String,
}

// ── Parsed + converted registry ─────────────────────────────────────

struct ParsedStdlib {
    structs: Vec<StdlibStruct>,
    circuits: Vec<StdlibCircuit>,
}

static STDLIB: OnceLock<ParsedStdlib> = OnceLock::new();

fn stdlib() -> &'static ParsedStdlib {
    STDLIB.get_or_init(|| {
        let raw: StdlibRegistry =
            toml::from_str(include_str!("stdlib.toml")).expect("stdlib.toml is invalid TOML");

        let structs = raw
            .structs
            .into_iter()
            .map(|s| StdlibStruct {
                name: Box::leak(s.name.into_boxed_str()),
                type_params: Box::leak(s.type_params.into_boxed_str()),
                description: Box::leak(s.description.into_boxed_str()),
                doc_url: Box::leak(s.doc_url.into_boxed_str()),
                min_language_version: Box::leak(s.min_language_version.into_boxed_str()),
                max_language_version: Box::leak(s.max_language_version.into_boxed_str()),
                fields: s
                    .fields
                    .into_iter()
                    .map(|f| StdlibField {
                        name: Box::leak(f.name.into_boxed_str()),
                        type_str: Box::leak(f.type_str.into_boxed_str()),
                        doc: Box::leak(f.doc.into_boxed_str()),
                    })
                    .collect(),
            })
            .collect();

        let circuits = raw
            .circuits
            .into_iter()
            .map(|c| StdlibCircuit {
                name: Box::leak(c.name.into_boxed_str()),
                type_params: Box::leak(c.type_params.into_boxed_str()),
                signature: Box::leak(c.signature.into_boxed_str()),
                snippet: Box::leak(c.snippet.into_boxed_str()),
                doc: Box::leak(c.doc.into_boxed_str()),
                doc_url: Box::leak(c.doc_url.into_boxed_str()),
                min_language_version: Box::leak(c.min_language_version.into_boxed_str()),
                max_language_version: Box::leak(c.max_language_version.into_boxed_str()),
            })
            .collect();

        ParsedStdlib { structs, circuits }
    })
}

// ── Version gating ──────────────────────────────────────────────────

/// Whether an entry restricted to `[min_language_version, max_language_version)`
/// is available for a detected `language` version.
///
/// - An empty or unparseable bound means "no bound on that side".
/// - `max_language_version` is **exclusive**: set it to the language version in
///   which a symbol was removed (the symbol is hidden at that version and newer).
/// - An unknown detected version (no compiler) disables gating, so symbols are
///   shown rather than hidden when the target cannot be verified.
pub(crate) fn is_available_range(
    min_language_version: &str,
    max_language_version: &str,
    language: Option<LanguageVersion>,
) -> bool {
    let Some(detected) = language else {
        return true;
    };
    if !min_language_version.is_empty()
        && let Some(min) = LanguageVersion::parse(min_language_version)
        && detected < min
    {
        return false;
    }
    if !max_language_version.is_empty()
        && let Some(max) = LanguageVersion::parse(max_language_version)
        && detected >= max
    {
        return false;
    }
    true
}

// ── Lookup API ──────────────────────────────────────────────────────

/// Find a stdlib struct by name.
pub fn find_stdlib_struct(name: &str) -> Option<&'static StdlibStruct> {
    stdlib().structs.iter().find(|s| s.name == name)
}

/// Find a stdlib circuit by name.
pub fn find_stdlib_circuit(name: &str) -> Option<&'static StdlibCircuit> {
    stdlib().circuits.iter().find(|c| c.name == name)
}

/// The declared type of a field on a stdlib struct, with the receiver's
/// generic arguments substituted (`Maybe<Token>.value` → `Token`,
/// `Either<A, B>.left` → `A`).
pub fn struct_field_type(
    struct_name: &str,
    type_args: &[String],
    field_name: &str,
) -> Option<String> {
    let st = find_stdlib_struct(struct_name)?;
    let field = st.fields.iter().find(|f| f.name == field_name)?;
    Some(super::builtins::substitute_generic_params(
        field.type_str,
        st.type_params,
        type_args,
    ))
}

/// Find a stdlib struct by name, respecting the detected language version.
pub fn find_stdlib_struct_for(
    name: &str,
    language: Option<LanguageVersion>,
) -> Option<&'static StdlibStruct> {
    let st = find_stdlib_struct(name)?;
    is_available_range(st.min_language_version, st.max_language_version, language).then_some(st)
}

/// Find a stdlib circuit by name, respecting the detected language version.
pub fn find_stdlib_circuit_for(
    name: &str,
    language: Option<LanguageVersion>,
) -> Option<&'static StdlibCircuit> {
    let circ = find_stdlib_circuit(name)?;
    is_available_range(
        circ.min_language_version,
        circ.max_language_version,
        language,
    )
    .then_some(circ)
}

/// Return all stdlib struct types.
pub fn all_stdlib_structs() -> &'static [StdlibStruct] {
    &stdlib().structs
}

/// Return all stdlib circuit functions.
pub fn all_stdlib_circuits() -> &'static [StdlibCircuit] {
    &stdlib().circuits
}

/// Return stdlib struct types available for the detected language version.
pub fn all_stdlib_structs_for(language: Option<LanguageVersion>) -> Vec<&'static StdlibStruct> {
    all_stdlib_structs()
        .iter()
        .filter(|s| is_available_range(s.min_language_version, s.max_language_version, language))
        .collect()
}

/// Return stdlib circuit functions available for the detected language version.
pub fn all_stdlib_circuits_for(language: Option<LanguageVersion>) -> Vec<&'static StdlibCircuit> {
    all_stdlib_circuits()
        .iter()
        .filter(|c| is_available_range(c.min_language_version, c.max_language_version, language))
        .collect()
}

// ── Doc file generation ─────────────────────────────────────────────

struct StructDocInfo {
    path: String,
    field_lines: HashMap<String, u32>,
}

struct CircuitsDocInfo {
    path: String,
    circuit_lines: HashMap<String, u32>,
}

struct StdlibDocCache {
    structs: HashMap<String, StructDocInfo>,
    circuits: CircuitsDocInfo,
}

static DOC_CACHE: OnceLock<StdlibDocCache> = OnceLock::new();

fn doc_cache() -> &'static StdlibDocCache {
    DOC_CACHE.get_or_init(|| {
        let dir = std::env::temp_dir().join("compact-lsp-docs");
        if let Err(e) = std::fs::create_dir_all(&dir) {
            tracing::warn!("Failed to create doc dir {:?}: {}", dir, e);
            return StdlibDocCache {
                structs: HashMap::new(),
                circuits: CircuitsDocInfo {
                    path: String::new(),
                    circuit_lines: HashMap::new(),
                },
            };
        }

        let reg = stdlib();

        // ── Per-struct doc files ────────────────────────────────
        let mut struct_cache = HashMap::new();
        for st in &reg.structs {
            let mut content = String::new();
            let mut field_lines: HashMap<String, u32> = HashMap::new();
            let mut line: u32 = 0;

            // # TypeName<Params>
            if st.type_params.is_empty() {
                content.push_str(&format!("# {}\n", st.name));
            } else {
                content.push_str(&format!("# {}{}\n", st.name, st.type_params));
            }
            line += 1;

            content.push('\n');
            line += 1;

            if !st.description.is_empty() {
                content.push_str(st.description);
                content.push('\n');
                line += 1;

                content.push('\n');
                line += 1;
            }

            if !st.doc_url.is_empty() {
                content.push_str(&format!(
                    "> Full reference: [Compact Standard Library]({})\n",
                    st.doc_url
                ));
                line += 1;

                content.push('\n');
                line += 1;
            }

            // ## Fields
            content.push_str("## Fields\n");
            line += 1;

            content.push('\n');
            line += 1;

            for field in &st.fields {
                field_lines.insert(field.name.to_string(), line);
                content.push_str(&format!("### `{}: {}`\n", field.name, field.type_str));
                line += 1;

                content.push('\n');
                line += 1;

                content.push_str(field.doc);
                content.push('\n');
                line += 1;

                content.push('\n');
                line += 1;
            }

            let file_path = dir.join(format!("{}.md", st.name));
            if let Err(e) = std::fs::write(&file_path, &content) {
                tracing::warn!("Failed to write doc file {:?}: {}", file_path, e);
                continue;
            }

            struct_cache.insert(
                st.name.to_string(),
                StructDocInfo {
                    path: file_path.to_string_lossy().into_owned(),
                    field_lines,
                },
            );
        }

        // ── Single circuits doc file ────────────────────────────
        let mut content = String::new();
        let mut circuit_lines: HashMap<String, u32> = HashMap::new();
        let mut line: u32 = 0;

        content.push_str("# Standard Library Circuits\n");
        line += 1;

        content.push('\n');
        line += 1;

        content.push_str("> Full reference: [Compact Standard Library](https://docs.midnight.network/develop/reference/compact/compact-std-library/exports)\n");
        line += 1;

        content.push('\n');
        line += 1;

        for circ in &reg.circuits {
            circuit_lines.insert(circ.name.to_string(), line);
            content.push_str(&format!("## `circuit {}`\n", circ.signature));
            line += 1;

            content.push('\n');
            line += 1;

            content.push_str(circ.doc);
            content.push('\n');
            line += 1;

            content.push('\n');
            line += 1;
        }

        let circuits_path = dir.join("StdlibCircuits.md");
        if let Err(e) = std::fs::write(&circuits_path, &content) {
            tracing::warn!("Failed to write circuits doc file: {}", e);
        }

        StdlibDocCache {
            structs: struct_cache,
            circuits: CircuitsDocInfo {
                path: circuits_path.to_string_lossy().into_owned(),
                circuit_lines,
            },
        }
    })
}

// ── Doc location API ────────────────────────────────────────────────

/// Return a doc location for a stdlib struct type (navigates to the type header).
pub fn get_stdlib_struct_doc_location(name: &str) -> Option<BuiltinDocLocation> {
    let info = doc_cache().structs.get(name)?;
    Some(BuiltinDocLocation {
        uri: format!("file://{}", info.path),
        line: 0,
    })
}

/// Return a doc location for a field on a stdlib struct.
#[allow(dead_code)]
pub fn get_stdlib_field_doc_location(
    struct_name: &str,
    field_name: &str,
) -> Option<BuiltinDocLocation> {
    let info = doc_cache().structs.get(struct_name)?;
    let &line = info.field_lines.get(field_name)?;
    Some(BuiltinDocLocation {
        uri: format!("file://{}", info.path),
        line,
    })
}

/// Return a doc location for a stdlib circuit function.
pub fn get_stdlib_circuit_doc_location(name: &str) -> Option<BuiltinDocLocation> {
    let cache = doc_cache();
    let &line = cache.circuits.circuit_lines.get(name)?;
    Some(BuiltinDocLocation {
        uri: format!("file://{}", cache.circuits.path),
        line,
    })
}

#[cfg(test)]
mod tests;
