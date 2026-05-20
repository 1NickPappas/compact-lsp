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
            })
            .collect();

        ParsedStdlib { structs, circuits }
    })
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

/// Return all stdlib struct types.
pub fn all_stdlib_structs() -> &'static [StdlibStruct] {
    &stdlib().structs
}

/// Return all stdlib circuit functions.
pub fn all_stdlib_circuits() -> &'static [StdlibCircuit] {
    &stdlib().circuits
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
mod tests {
    use super::*;

    #[test]
    fn test_all_structs_have_fields() {
        for st in all_stdlib_structs() {
            assert!(!st.name.is_empty(), "Struct name must not be empty");
            assert!(
                !st.fields.is_empty(),
                "Struct {} must have at least one field",
                st.name
            );
            assert!(
                !st.description.is_empty(),
                "Struct {} must have a description",
                st.name
            );
            assert!(
                !st.doc_url.is_empty(),
                "Struct {} must have a doc_url",
                st.name
            );
        }
    }

    #[test]
    fn test_all_circuits_have_required_fields() {
        for circ in all_stdlib_circuits() {
            assert!(!circ.name.is_empty(), "Circuit name must not be empty");
            assert!(
                !circ.signature.is_empty(),
                "Circuit {} must have a signature",
                circ.name
            );
            assert!(
                !circ.snippet.is_empty(),
                "Circuit {} must have a snippet",
                circ.name
            );
            assert!(
                !circ.doc.is_empty(),
                "Circuit {} must have a doc",
                circ.name
            );
        }
    }

    #[test]
    fn test_find_stdlib_struct_maybe() {
        let st = find_stdlib_struct("Maybe").expect("Maybe should exist");
        assert_eq!(st.name, "Maybe");
        assert_eq!(st.type_params, "<T>");
        let field_names: Vec<&str> = st.fields.iter().map(|f| f.name).collect();
        assert!(field_names.contains(&"isSome"));
        assert!(field_names.contains(&"value"));
    }

    #[test]
    fn test_find_stdlib_struct_unknown() {
        assert!(find_stdlib_struct("NonExistent").is_none());
    }

    #[test]
    fn test_find_stdlib_circuit_some() {
        let circ = find_stdlib_circuit("some").expect("some should exist");
        assert_eq!(circ.name, "some");
        assert!(circ.signature.contains("Maybe<T>"));
    }

    #[test]
    fn test_find_stdlib_circuit_unknown() {
        assert!(find_stdlib_circuit("nonExistent").is_none());
    }

    #[test]
    fn test_generate_doc_file_coininfo() {
        let info = doc_cache()
            .structs
            .get("CoinInfo")
            .expect("CoinInfo doc should exist");
        let content = std::fs::read_to_string(&info.path).expect("CoinInfo.md should be readable");

        assert!(content.starts_with("# CoinInfo\n"));
        assert!(content.contains("Coin information type"));
        assert!(content.contains("[Compact Standard Library]"));
        assert!(content.contains("## Fields"));
        assert!(content.contains("### `nonce: Bytes<32>`"));
        assert!(content.contains("### `color: Bytes<32>`"));
        assert!(content.contains("### `value: Uint<128>`"));
    }

    #[test]
    fn test_generate_doc_file_circuits() {
        let cache = doc_cache();
        let content = std::fs::read_to_string(&cache.circuits.path)
            .expect("StdlibCircuits.md should be readable");

        assert!(content.starts_with("# Standard Library Circuits\n"));
        assert!(content.contains("## `circuit some<T>(value: T): Maybe<T>`"));
        assert!(content.contains("## `circuit send("));
        assert!(content.contains("## `circuit blockTimeLt("));
    }

    #[test]
    fn test_get_stdlib_struct_doc_location() {
        let loc = get_stdlib_struct_doc_location("CoinInfo");
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert!(loc.uri.starts_with("file://"));
        assert!(loc.uri.ends_with("CoinInfo.md"));
        assert_eq!(loc.line, 0);
    }

    #[test]
    fn test_get_stdlib_circuit_doc_location() {
        let loc = get_stdlib_circuit_doc_location("send");
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert!(loc.uri.ends_with("StdlibCircuits.md"));
        assert!(loc.line > 0);
    }

    #[test]
    fn test_field_line_numbers_are_correct() {
        for (struct_name, info) in &doc_cache().structs {
            let content = std::fs::read_to_string(&info.path).expect("Doc file should be readable");
            let lines: Vec<&str> = content.lines().collect();

            for (field_name, &line_num) in &info.field_lines {
                let line = lines.get(line_num as usize).unwrap_or_else(|| {
                    panic!(
                        "Line {} for {}.{} is out of range (file has {} lines)",
                        line_num,
                        struct_name,
                        field_name,
                        lines.len()
                    )
                });
                assert!(
                    line.starts_with("### `"),
                    "Line {} for {}.{} should start with '### `', got: {}",
                    line_num,
                    struct_name,
                    field_name,
                    line
                );
            }
        }
    }

    #[test]
    fn test_circuit_line_numbers_are_correct() {
        let cache = doc_cache();
        let content = std::fs::read_to_string(&cache.circuits.path)
            .expect("StdlibCircuits.md should be readable");
        let lines: Vec<&str> = content.lines().collect();

        for (circuit_name, &line_num) in &cache.circuits.circuit_lines {
            let line = lines.get(line_num as usize).unwrap_or_else(|| {
                panic!(
                    "Line {} for circuit {} is out of range (file has {} lines)",
                    line_num,
                    circuit_name,
                    lines.len()
                )
            });
            assert!(
                line.starts_with("## `circuit "),
                "Line {} for circuit {} should start with '## `circuit ', got: {}",
                line_num,
                circuit_name,
                line
            );
        }
    }
}
