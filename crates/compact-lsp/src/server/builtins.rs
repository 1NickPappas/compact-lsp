// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Built-in type method registry for dot-completion and go-to-definition.
//!
//! Method definitions live in `builtins.toml` (embedded at compile time).
//! This module parses the TOML once on first access and exposes:
//! - Method lookup for completions and hover
//! - Generated markdown doc files for go-to-definition on built-in types

use serde::Deserialize;
use std::collections::HashMap;
use std::sync::OnceLock;

/// A built-in method available on a Compact type.
pub struct BuiltinMethod {
    /// Method name (e.g., "increment").
    pub name: &'static str,
    /// Display signature (e.g., "increment(amount: Uint<64>)").
    pub signature: &'static str,
    /// Snippet for insertion (e.g., "increment(${1:amount})").
    pub snippet: &'static str,
    /// Documentation string.
    pub documentation: &'static str,
}

// ── TOML deserialization structures ─────────────────────────────────

#[derive(Deserialize)]
struct Registry {
    types: Vec<TypeDef>,
}

#[derive(Deserialize)]
struct TypeDef {
    name: String,
    #[serde(default)]
    description: String,
    #[serde(default)]
    doc_url: String,
    methods: Vec<MethodDef>,
}

#[derive(Deserialize)]
struct MethodDef {
    name: String,
    signature: String,
    snippet: String,
    doc: String,
}

// ── Parsed + converted registry ─────────────────────────────────────

struct ParsedRegistry {
    types: Vec<ParsedType>,
}

struct ParsedType {
    name: String,
    description: &'static str,
    doc_url: &'static str,
    methods: Vec<BuiltinMethod>,
}

static REGISTRY: OnceLock<ParsedRegistry> = OnceLock::new();

fn registry() -> &'static ParsedRegistry {
    REGISTRY.get_or_init(|| {
        // The TOML source is embedded at compile time — no runtime I/O.
        let raw: Registry =
            toml::from_str(include_str!("builtins.toml")).expect("builtins.toml is invalid TOML");

        // Leak strings so BuiltinMethod can hold &'static str references.
        // This happens exactly once for the lifetime of the process.
        let types = raw
            .types
            .into_iter()
            .map(|t| ParsedType {
                name: t.name,
                description: Box::leak(t.description.into_boxed_str()),
                doc_url: Box::leak(t.doc_url.into_boxed_str()),
                methods: t
                    .methods
                    .into_iter()
                    .map(|m| BuiltinMethod {
                        name: Box::leak(m.name.into_boxed_str()),
                        signature: Box::leak(m.signature.into_boxed_str()),
                        snippet: Box::leak(m.snippet.into_boxed_str()),
                        documentation: Box::leak(m.doc.into_boxed_str()),
                    })
                    .collect(),
            })
            .collect();

        ParsedRegistry { types }
    })
}

/// Return the built-in methods for a given base type name.
pub fn methods_for_type(type_name: &str) -> &'static [BuiltinMethod] {
    registry()
        .types
        .iter()
        .find(|t| t.name == type_name)
        .map(|t| t.methods.as_slice())
        .unwrap_or(&[])
}

/// Find a specific built-in method by type and method name.
pub fn find_method_by_name(type_name: &str, method_name: &str) -> Option<&'static BuiltinMethod> {
    methods_for_type(type_name)
        .iter()
        .find(|m| m.name == method_name)
}

/// Extract the base type name from a potentially parameterized type string.
///
/// For example, `"Map<Address, Uint<64>>"` returns `"Map"`,
/// and `"Counter"` returns `"Counter"`.
pub fn extract_base_type(type_str: &str) -> &str {
    match type_str.find('<') {
        Some(idx) => &type_str[..idx],
        None => type_str,
    }
}

// ── Doc file generation for go-to-definition ────────────────────────

/// Information about a generated doc file for a built-in type.
struct DocFileInfo {
    /// Absolute path to the generated markdown file.
    path: String,
    /// Method name → 0-based line number of its `### \`signature\`` heading.
    method_lines: HashMap<String, u32>,
}

/// A resolved location in a generated doc file.
pub struct BuiltinDocLocation {
    /// `file://` URI pointing to the generated markdown file.
    pub uri: String,
    /// 0-based line number to navigate to.
    pub line: u32,
}

static DOC_CACHE: OnceLock<HashMap<String, DocFileInfo>> = OnceLock::new();

/// Get or create the doc file cache. Files are generated once in a temp directory.
fn doc_cache() -> &'static HashMap<String, DocFileInfo> {
    DOC_CACHE.get_or_init(|| {
        let dir = std::env::temp_dir().join("compact-lsp-docs");
        if let Err(e) = std::fs::create_dir_all(&dir) {
            tracing::warn!("Failed to create doc dir {:?}: {}", dir, e);
            return HashMap::new();
        }

        let reg = registry();
        let mut cache = HashMap::new();

        for ptype in &reg.types {
            if ptype.methods.is_empty() {
                continue;
            }

            let mut content = String::new();
            let mut method_lines: HashMap<String, u32> = HashMap::new();
            let mut line: u32 = 0;

            // # TypeName
            content.push_str(&format!("# {}\n", ptype.name));
            line += 1;

            // blank line
            content.push('\n');
            line += 1;

            // Description
            if !ptype.description.is_empty() {
                content.push_str(ptype.description);
                content.push('\n');
                line += 1;

                content.push('\n');
                line += 1;
            }

            // Doc link
            if !ptype.doc_url.is_empty() {
                content.push_str(&format!(
                    "> Full reference: [Ledger data types]({})\n",
                    ptype.doc_url
                ));
                line += 1;

                content.push('\n');
                line += 1;
            }

            // ## Methods
            content.push_str("## Methods\n");
            line += 1;

            content.push('\n');
            line += 1;

            for method in &ptype.methods {
                // ### `signature`
                method_lines.insert(method.name.to_string(), line);
                content.push_str(&format!("### `{}`\n", method.signature));
                line += 1;

                content.push('\n');
                line += 1;

                // doc
                content.push_str(method.documentation);
                content.push('\n');
                line += 1;

                content.push('\n');
                line += 1;
            }

            let file_path = dir.join(format!("{}.md", ptype.name));
            if let Err(e) = std::fs::write(&file_path, &content) {
                tracing::warn!("Failed to write doc file {:?}: {}", file_path, e);
                continue;
            }

            cache.insert(
                ptype.name.clone(),
                DocFileInfo {
                    path: file_path.to_string_lossy().into_owned(),
                    method_lines,
                },
            );
        }

        cache
    })
}

/// Return a doc location for a built-in type name (navigates to the type header).
///
/// Returns `None` for types without methods (Boolean, Field, etc.).
pub fn get_builtin_type_doc_location(type_name: &str) -> Option<BuiltinDocLocation> {
    let info = doc_cache().get(type_name)?;
    Some(BuiltinDocLocation {
        uri: format!("file://{}", info.path),
        line: 0,
    })
}

/// Return a doc location for a method on a built-in type (navigates to the method heading).
pub fn get_builtin_method_doc_location(
    type_name: &str,
    method_name: &str,
) -> Option<BuiltinDocLocation> {
    let info = doc_cache().get(type_name)?;
    let &line = info.method_lines.get(method_name)?;
    Some(BuiltinDocLocation {
        uri: format!("file://{}", info.path),
        line,
    })
}

/// Check whether a built-in type has methods (and thus a doc file).
#[allow(dead_code)]
pub fn has_builtin_methods(type_name: &str) -> bool {
    doc_cache().contains_key(type_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_base_type_simple() {
        assert_eq!(extract_base_type("Counter"), "Counter");
    }

    #[test]
    fn test_extract_base_type_parameterized() {
        assert_eq!(extract_base_type("Map<Address, Uint<64>>"), "Map");
    }

    #[test]
    fn test_extract_base_type_single_param() {
        assert_eq!(extract_base_type("Set<Address>"), "Set");
    }

    #[test]
    fn test_methods_for_counter() {
        let methods = methods_for_type("Counter");
        assert_eq!(methods.len(), 5);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"increment"));
        assert!(names.contains(&"decrement"));
        assert!(names.contains(&"read"));
        assert!(names.contains(&"lessThan"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_cell() {
        let methods = methods_for_type("Cell");
        assert_eq!(methods.len(), 4);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"read"));
        assert!(names.contains(&"write"));
        assert!(names.contains(&"writeCoin"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_map() {
        let methods = methods_for_type("Map");
        assert_eq!(methods.len(), 9);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"insert"));
        assert!(names.contains(&"insertDefault"));
        assert!(names.contains(&"insertCoin"));
        assert!(names.contains(&"remove"));
        assert!(names.contains(&"lookup"));
        assert!(names.contains(&"member"));
        assert!(names.contains(&"isEmpty"));
        assert!(names.contains(&"size"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_set() {
        let methods = methods_for_type("Set");
        assert_eq!(methods.len(), 7);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"insert"));
        assert!(names.contains(&"insertCoin"));
        assert!(names.contains(&"remove"));
        assert!(names.contains(&"member"));
        assert!(names.contains(&"isEmpty"));
        assert!(names.contains(&"size"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_list() {
        let methods = methods_for_type("List");
        assert_eq!(methods.len(), 7);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"pushFront"));
        assert!(names.contains(&"pushFrontCoin"));
        assert!(names.contains(&"popFront"));
        assert!(names.contains(&"head"));
        assert!(names.contains(&"length"));
        assert!(names.contains(&"isEmpty"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_merkle_tree() {
        let methods = methods_for_type("MerkleTree");
        assert_eq!(methods.len(), 8);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"insert"));
        assert!(names.contains(&"insertIndex"));
        assert!(names.contains(&"insertHash"));
        assert!(names.contains(&"insertHashIndex"));
        assert!(names.contains(&"insertIndexDefault"));
        assert!(names.contains(&"checkRoot"));
        assert!(names.contains(&"isFull"));
        assert!(names.contains(&"resetToDefault"));
    }

    #[test]
    fn test_methods_for_historic_merkle_tree() {
        let methods = methods_for_type("HistoricMerkleTree");
        assert_eq!(methods.len(), 9);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"insert"));
        assert!(names.contains(&"insertIndex"));
        assert!(names.contains(&"insertHash"));
        assert!(names.contains(&"insertHashIndex"));
        assert!(names.contains(&"insertIndexDefault"));
        assert!(names.contains(&"checkRoot"));
        assert!(names.contains(&"isFull"));
        assert!(names.contains(&"resetToDefault"));
        assert!(names.contains(&"resetHistory"));
    }

    #[test]
    fn test_methods_for_kernel() {
        let methods = methods_for_type("Kernel");
        assert_eq!(methods.len(), 9);
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"self"));
        assert!(names.contains(&"mint"));
        assert!(names.contains(&"blockTimeGreaterThan"));
        assert!(names.contains(&"blockTimeLessThan"));
        assert!(names.contains(&"checkpoint"));
        assert!(names.contains(&"claimContractCall"));
        assert!(names.contains(&"claimZswapCoinReceive"));
        assert!(names.contains(&"claimZswapCoinSpend"));
        assert!(names.contains(&"claimZswapNullifier"));
    }

    #[test]
    fn test_methods_for_unknown() {
        let methods = methods_for_type("Boolean");
        assert!(methods.is_empty());
    }

    #[test]
    fn test_find_method_by_name_found() {
        let method = find_method_by_name("Counter", "increment");
        assert!(method.is_some());
        assert_eq!(method.unwrap().name, "increment");
    }

    #[test]
    fn test_find_method_by_name_not_found() {
        assert!(find_method_by_name("Counter", "unknown_method").is_none());
    }

    #[test]
    fn test_find_method_by_name_unknown_type() {
        assert!(find_method_by_name("Boolean", "increment").is_none());
    }

    #[test]
    fn test_all_methods_have_required_fields() {
        for ptype in &registry().types {
            assert!(!ptype.name.is_empty(), "Type name must not be empty");
            assert!(
                !ptype.methods.is_empty(),
                "Type {} must have at least one method",
                ptype.name
            );
            for method in &ptype.methods {
                assert!(
                    !method.name.is_empty(),
                    "{}.{}: name must not be empty",
                    ptype.name,
                    method.name
                );
                assert!(
                    !method.signature.is_empty(),
                    "{}.{}: signature must not be empty",
                    ptype.name,
                    method.name
                );
                assert!(
                    !method.snippet.is_empty(),
                    "{}.{}: snippet must not be empty",
                    ptype.name,
                    method.name
                );
                assert!(
                    !method.documentation.is_empty(),
                    "{}.{}: documentation must not be empty",
                    ptype.name,
                    method.name
                );
            }
        }
    }

    #[test]
    fn test_all_types_have_descriptions() {
        for ptype in &registry().types {
            assert!(
                !ptype.description.is_empty(),
                "Type {} must have a non-empty description",
                ptype.name
            );
            assert!(
                !ptype.doc_url.is_empty(),
                "Type {} must have a non-empty doc_url",
                ptype.name
            );
        }
    }

    #[test]
    fn test_generate_doc_file_counter() {
        let info = doc_cache()
            .get("Counter")
            .expect("Counter doc should exist");
        let content = std::fs::read_to_string(&info.path).expect("Counter.md should be readable");

        assert!(content.starts_with("# Counter\n"));
        assert!(content.contains("Simple incrementing/decrementing counter"));
        assert!(content.contains("[Ledger data types]"));
        assert!(content.contains("## Methods"));
        assert!(content.contains("### `increment(amount: Uint<16>)`"));
        assert!(content.contains("### `decrement(amount: Uint<16>)`"));
        assert!(content.contains("### `read(): Uint<64>`"));
    }

    #[test]
    fn test_get_builtin_type_doc_location() {
        // Types with methods should return Some
        let loc = get_builtin_type_doc_location("Counter");
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert!(loc.uri.starts_with("file://"));
        assert!(loc.uri.ends_with("Counter.md"));
        assert_eq!(loc.line, 0);

        // Types without methods should return None
        assert!(get_builtin_type_doc_location("Boolean").is_none());
        assert!(get_builtin_type_doc_location("Field").is_none());
    }

    #[test]
    fn test_get_builtin_method_doc_location() {
        // Known method should return Some with correct line
        let loc = get_builtin_method_doc_location("Counter", "increment");
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert!(loc.uri.ends_with("Counter.md"));
        assert!(loc.line > 0);

        // Unknown method should return None
        assert!(get_builtin_method_doc_location("Counter", "nonexistent").is_none());

        // Unknown type should return None
        assert!(get_builtin_method_doc_location("Boolean", "increment").is_none());
    }

    #[test]
    fn test_method_line_numbers_are_correct() {
        let info = doc_cache()
            .get("Counter")
            .expect("Counter doc should exist");
        let content = std::fs::read_to_string(&info.path).expect("Counter.md should be readable");
        let lines: Vec<&str> = content.lines().collect();

        for (method_name, &line_num) in &info.method_lines {
            let line = lines.get(line_num as usize).unwrap_or_else(|| {
                panic!(
                    "Line {} for method {} is out of range (file has {} lines)",
                    line_num,
                    method_name,
                    lines.len()
                )
            });
            assert!(
                line.starts_with("### `"),
                "Line {} for method {} should start with '### `', got: {}",
                line_num,
                method_name,
                line
            );
        }
    }

    #[test]
    fn test_has_builtin_methods() {
        assert!(has_builtin_methods("Counter"));
        assert!(has_builtin_methods("Cell"));
        assert!(has_builtin_methods("Map"));
        assert!(has_builtin_methods("Kernel"));
        assert!(!has_builtin_methods("Boolean"));
        assert!(!has_builtin_methods("Field"));
        assert!(!has_builtin_methods("NonExistent"));
    }
}
