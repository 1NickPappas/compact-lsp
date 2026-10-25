// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Built-in type method registry for dot-completion and go-to-definition.
//!
//! Method definitions live in `builtins.toml` (embedded at compile time).
//! This module parses the TOML once on first access and exposes:
//! - Method lookup for completions and hover
//! - Generated markdown doc files for go-to-definition on built-in types

use compact_analyzer::LanguageVersion;
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::OnceLock;

use super::stdlib::is_available_range;

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
    /// Minimum language version required to use this method.
    /// Empty means it is available in every version.
    pub min_language_version: &'static str,
    /// Exclusive upper bound: the language version in which this method was
    /// removed. Empty means it is not removed in any supported version.
    pub max_language_version: &'static str,
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
    /// Generic parameter names, e.g. `<K, V>` for `Map`. Empty when the type
    /// is not generic.
    #[serde(default)]
    type_params: String,
    methods: Vec<MethodDef>,
}

#[derive(Deserialize)]
struct MethodDef {
    name: String,
    signature: String,
    snippet: String,
    doc: String,
    #[serde(default)]
    min_language_version: String,
    #[serde(default)]
    max_language_version: String,
}

// ── Parsed + converted registry ─────────────────────────────────────

struct ParsedRegistry {
    types: Vec<ParsedType>,
}

struct ParsedType {
    name: String,
    description: &'static str,
    doc_url: &'static str,
    type_params: &'static str,
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
                type_params: Box::leak(t.type_params.into_boxed_str()),
                methods: t
                    .methods
                    .into_iter()
                    .map(|m| BuiltinMethod {
                        name: Box::leak(m.name.into_boxed_str()),
                        signature: Box::leak(m.signature.into_boxed_str()),
                        snippet: Box::leak(m.snippet.into_boxed_str()),
                        documentation: Box::leak(m.doc.into_boxed_str()),
                        min_language_version: Box::leak(m.min_language_version.into_boxed_str()),
                        max_language_version: Box::leak(m.max_language_version.into_boxed_str()),
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

/// Return the built-in methods available for the detected language version.
pub fn methods_for_type_for(
    type_name: &str,
    language: Option<LanguageVersion>,
) -> Vec<&'static BuiltinMethod> {
    methods_for_type(type_name)
        .iter()
        .filter(|m| is_available_range(m.min_language_version, m.max_language_version, language))
        .collect()
}

/// The generic parameter names declared for a built-in type (`Map` → `["K", "V"]`).
fn type_param_names(type_name: &str) -> Vec<&'static str> {
    let raw = registry()
        .types
        .iter()
        .find(|t| t.name == type_name)
        .map(|t| t.type_params)
        .unwrap_or("");
    parse_type_params(raw)
}

/// Parse the parameter names out of a `<...>` declaration (`<K, V>` → `["K", "V"]`).
pub(crate) fn parse_type_params(type_params: &str) -> Vec<&str> {
    type_params
        .trim()
        .trim_start_matches('<')
        .trim_end_matches('>')
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect()
}

/// Substitute generic parameter identifiers declared in `type_params` (`<K, V>`)
/// with `type_args` throughout `text`. Shared with the stdlib registry.
pub fn substitute_generic_params(text: &str, type_params: &str, type_args: &[String]) -> String {
    let params = parse_type_params(type_params);
    substitute_generics(text, &params, type_args)
}

/// The declared return type of a method signature (`lookup(key: K): V` → `V`).
fn return_type_of(signature: &str) -> Option<String> {
    let (_, ret) = signature.rsplit_once("): ")?;
    let ret = ret.trim();
    if ret.is_empty() {
        None
    } else {
        Some(ret.to_string())
    }
}

/// Substitute generic parameter identifiers with concrete type arguments
/// (`Maybe<T>` + `T` → `Token` becomes `Maybe<Token>`).
fn substitute_generics(text: &str, params: &[&str], args: &[String]) -> String {
    let replace = |word: &str| -> String {
        match params.iter().position(|p| *p == word) {
            Some(i) => args.get(i).cloned().unwrap_or_else(|| word.to_string()),
            None => word.to_string(),
        }
    };
    let mut out = String::with_capacity(text.len());
    let mut ident = String::new();
    for c in text.chars() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(c);
        } else {
            if !ident.is_empty() {
                out.push_str(&replace(&ident));
                ident.clear();
            }
            out.push(c);
        }
    }
    if !ident.is_empty() {
        out.push_str(&replace(&ident));
    }
    out
}

/// The declared return type of a built-in method on a (possibly generic) type,
/// with the receiver's generic arguments substituted in.
///
/// `method_return_type("Map", &["Address".into(), "Token".into()], "lookup")`
/// returns `Some("Token")`.
pub fn method_return_type(
    type_name: &str,
    type_args: &[String],
    method_name: &str,
) -> Option<String> {
    let method = find_method_by_name(type_name, method_name)?;
    let ret = return_type_of(method.signature)?;
    let params = type_param_names(type_name);
    Some(substitute_generics(&ret, &params, type_args))
}

/// Find a specific built-in method by type and method name.
pub fn find_method_by_name(type_name: &str, method_name: &str) -> Option<&'static BuiltinMethod> {
    methods_for_type(type_name)
        .iter()
        .find(|m| m.name == method_name)
}

/// Find a built-in method by type and name, respecting the detected language version.
pub fn find_method_by_name_for(
    type_name: &str,
    method_name: &str,
    language: Option<LanguageVersion>,
) -> Option<&'static BuiltinMethod> {
    let method = find_method_by_name(type_name, method_name)?;
    is_available_range(
        method.min_language_version,
        method.max_language_version,
        language,
    )
    .then_some(method)
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
mod tests;
