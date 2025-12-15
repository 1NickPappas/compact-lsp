// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic engine that wraps the compactc compiler.
//!
//! # How it works
//!
//! 1. We receive a file path from the LSP server
//! 2. We invoke `compactc.bin --vscode <file> <temp_dir>`
//! 3. We parse stderr for error messages in the format:
//!    `Exception: <filename> line <line> char <col>: <message>`
//! 4. We convert these to LSP Diagnostic objects

// LSP types for diagnostics
// We use ls-types (aliased as lsp-types in Cargo.toml), which is compatible with tower-lsp-server
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// The diagnostic engine that wraps the compactc compiler.
///
/// This is the main interface between the LSP and the compiler.
pub struct DiagnosticEngine {
    /// Path to the compactc.bin compiler
    compiler_path: Option<String>,
}

impl DiagnosticEngine {
    /// Create a new diagnostic engine, auto-detecting the compiler location.
    pub fn new() -> Self {
        let compiler_path = Self::find_compiler();
        Self { compiler_path }
    }

    /// Try to find the compactc.bin compiler in common locations.
    ///
    /// Search order:
    /// 1. COMPACT_COMPILER environment variable
    /// 2. ~/compactc/compactc.bin
    /// 3. compactc.bin in PATH
    fn find_compiler() -> Option<String> {
        // 1. Check environment variable
        if let Ok(path) = std::env::var("COMPACT_COMPILER") {
            if std::path::Path::new(&path).exists() {
                tracing::info!("Found compiler via COMPACT_COMPILER env: {}", path);
                return Some(path);
            }
        }

        // 2. Check ~/compactc/compactc.bin
        if let Ok(home) = std::env::var("HOME") {
            let path = std::path::Path::new(&home)
                .join("compactc")
                .join("compactc.bin");
            if path.exists() {
                let path_str = path.to_string_lossy().to_string();
                tracing::info!("Found compiler at: {}", path_str);
                return Some(path_str);
            }
        }

        // 3. Check PATH
        if let Ok(output) = std::process::Command::new("which")
            .arg("compactc.bin")
            .output()
        {
            if output.status.success() {
                let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
                tracing::info!("Found compiler in PATH: {}", path);
                return Some(path);
            }
        }

        tracing::warn!("Could not find compactc.bin compiler");
        None
    }

    /// Check if the compiler was found.
    pub fn is_available(&self) -> bool {
        self.compiler_path.is_some()
    }

    /// Run diagnostics on a file and return LSP Diagnostic objects.
    ///
    /// This is a placeholder - we'll implement the full logic in Step 5.
    pub async fn diagnose(&self, _uri: &str, _content: &str) -> Vec<Diagnostic> {
        // TODO: Implement in Step 5
        // For now, return empty diagnostics
        Vec::new()
    }
}

impl Default for DiagnosticEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a single error line from compactc output.
///
/// Format: `Exception: <filename> line <line> char <col>: <message>`
///
/// Returns None if the line doesn't match the expected format.
#[allow(dead_code)] // Will be used in Step 5
fn parse_error_line(line: &str) -> Option<Diagnostic> {
    // Regex pattern for compiler errors
    let re = regex::Regex::new(
        r"^Exception:\s*(\S+)\s+line\s+(\d+)\s+char\s+(\d+):\s*(.+)$"
    ).ok()?;

    let caps = re.captures(line)?;

    let _filename = caps.get(1)?.as_str();
    let line_num: u32 = caps.get(2)?.as_str().parse().ok()?;
    let col: u32 = caps.get(3)?.as_str().parse().ok()?;
    let message = caps.get(4)?.as_str().to_string();

    // LSP uses 0-indexed lines and columns, compiler uses 1-indexed
    let line_0 = line_num.saturating_sub(1);
    let col_0 = col.saturating_sub(1);

    Some(Diagnostic {
        range: Range {
            start: Position {
                line: line_0,
                character: col_0,
            },
            end: Position {
                line: line_0,
                character: col_0 + 1, // Highlight at least one character
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("compactc".to_string()),
        message,
        ..Default::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_error_line() {
        let line = "Exception: broken.compact line 1 char 1: parse error: found \"this\" looking for a program element or end of file";
        let diag = parse_error_line(line).expect("Should parse error line");

        assert_eq!(diag.range.start.line, 0); // 1-indexed -> 0-indexed
        assert_eq!(diag.range.start.character, 0);
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diag.message.contains("parse error"));
        assert_eq!(diag.source, Some("compactc".to_string()));
    }

    #[test]
    fn test_parse_type_error() {
        let line = "Exception: broken3.compact line 4 char 3: mismatch between actual return type Bytes<12> and declared return type Uint<64> of circuit test";
        let diag = parse_error_line(line).expect("Should parse type error");

        assert_eq!(diag.range.start.line, 3); // line 4 -> index 3
        assert_eq!(diag.range.start.character, 2); // char 3 -> index 2
        assert!(diag.message.contains("mismatch"));
    }

    #[test]
    fn test_parse_invalid_line() {
        let line = "This is not an error line";
        assert!(parse_error_line(line).is_none());
    }
}
