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
    /// # How it works
    ///
    /// 1. Extract file path from URI (file:///path/to/file.compact)
    /// 2. Create temp directory for compiler output
    /// 3. Run: `compactc.bin --vscode <file> <temp_dir>`
    /// 4. Parse stderr for error messages
    /// 5. Return LSP Diagnostic objects
    pub async fn diagnose(&self, uri: &str, _content: &str) -> Vec<Diagnostic> {
        // Check if compiler is available
        let compiler_path = match &self.compiler_path {
            Some(path) => path,
            None => {
                tracing::warn!("Compiler not available, skipping diagnostics");
                return Vec::new();
            }
        };

        // Extract file path from URI (e.g., "file:///path/to/file.compact" -> "/path/to/file.compact")
        let file_path = match uri.strip_prefix("file://") {
            Some(path) => path,
            None => {
                tracing::warn!("Invalid URI format: {}", uri);
                return Vec::new();
            }
        };

        // Check if file exists
        if !std::path::Path::new(file_path).exists() {
            tracing::warn!("File does not exist: {}", file_path);
            return Vec::new();
        }

        // Create temp directory for compiler output
        let temp_dir = match tempfile::tempdir() {
            Ok(dir) => dir,
            Err(e) => {
                tracing::error!("Failed to create temp directory: {}", e);
                return Vec::new();
            }
        };

        tracing::debug!(
            "Running compiler: {} --vscode {} {}",
            compiler_path,
            file_path,
            temp_dir.path().display()
        );

        // Run the compiler
        // compactc.bin --vscode <source-file> <output-dir>
        let output = match tokio::process::Command::new(compiler_path)
            .arg("--vscode")
            .arg("--skip-zk") // Skip ZK key generation for faster diagnostics
            .arg(file_path)
            .arg(temp_dir.path())
            .output()
            .await
        {
            Ok(output) => output,
            Err(e) => {
                tracing::error!("Failed to run compiler: {}", e);
                return Vec::new();
            }
        };

        // Parse stderr for errors
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);

        tracing::debug!("Compiler exit code: {:?}", output.status.code());
        if !stderr.is_empty() {
            tracing::debug!("Compiler stderr: {}", stderr);
        }
        if !stdout.is_empty() {
            tracing::trace!("Compiler stdout: {}", stdout);
        }

        // Collect diagnostics from both stderr and stdout
        let mut diagnostics = Vec::new();

        for line in stderr.lines().chain(stdout.lines()) {
            if let Some(diag) = parse_error_line(line) {
                diagnostics.push(diag);
            }
        }

        tracing::info!(
            "Diagnostics for {}: {} error(s)",
            file_path,
            diagnostics.len()
        );

        diagnostics
    }

    /// Run diagnostics on in-memory content (without requiring file save).
    ///
    /// This writes the content to a temporary file and runs the compiler on it.
    /// Useful for live diagnostics while typing.
    ///
    /// # Arguments
    ///
    /// * `uri` - The original file URI (for logging and path resolution)
    /// * `content` - The current in-memory content to diagnose
    pub async fn diagnose_content(&self, uri: &str, content: &str) -> Vec<Diagnostic> {
        // Check if compiler is available
        let compiler_path = match &self.compiler_path {
            Some(path) => path,
            None => {
                tracing::trace!("Compiler not available, skipping live diagnostics");
                return Vec::new();
            }
        };

        // Extract file path from URI to get the directory (for imports to resolve)
        let original_path = match uri.strip_prefix("file://") {
            Some(path) => path,
            None => {
                tracing::warn!("Invalid URI format: {}", uri);
                return Vec::new();
            }
        };

        // Get the directory of the original file for proper import resolution
        let original_dir = match std::path::Path::new(original_path).parent() {
            Some(dir) => dir,
            None => {
                tracing::warn!("Could not get parent directory: {}", original_path);
                return Vec::new();
            }
        };

        // Create temp file in the same directory as the original file
        // This ensures imports resolve correctly
        let temp_file_name = format!(".compact-lsp-temp-{}.compact", std::process::id());
        let temp_file_path = original_dir.join(&temp_file_name);

        // Write content to temp file
        if let Err(e) = std::fs::write(&temp_file_path, content) {
            tracing::error!("Failed to write temp file: {}", e);
            return Vec::new();
        }

        // Create temp directory for compiler output
        let temp_output_dir = match tempfile::tempdir() {
            Ok(dir) => dir,
            Err(e) => {
                tracing::error!("Failed to create temp directory: {}", e);
                let _ = std::fs::remove_file(&temp_file_path);
                return Vec::new();
            }
        };

        tracing::trace!(
            "Running live diagnostics: {} --vscode {} {}",
            compiler_path,
            temp_file_path.display(),
            temp_output_dir.path().display()
        );

        // Run the compiler
        let output = match tokio::process::Command::new(compiler_path)
            .arg("--vscode")
            .arg("--skip-zk")
            .arg(&temp_file_path)
            .arg(temp_output_dir.path())
            .output()
            .await
        {
            Ok(output) => output,
            Err(e) => {
                tracing::error!("Failed to run compiler: {}", e);
                let _ = std::fs::remove_file(&temp_file_path);
                return Vec::new();
            }
        };

        // Clean up temp file
        let _ = std::fs::remove_file(&temp_file_path);

        // Parse stderr and stdout for errors
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);

        let mut diagnostics = Vec::new();

        for line in stderr.lines().chain(stdout.lines()) {
            if let Some(diag) = parse_error_line(line) {
                diagnostics.push(diag);
            }
        }

        tracing::trace!(
            "Live diagnostics for {}: {} error(s)",
            uri,
            diagnostics.len()
        );

        diagnostics
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
