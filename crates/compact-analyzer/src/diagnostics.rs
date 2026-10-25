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

use std::path::{Path, PathBuf};

use crate::version::LanguageVersion;

/// The Compact devtools artifact directory: `$COMPACT_DIRECTORY` or `~/.compact`.
pub(crate) fn compact_directory() -> Option<PathBuf> {
    if let Ok(dir) = std::env::var("COMPACT_DIRECTORY")
        && !dir.is_empty()
    {
        return Some(PathBuf::from(dir));
    }
    std::env::var("HOME")
        .ok()
        .map(|home| Path::new(&home).join(".compact"))
}

/// Resolve a Compact devtools `bin/<name>` entry to a runnable binary.
///
/// `<dir>/bin/<name>` is typically a symlink into a versioned toolchain
/// directory. For `compactc`, that target is a *wrapper script* that execs
/// `compactc.bin` relative to its own real directory; invoking the wrapper
/// through the `bin/` symlink makes `$0` point at `bin/`, so it looks for a
/// nonexistent `bin/compactc.bin`. Resolve the symlink and prefer `<real_name>`
/// next to the target. When `<real_name>` is absent, use the resolved target
/// itself (e.g. `format-compact` is symlinked straight to the real binary).
pub(crate) fn resolve_devtools_binary(dir: &Path, name: &str, real_name: &str) -> Option<String> {
    let link = dir.join("bin").join(name);
    if !link.exists() {
        return None;
    }

    let resolved = std::fs::canonicalize(&link).ok()?;
    let real = resolved.with_file_name(real_name);
    let chosen = if real.exists() { real } else { resolved };
    Some(chosen.to_string_lossy().into_owned())
}

/// The active compiler from the Compact devtools toolchain.
fn devtools_compiler() -> Option<String> {
    resolve_devtools_binary(&compact_directory()?, "compactc", "compactc.bin")
}

/// The diagnostic engine that wraps the compactc compiler.
///
/// This is the main interface between the LSP and the compiler.
pub struct DiagnosticEngine {
    /// Path to the compactc.bin compiler
    compiler_path: Option<String>,
    /// Extra flags required to enable newer language features on this toolchain.
    feature_flags: Vec<String>,
}

impl DiagnosticEngine {
    /// Create a new diagnostic engine, auto-detecting the compiler location.
    pub fn new() -> Self {
        let compiler_path = Self::find_compiler();
        let feature_flags = Self::detect_feature_flags(&compiler_path);
        Self {
            compiler_path,
            feature_flags,
        }
    }

    /// Extra compiler flags to pass for the detected toolchain (e.g.
    /// `--feature-zkir-v3` on toolchains that make the ZKIR-v3 backend opt-in).
    pub fn feature_flags(&self) -> &[String] {
        &self.feature_flags
    }

    /// Detect feature flags the compiler advertises.
    ///
    /// The ZKIR-v3 backend was opt-in in toolchains 0.34.100–0.35.104 and is
    /// required for the foreign secp256r1/Curve25519 types. It became the
    /// default in 0.35.105 and the flag was removed (passing it is an error), so
    /// only pass it when `--help` still advertises it.
    fn detect_feature_flags(compiler_path: &Option<String>) -> Vec<String> {
        let Some(path) = compiler_path else {
            return Vec::new();
        };
        let Ok(output) = std::process::Command::new(path).arg("--help").output() else {
            return Vec::new();
        };
        let help = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        if help.contains("--feature-zkir-v3") {
            tracing::info!("Compiler advertises --feature-zkir-v3; enabling it");
            vec!["--feature-zkir-v3".to_string()]
        } else {
            Vec::new()
        }
    }

    /// Try to find the compactc compiler in common locations.
    ///
    /// Search order:
    /// 1. `COMPACT_COMPILER` environment variable
    /// 2. Compact devtools active toolchain: `$COMPACT_DIRECTORY/bin/compactc`
    ///    (default `~/.compact/bin/compactc`) — `compact update <ver>` repoints this
    ///    symlink, so this tracks whichever toolchain the user activated.
    /// 3. `~/compactc/compactc.bin`
    /// 4. `compactc.bin` on `PATH`
    fn find_compiler() -> Option<String> {
        // 1. Check environment variable
        if let Ok(path) = std::env::var("COMPACT_COMPILER")
            && !path.is_empty()
            && std::path::Path::new(&path).exists()
        {
            tracing::info!("Found compiler via COMPACT_COMPILER env: {}", path);
            return Some(path);
        }

        // 2. Compact devtools active toolchain.
        if let Some(path) = devtools_compiler() {
            tracing::info!("Found compiler via Compact devtools: {}", path);
            return Some(path);
        }

        // 3. Check ~/compactc/compactc.bin
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

        // 4. Check PATH
        for name in ["compactc.bin", "compactc"] {
            if let Ok(output) = std::process::Command::new("which").arg(name).output()
                && output.status.success()
            {
                let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !path.is_empty() {
                    tracing::info!("Found compiler in PATH: {}", path);
                    return Some(path);
                }
            }
        }

        tracing::warn!("Could not find compactc compiler");
        None
    }

    /// Check if the compiler was found.
    pub fn is_available(&self) -> bool {
        self.compiler_path.is_some()
    }

    /// Run a single compiler flag and return its trimmed stdout, when it succeeds.
    fn run_flag(&self, flag: &str) -> Option<String> {
        let path = self.compiler_path.as_ref()?;
        std::process::Command::new(path)
            .arg(flag)
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .filter(|s| !s.is_empty())
    }

    /// Detect the compiler and language versions by invoking the compiler.
    /// Returns `(compiler_version, language_version)` when available.
    pub fn version_info(&self) -> Option<(String, String)> {
        let compiler = self.run_flag("--version")?;
        let language = self.run_flag("--language-version").unwrap_or_default();
        Some((compiler, language))
    }

    /// The parsed language version the detected compiler targets (`0.23.0` for
    /// toolchain 0.31.1). `None` when no compiler is available or it cannot be parsed.
    pub fn language_version(&self) -> Option<LanguageVersion> {
        LanguageVersion::parse(&self.run_flag("--language-version")?)
    }

    /// The Midnight ledger version the detected compiler targets
    /// (e.g. `ledger-8.0.0` for toolchain 0.31.1).
    pub fn ledger_version(&self) -> Option<String> {
        self.run_flag("--ledger-version")
    }

    /// The Compact runtime version the detected compiler targets.
    pub fn runtime_version(&self) -> Option<String> {
        self.run_flag("--runtime-version")
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
            .args(&self.feature_flags)
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

        // Write the content to a uniquely named temp file in the same
        // directory as the original file, so imports resolve correctly.
        // `tempfile` guarantees a fresh name per call, which keeps concurrent
        // diagnostics for two documents in one directory from colliding.
        let temp_file = match tempfile::Builder::new()
            .prefix(".compact-lsp-temp-")
            .suffix(".compact")
            .tempfile_in(original_dir)
        {
            Ok(file) => file,
            Err(e) => {
                tracing::error!("Failed to create temp file: {}", e);
                return Vec::new();
            }
        };
        let temp_file_path = temp_file.path().to_path_buf();

        if let Err(e) = std::fs::write(&temp_file_path, content) {
            tracing::error!("Failed to write temp file: {}", e);
            return Vec::new();
        }

        // Create temp directory for compiler output
        let temp_output_dir = match tempfile::tempdir() {
            Ok(dir) => dir,
            Err(e) => {
                tracing::error!("Failed to create temp directory: {}", e);
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
            .args(&self.feature_flags)
            .arg(&temp_file_path)
            .arg(temp_output_dir.path())
            .output()
            .await
        {
            Ok(output) => output,
            Err(e) => {
                tracing::error!("Failed to run compiler: {}", e);
                return Vec::new();
            }
        };
        // `temp_file` is dropped here, removing the temp input.
        drop(temp_file);

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
    let re =
        regex::Regex::new(r"^Exception:\s*(\S+)\s+line\s+(\d+)\s+char\s+(\d+):\s*(.+)$").ok()?;

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
mod tests;
