// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Formatter engine that wraps the format-compact binary.
//!
//! # How it works
//!
//! 1. We receive file content from the LSP server
//! 2. We write it to a temp file
//! 3. We invoke `format-compact <temp_file>`
//! 4. We read the formatted output from stdout
//! 5. We return the formatted content

use std::path::Path;

/// The formatter engine that wraps the format-compact binary.
pub struct FormatterEngine {
    /// Path to the format-compact binary
    formatter_path: Option<String>,
}

impl FormatterEngine {
    /// Create a new formatter engine, auto-detecting the formatter location.
    pub fn new() -> Self {
        let formatter_path = Self::find_formatter();
        Self { formatter_path }
    }

    /// Try to find the format-compact binary in common locations.
    ///
    /// Search order:
    /// 1. COMPACT_FORMATTER environment variable
    /// 2. ~/compactc/format-compact
    /// 3. format-compact in PATH
    fn find_formatter() -> Option<String> {
        // 1. Check environment variable
        if let Ok(path) = std::env::var("COMPACT_FORMATTER") {
            if Path::new(&path).exists() {
                tracing::info!("Found formatter via COMPACT_FORMATTER env: {}", path);
                return Some(path);
            }
        }

        // 2. Check ~/compactc/format-compact
        if let Ok(home) = std::env::var("HOME") {
            let path = Path::new(&home)
                .join("compactc")
                .join("format-compact");
            if path.exists() {
                let path_str = path.to_string_lossy().to_string();
                tracing::info!("Found formatter at: {}", path_str);
                return Some(path_str);
            }
        }

        // 3. Check PATH
        if let Ok(output) = std::process::Command::new("which")
            .arg("format-compact")
            .output()
        {
            if output.status.success() {
                let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
                tracing::info!("Found formatter in PATH: {}", path);
                return Some(path);
            }
        }

        tracing::warn!("Could not find format-compact binary");
        None
    }

    /// Check if the formatter was found.
    pub fn is_available(&self) -> bool {
        self.formatter_path.is_some()
    }

    /// Format the given content and return the formatted result.
    ///
    /// # How it works
    ///
    /// 1. Write content to a temp file
    /// 2. Run: `format-compact <temp_file>`
    /// 3. Read formatted output from stdout
    /// 4. Return formatted content or error
    pub async fn format(&self, content: &str) -> Result<String, String> {
        // Check if formatter is available
        let formatter_path = match &self.formatter_path {
            Some(path) => path,
            None => {
                return Err("Formatter not available".to_string());
            }
        };

        // Create temp file with content
        let temp_dir = tempfile::tempdir()
            .map_err(|e| format!("Failed to create temp directory: {}", e))?;

        let temp_file = temp_dir.path().join("format_input.compact");
        tokio::fs::write(&temp_file, content)
            .await
            .map_err(|e| format!("Failed to write temp file: {}", e))?;

        tracing::debug!(
            "Running formatter: {} {}",
            formatter_path,
            temp_file.display()
        );

        // Run the formatter
        let output = tokio::process::Command::new(formatter_path)
            .arg(&temp_file)
            .output()
            .await
            .map_err(|e| format!("Failed to run formatter: {}", e))?;

        // Check for errors
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            tracing::warn!("Formatter failed: {}", stderr);
            return Err(format!("Formatter failed: {}", stderr));
        }

        // Return formatted output
        let formatted = String::from_utf8_lossy(&output.stdout).to_string();
        tracing::debug!("Formatter succeeded, output length: {}", formatted.len());

        Ok(formatted)
    }
}

impl Default for FormatterEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_formatter_detection() {
        let engine = FormatterEngine::new();
        // Just check that it doesn't panic
        let _ = engine.is_available();
    }
}
