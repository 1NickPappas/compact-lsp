// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Compact Analyzer - Analysis engines for the Compact LSP
//!
//! This crate provides:
//! - Diagnostics engine: wraps `compactc` compiler for error reporting
//! - Formatter engine: wraps `format-compact` for code formatting
//! - Parser engine: wraps tree-sitter for AST-based features

pub mod diagnostics;
pub mod formatter;
pub mod parser;

pub use diagnostics::DiagnosticEngine;
pub use formatter::FormatterEngine;
pub use parser::{HoverInfo, ParserEngine};
