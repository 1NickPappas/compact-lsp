// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Compact Analyzer - Analysis engines for the Compact LSP
//!
//! This crate provides:
//! - Diagnostics engine: wraps `compactc` compiler for error reporting
//! - Formatter engine: wraps `format-compact` for code formatting

pub mod diagnostics;
pub mod formatter;

pub use diagnostics::DiagnosticEngine;
pub use formatter::FormatterEngine;
