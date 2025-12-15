// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Compact Analyzer - Diagnostics engine for the Compact LSP
//!
//! This crate wraps the `compactc` compiler to provide diagnostics.
//! It parses compiler error output and converts it to LSP Diagnostic objects.

pub mod diagnostics;

pub use diagnostics::DiagnosticEngine;
