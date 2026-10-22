// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Parsed semantic versions used for **version-aware feature gating**.
//!
//! The Compact toolchain reports a *compiler* version (e.g. `0.31.1`) and a
//! *language* version (e.g. `0.23.0`). Registry entries gate on the language
//! version, because that is what determines which symbols the compiler accepts.

use std::fmt;

/// A parsed `MAJOR.MINOR.PATCH` version.
///
/// Parsing is lenient: it reads the leading numeric triple and ignores any
/// trailing suffix (e.g. `0.35.0 (debb05f94 2026-09-29)` or `0.35.0-rc.0`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LanguageVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl LanguageVersion {
    /// Construct a version from its components.
    pub const fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    /// Parse a version from the output of `compactc --language-version`
    /// (or `--version`). Returns `None` if no leading numeric triple is found.
    ///
    /// A missing patch component is treated as `0` (e.g. `0.23` → `0.23.0`).
    pub fn parse(s: &str) -> Option<Self> {
        // Take the first whitespace-delimited token: "0.35.0 (hash date)" -> "0.35.0".
        let token = s.split_whitespace().next()?;
        let mut parts = token.split('.');

        let major = leading_u32(parts.next()?)?;
        let minor = leading_u32(parts.next()?)?;
        let patch = match parts.next() {
            Some(p) => leading_u32(p)?,
            None => 0,
        };

        Some(Self {
            major,
            minor,
            patch,
        })
    }

    /// Whether this version is at least `major.minor.patch`.
    pub fn at_least(&self, major: u32, minor: u32, patch: u32) -> bool {
        *self >= Self::new(major, minor, patch)
    }

    /// The version as a `(major, minor, patch)` tuple.
    pub fn as_tuple(&self) -> (u32, u32, u32) {
        (self.major, self.minor, self.patch)
    }
}

impl fmt::Display for LanguageVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Parse the leading run of ASCII digits, ignoring any trailing suffix.
fn leading_u32(s: &str) -> Option<u32> {
    let digits: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
    if digits.is_empty() {
        None
    } else {
        digits.parse().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_full_version() {
        assert_eq!(
            LanguageVersion::parse("0.23.0"),
            Some(LanguageVersion::new(0, 23, 0))
        );
        assert_eq!(
            LanguageVersion::parse("0.31.1"),
            Some(LanguageVersion::new(0, 31, 1))
        );
    }

    #[test]
    fn test_parse_missing_patch() {
        assert_eq!(
            LanguageVersion::parse("0.23"),
            Some(LanguageVersion::new(0, 23, 0))
        );
    }

    #[test]
    fn test_parse_ignores_suffix() {
        // `compactc --version` on 0.35.0 prints commit + date after the version.
        assert_eq!(
            LanguageVersion::parse("0.35.0 (debb05f94 2026-09-29)"),
            Some(LanguageVersion::new(0, 35, 0))
        );
        assert_eq!(
            LanguageVersion::parse("0.35.0-rc.0"),
            Some(LanguageVersion::new(0, 35, 0))
        );
    }

    #[test]
    fn test_parse_invalid() {
        assert_eq!(LanguageVersion::parse(""), None);
        assert_eq!(LanguageVersion::parse("no version here"), None);
        assert_eq!(LanguageVersion::parse("x.y.z"), None);
    }

    #[test]
    fn test_ordering_and_at_least() {
        let v0 = LanguageVersion::parse("0.23.0").unwrap();
        let v103 = LanguageVersion::parse("0.23.103").unwrap();

        assert!(v103 > v0);
        assert!(v0 < v103);

        assert!(v0.at_least(0, 23, 0));
        assert!(!v0.at_least(0, 23, 1));
        assert!(v103.at_least(0, 23, 103));
        assert!(v103.at_least(0, 23, 0));
        assert!(!v103.at_least(0, 24, 0));
    }

    #[test]
    fn test_display() {
        assert_eq!(LanguageVersion::new(0, 23, 103).to_string(), "0.23.103");
    }
}
