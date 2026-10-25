//! Format helpers for the server.

use super::*;

/// A line-range replacement: original lines `[start, end)` become `replacement`.
#[derive(Debug)]
pub(super) struct LineEdit {
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) replacement: Vec<String>,
}
/// Compute line-level, minimal edits that turn `original` into `formatted`.
///
/// Uses an LCS alignment over lines (with common prefix/suffix trimming), so
/// every `LineEdit` replaces one contiguous run of original lines and inserts
/// the corresponding formatted lines. The edits are independent and in
/// increasing order, so any subset of them yields a valid document.
pub(super) fn line_edits(original: &[&str], formatted: &[&str]) -> Vec<LineEdit> {
    // Trim the common prefix and suffix so the LCS operates on just the change.
    let mut prefix = 0;
    while prefix < original.len()
        && prefix < formatted.len()
        && original[prefix] == formatted[prefix]
    {
        prefix += 1;
    }
    let mut suffix = 0;
    while suffix < original.len() - prefix
        && suffix < formatted.len() - prefix
        && original[original.len() - 1 - suffix] == formatted[formatted.len() - 1 - suffix]
    {
        suffix += 1;
    }
    let a = &original[prefix..original.len() - suffix];
    let b = &formatted[prefix..formatted.len() - suffix];
    if a.is_empty() && b.is_empty() {
        return Vec::new();
    }

    // Guard against pathological inputs: collapse to one block replacement.
    if a.len().saturating_mul(b.len()) > 4_000_000 {
        return vec![LineEdit {
            start: prefix,
            end: prefix + a.len(),
            replacement: b.iter().map(|s| s.to_string()).collect(),
        }];
    }

    let n = a.len();
    let m = b.len();
    // dp[i][j] = LCS length of a[i..] and b[j..].
    let mut dp = vec![vec![0u32; m + 1]; n + 1];
    for i in (0..n).rev() {
        for j in (0..m).rev() {
            dp[i][j] = if a[i] == b[j] {
                dp[i + 1][j + 1] + 1
            } else {
                dp[i + 1][j].max(dp[i][j + 1])
            };
        }
    }

    // Back-track, grouping each run of non-equal lines into one edit.
    let mut edits: Vec<LineEdit> = Vec::new();
    let mut orig_pos = 0usize;
    let (mut i, mut j) = (0usize, 0usize);
    let mut run_start: Option<usize> = None;
    let mut repl: Vec<String> = Vec::new();

    while i < n && j < m {
        if a[i] == b[j] {
            if let Some(s) = run_start.take() {
                edits.push(LineEdit {
                    start: s,
                    end: prefix + orig_pos,
                    replacement: std::mem::take(&mut repl),
                });
            }
            i += 1;
            j += 1;
            orig_pos += 1;
        } else {
            run_start.get_or_insert(prefix + orig_pos);
            if dp[i + 1][j] >= dp[i][j + 1] {
                i += 1;
                orig_pos += 1;
            } else {
                repl.push(b[j].to_string());
                j += 1;
            }
        }
    }
    if run_start.is_none() && (i < n || j < m) {
        run_start = Some(prefix + orig_pos);
    }
    while i < n {
        i += 1;
        orig_pos += 1;
    }
    while j < m {
        repl.push(b[j].to_string());
        j += 1;
    }
    if let Some(s) = run_start.take() {
        edits.push(LineEdit {
            start: s,
            end: prefix + orig_pos,
            replacement: repl,
        });
    }
    edits
}
/// Whole-file formatting edits filtered to those intersecting the selected
/// inclusive line range `[sel_start, sel_end]`. Newlines are re-added because
/// each [`TextEdit`] spans whole lines (start/end at column 0).
///
/// A diff group that straddles the selection boundary is kept in full (the
/// line mapping inside a group is ambiguous), so a neighbouring unselected
/// line sharing that group can also change.
pub(super) fn range_format_edits(
    original: &str,
    formatted: &str,
    sel_start: u32,
    sel_end: u32,
) -> Vec<TextEdit> {
    let orig: Vec<&str> = original.split('\n').collect();
    let fmt: Vec<&str> = formatted.split('\n').collect();

    let mut edits = Vec::new();
    for edit in line_edits(&orig, &fmt) {
        let overlaps = if edit.start == edit.end {
            // Pure insertion anchored between lines: include it when the
            // anchor line is within the selection.
            edit.start as u32 >= sel_start && edit.start as u32 <= sel_end
        } else {
            edit.start as u32 <= sel_end && (edit.end as u32 - 1) >= sel_start
        };
        if !overlaps {
            continue;
        }
        let new_text = if edit.replacement.is_empty() {
            String::new()
        } else {
            format!("{}\n", edit.replacement.join("\n"))
        };
        edits.push(TextEdit {
            range: Range {
                start: Position {
                    line: edit.start as u32,
                    character: 0,
                },
                end: Position {
                    line: edit.end as u32,
                    character: 0,
                },
            },
            new_text,
        });
    }
    edits
}
