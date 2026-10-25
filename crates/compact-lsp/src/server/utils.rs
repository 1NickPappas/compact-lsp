//! Text analysis utilities.

use lsp_types::{Position, Range};

/// Get the word (identifier) at a given position in the source.
pub fn get_word_at_position(content: &str, line: u32, character: u32) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = lines.get(line as usize)?;
    let char_idx = character as usize;

    // Check if position is within bounds
    if char_idx > line_content.len() {
        return None;
    }

    // Find word boundaries (identifiers can contain a-z, A-Z, 0-9, _)
    let chars: Vec<char> = line_content.chars().collect();

    // Find start of word
    let mut start = char_idx;
    while start > 0 {
        let c = chars.get(start - 1)?;
        if c.is_alphanumeric() || *c == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    // Find end of word
    let mut end = char_idx;
    while end < chars.len() {
        let c = chars.get(end)?;
        if c.is_alphanumeric() || *c == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    Some(chars[start..end].iter().collect())
}

/// Get the range of the word at a given position.
pub fn get_word_range_at_position(content: &str, line: u32, character: u32) -> Option<Range> {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = lines.get(line as usize)?;

    let char_idx = character as usize;
    if char_idx > line_content.len() {
        return None;
    }

    let chars: Vec<char> = line_content.chars().collect();

    // Find word boundaries
    let mut start = char_idx;
    while start > 0 {
        let c = chars.get(start - 1)?;
        if c.is_alphanumeric() || *c == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    let mut end = char_idx;
    while end < chars.len() {
        let c = chars.get(end)?;
        if c.is_alphanumeric() || *c == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    Some(Range {
        start: Position {
            line,
            character: start as u32,
        },
        end: Position {
            line,
            character: end as u32,
        },
    })
}

/// Get the function name from a call context (e.g., "func_name(" before cursor).
pub fn get_function_call_name(content: &str, line: u32, character: u32) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = lines.get(line as usize)?;

    // Search backwards from cursor to find the opening paren
    let chars: Vec<char> = line_content.chars().collect();
    let mut idx = character as usize;

    // First, find the opening paren of our call (handling nested parens)
    let mut paren_depth = 0;
    while idx > 0 {
        idx -= 1;
        match chars.get(idx) {
            Some(')') => paren_depth += 1,
            Some('(') => {
                if paren_depth == 0 {
                    // Found our opening paren - now get the identifier before it
                    break;
                }
                paren_depth -= 1;
            }
            _ => {}
        }
    }

    // Now idx points to '(' - search backwards for the function name
    if idx == 0 {
        return None;
    }

    // Skip any whitespace
    while idx > 0
        && chars
            .get(idx - 1)
            .map(|c| c.is_whitespace())
            .unwrap_or(false)
    {
        idx -= 1;
    }

    // Now find the identifier
    let end = idx;
    while idx > 0
        && chars
            .get(idx - 1)
            .map(|c| c.is_alphanumeric() || *c == '_')
            .unwrap_or(false)
    {
        idx -= 1;
    }

    if idx == end {
        return None;
    }

    Some(chars[idx..end].iter().collect())
}

/// Count commas before cursor position in a function call.
pub fn count_commas_before_cursor(content: &str, line: u32, character: u32) -> u32 {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = match lines.get(line as usize) {
        Some(l) => *l,
        None => return 0,
    };

    let chars: Vec<char> = line_content.chars().collect();
    let cursor_idx = character as usize;
    let mut comma_count = 0;
    let mut paren_depth = 0;

    // Search backwards from cursor to find commas (not inside nested parens)
    let mut idx = cursor_idx;
    while idx > 0 {
        idx -= 1;
        match chars.get(idx) {
            Some(')') => paren_depth += 1,
            Some('(') => {
                if paren_depth == 0 {
                    // Found our function call's opening paren
                    break;
                }
                paren_depth -= 1;
            }
            Some(',') if paren_depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    comma_count
}

/// Parse parameter strings from a detail string like "(a: Field, b: Field): ReturnType".
pub fn parse_params_from_detail(detail: &str) -> Vec<String> {
    // Extract content between first '(' and matching ')'
    let start = match detail.find('(') {
        Some(i) => i + 1,
        None => return vec![],
    };

    let end = match detail.find(')') {
        Some(i) => i,
        None => return vec![],
    };

    if start >= end {
        return vec![];
    }

    let params_str = &detail[start..end];
    if params_str.trim().is_empty() {
        return vec![];
    }

    // Split by comma, handling potential nested types
    params_str
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

/// Detect dot-completion context by scanning backwards from cursor.
///
/// Unlike `get_dot_access_variable` (which requires cursor immediately after the dot),
/// this handles partial identifiers after the dot (e.g., `round.inc|` returns `Some("round")`).
/// Works for both `round.|` and `round.inc|` positions.
/// Whether the cursor is in a `contract implements <Type>` declaration.
///
/// Used to offer contract types as completions. This is intentionally
/// text-based: while the type name is being typed the tree-sitter parse may be
/// incomplete, so the AST is not reliable here.
pub fn is_after_contract_implements(content: &str, line: u32, character: u32) -> bool {
    let Some(line_content) = content.lines().nth(line as usize) else {
        return false;
    };
    let before: String = line_content.chars().take(character as usize).collect();
    if before.contains(';') {
        return false;
    }
    before.trim_start().starts_with("contract implements")
}

pub fn detect_dot_context(content: &str, line: u32, character: u32) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = lines.get(line as usize)?;
    let char_idx = character as usize;

    if char_idx > line_content.len() {
        return None;
    }

    let chars: Vec<char> = line_content.chars().collect();

    // Skip backwards over any partial identifier (e.g., the "inc" in "round.inc|")
    let mut idx = char_idx;
    while idx > 0 {
        let c = chars.get(idx - 1)?;
        if c.is_alphanumeric() || *c == '_' {
            idx -= 1;
        } else {
            break;
        }
    }

    // Now check if there's a dot
    if idx == 0 {
        return None;
    }
    if chars.get(idx - 1).copied() != Some('.') {
        return None;
    }
    let dot_idx = idx - 1;

    if dot_idx == 0 {
        return None;
    }

    // Scan backwards from before the dot over an expression base
    // (`w.token` → "w.token", `make()` → "make()", `items[0]` → "items[0]",
    // `id<T>()` → "id<T>()").
    let end = dot_idx;
    let mut start = end;
    while start > 0 {
        let c = *chars.get(start - 1)?;
        if c.is_alphanumeric() || matches!(c, '_' | '.' | '(' | ')' | '[' | ']' | '<' | '>') {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    let path: String = chars[start..end].iter().collect();
    let path = path.trim_start_matches('.').to_string();
    if path.is_empty() { None } else { Some(path) }
}

/// Get the variable name before a dot at the cursor position.
///
/// If the cursor is right after `round.`, this returns `Some("round")`.
pub fn get_dot_access_variable(content: &str, line: u32, character: u32) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    let line_content = lines.get(line as usize)?;
    let char_idx = character as usize;

    if char_idx == 0 {
        return None;
    }

    let chars: Vec<char> = line_content.chars().collect();

    // The character before the cursor should be '.'
    let dot_idx = char_idx - 1;
    if chars.get(dot_idx).copied() != Some('.') {
        return None;
    }

    if dot_idx == 0 {
        return None;
    }

    // Scan backwards from before the dot over an expression base
    // (`w.token.` → "w.token", `make().` → "make()", `items[0].` → "items[0]",
    // `id<T>().` → "id<T>()").
    let end = dot_idx;
    let mut start = end;
    while start > 0 {
        let c = *chars.get(start - 1)?;
        if c.is_alphanumeric() || matches!(c, '_' | '.' | '(' | ')' | '[' | ']' | '<' | '>') {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    let path: String = chars[start..end].iter().collect();
    let path = path.trim_start_matches('.').to_string();
    if path.is_empty() { None } else { Some(path) }
}

#[cfg(test)]
mod tests;
