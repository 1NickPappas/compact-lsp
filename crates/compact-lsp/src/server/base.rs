//! Base helpers for the server.

use super::*;

/// Split a callable text into its name and explicit type arguments:
/// `id<Uint<64>>` → `("id", ["Uint<64>"])`, `make` → `("make", [])`.
pub(super) fn split_callable_type_args(text: &str) -> (&str, Vec<String>) {
    let text = text.trim();
    if let Some(lt) = text.find('<') {
        let rest = &text[lt..];
        let mut depth = 0i32;
        for (i, c) in rest.char_indices() {
            match c {
                '<' => depth += 1,
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        // Only a suffix that is entirely `<…>` is a type list.
                        if lt + i + 1 == text.len() {
                            return (text[..lt].trim(), split_type_args(&rest[1..i]));
                        }
                        break;
                    }
                }
                _ => {}
            }
        }
    }
    (text, Vec::new())
}
/// Split a type-argument list (`Uint<64>, Maybe<Address>`) on top-level commas,
/// tracking `<`/`(`/`[` nesting so a comma inside a type doesn't split.
pub(super) fn split_type_args(text: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    for (i, c) in text.char_indices() {
        match c {
            '<' | '(' | '[' => depth += 1,
            '>' | ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                let piece = text[start..i].trim();
                if !piece.is_empty() {
                    args.push(piece.to_string());
                }
                start = i + 1;
            }
            _ => {}
        }
    }
    let piece = text[start..].trim();
    if !piece.is_empty() {
        args.push(piece.to_string());
    }
    args
}
/// Parse a text-extracted base (`w.token`, `make()`, `items[0]`,
/// `factory.create()`, `items[0].get()`) into a [`MemberBase`] for completion,
/// where the AST is not available. Peels from the outside in (trailing call,
/// then trailing index, then a plain path), so nested bases compose.
pub(super) fn parse_base_text(text: &str) -> MemberBase {
    let text = text.trim();
    if text.is_empty() {
        return MemberBase::Other;
    }
    // Trailing call: `receiver.method(...)` or `callee<T>(...)`.
    if text.ends_with(')')
        && let Some(open) = matching_open(text, '(', ')')
    {
        let callable = text[..open].trim();
        let args = split_top_level_args(&text[open + 1..text.len() - 1]);
        let (callee, type_args) = split_callable_type_args(callable);
        return match callee.rsplit_once('.') {
            Some((receiver, method)) if !receiver.is_empty() && !method.is_empty() => {
                MemberBase::MethodCall(Box::new(parse_base_text(receiver)), method.to_string())
            }
            _ if !callee.is_empty() => MemberBase::Call {
                callee: callee.to_string(),
                args,
                type_args,
            },
            _ => MemberBase::Other,
        };
    }
    // Trailing index: `receiver[expr]`.
    if text.ends_with(']')
        && let Some(open) = matching_open(text, '[', ']')
    {
        return MemberBase::Index(Box::new(parse_base_text(&text[..open])));
    }
    // Trailing field access on a non-path receiver: `call().field`,
    // `arr[i].field`. Only reached when the text contains a call/index.
    if (text.contains('(') || text.contains('['))
        && let Some(dot) = text.rfind('.')
    {
        let (receiver, field) = text.split_at(dot);
        let field = &field[1..];
        if !receiver.is_empty() && !field.is_empty() {
            return MemberBase::Access(Box::new(parse_base_text(receiver)), field.to_string());
        }
    }
    MemberBase::Path(text.split('.').map(str::to_string).collect())
}
/// Split a call's argument list on top-level commas and parse each argument.
pub(super) fn split_top_level_args(text: &str) -> Vec<MemberBase> {
    let mut args = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    for (i, c) in text.char_indices() {
        match c {
            '(' | '[' => depth += 1,
            ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                let piece = text[start..i].trim();
                if !piece.is_empty() {
                    args.push(parse_base_text(piece));
                }
                start = i + 1;
            }
            _ => {}
        }
    }
    let piece = text[start..].trim();
    if !piece.is_empty() {
        args.push(parse_base_text(piece));
    }
    args
}
/// The index of the opening bracket matching the final closing bracket.
pub(super) fn matching_open(text: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 0i32;
    for (i, c) in text.char_indices().rev() {
        if c == close {
            depth += 1;
        } else if c == open {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}
/// The element type of a generic container (`Map<K, V>` → `V`, `Vector<T>` → `T`).
pub(super) fn element_type(type_text: &str) -> Option<String> {
    generic_args(type_text).pop()
}
/// Top-level generic arguments of a type expression
/// (`Map<Address, Token>` → `["Address", "Token"]`).
pub(super) fn generic_args(type_text: &str) -> Vec<String> {
    let Some(open) = type_text.find('<') else {
        return Vec::new();
    };
    let Some(close) = type_text.rfind('>') else {
        return Vec::new();
    };
    if close <= open + 1 {
        return Vec::new();
    }
    let inner = &type_text[open + 1..close];
    let mut depth = 0i32;
    let mut args = Vec::new();
    let mut current = String::new();
    for ch in inner.chars() {
        match ch {
            '<' => {
                depth += 1;
                current.push(ch);
            }
            '>' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                if !current.trim().is_empty() {
                    args.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        args.push(current.trim().to_string());
    }
    args
}
/// The return type declared after `): ` in a signature or circuit detail
/// (`(to: Address): Boolean` → `Boolean`).
pub(super) fn return_type_of_signature(signature: &str) -> Option<String> {
    let (_, ret) = signature.rsplit_once("): ")?;
    let ret = ret.trim();
    if ret.is_empty() {
        None
    } else {
        Some(ret.to_string())
    }
}
