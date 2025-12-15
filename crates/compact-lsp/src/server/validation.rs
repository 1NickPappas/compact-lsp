//! Validation utilities for Compact identifiers, keywords, and types.

/// Check if a name is a Compact keyword.
pub fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "circuit"
            | "witness"
            | "struct"
            | "enum"
            | "module"
            | "import"
            | "export"
            | "ledger"
            | "return"
            | "if"
            | "else"
            | "for"
            | "while"
            | "let"
            | "const"
            | "true"
            | "false"
            | "public"
            | "private"
            | "pure"
            | "sealed"
            | "pragma"
            | "include"
            | "constructor"
            | "contract"
            | "assert"
            | "default"
            | "map"
            | "fold"
            | "disclose"
            | "pad"
            | "as"
            | "of"
            | "prefix"
    )
}

/// Check if a name is a built-in type.
pub fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "Field"
            | "Boolean"
            | "Uint"
            | "Bytes"
            | "Vector"
            | "Opaque"
            | "Counter"
            | "Void"
            | "Map"
            | "Set"
            | "Cell"
            | "Address"
    )
}

/// Check if a name is a valid Compact identifier.
pub fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let mut chars = name.chars();
    // First char must be letter or underscore
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    // Rest must be alphanumeric or underscore
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_keyword() {
        assert!(is_keyword("circuit"));
        assert!(is_keyword("struct"));
        assert!(is_keyword("enum"));
        assert!(is_keyword("ledger"));
        assert!(is_keyword("witness"));
        assert!(is_keyword("return"));
        assert!(is_keyword("if"));
        assert!(is_keyword("else"));
        assert!(is_keyword("for"));
        assert!(is_keyword("while"));
        assert!(is_keyword("const"));
        assert!(is_keyword("pure"));
    }

    #[test]
    fn test_is_keyword_negative() {
        assert!(!is_keyword("foo"));
        assert!(!is_keyword("myCircuit"));
        assert!(!is_keyword("Field"));
        assert!(!is_keyword("Boolean"));
        assert!(!is_keyword(""));
    }

    #[test]
    fn test_is_builtin_type() {
        assert!(is_builtin_type("Field"));
        assert!(is_builtin_type("Boolean"));
        assert!(is_builtin_type("Uint"));
        assert!(is_builtin_type("Bytes"));
        assert!(is_builtin_type("Vector"));
        assert!(is_builtin_type("Opaque"));
        assert!(is_builtin_type("Counter"));
        assert!(is_builtin_type("Void"));
        assert!(is_builtin_type("Map"));
        assert!(is_builtin_type("Set"));
        assert!(is_builtin_type("Cell"));
        assert!(is_builtin_type("Address"));
    }

    #[test]
    fn test_is_builtin_type_negative() {
        assert!(!is_builtin_type("field"));
        assert!(!is_builtin_type("boolean"));
        assert!(!is_builtin_type("MyStruct"));
        assert!(!is_builtin_type("circuit"));
        assert!(!is_builtin_type(""));
    }

    #[test]
    fn test_is_valid_identifier() {
        assert!(is_valid_identifier("foo"));
        assert!(is_valid_identifier("_bar"));
        assert!(is_valid_identifier("a1"));
        assert!(is_valid_identifier("myFunction"));
        assert!(is_valid_identifier("_"));
        assert!(is_valid_identifier("_123"));
        assert!(is_valid_identifier("camelCase"));
        assert!(is_valid_identifier("snake_case"));
    }

    #[test]
    fn test_is_valid_identifier_invalid() {
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("123"));
        assert!(!is_valid_identifier("123abc"));
        assert!(!is_valid_identifier("-foo"));
        assert!(!is_valid_identifier("foo-bar"));
        assert!(!is_valid_identifier("foo bar"));
    }
}
