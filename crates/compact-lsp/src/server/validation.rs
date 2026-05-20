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
            | "const"
            | "true"
            | "false"
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
            | "type"
            | "new"
            | "from"
            | "slice"
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
            | "Map"
            | "Set"
            | "Cell"
            | "List"
            | "MerkleTree"
            | "HistoricMerkleTree"
            | "Kernel"
            | "ContractAddress"
            | "UserAddress"
            | "ShieldedCoinInfo"
            | "QualifiedShieldedCoinInfo"
            | "ShieldedSendResult"
            | "MerkleTreeDigest"
            | "Maybe"
            | "Either"
            | "JubjubPoint"
            | "MerkleTreePathEntry"
            | "MerkleTreePath"
            | "ZswapCoinPublicKey"
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
        assert!(is_keyword("const"));
        assert!(is_keyword("pure"));
        // 0.21 keywords
        assert!(is_keyword("type"));
        assert!(is_keyword("new"));
        assert!(is_keyword("from"));
        assert!(is_keyword("slice"));
    }

    #[test]
    fn test_is_keyword_negative() {
        assert!(!is_keyword("foo"));
        assert!(!is_keyword("myCircuit"));
        assert!(!is_keyword("Field"));
        assert!(!is_keyword("Boolean"));
        assert!(!is_keyword(""));
        // Reserved-but-not-active words are not Compact keywords
        assert!(!is_keyword("while"));
        assert!(!is_keyword("let"));
        assert!(!is_keyword("public"));
        assert!(!is_keyword("private"));
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
        assert!(is_builtin_type("Map"));
        assert!(is_builtin_type("Set"));
        assert!(is_builtin_type("Cell"));
        assert!(is_builtin_type("List"));
        assert!(is_builtin_type("MerkleTree"));
        assert!(is_builtin_type("HistoricMerkleTree"));
        assert!(is_builtin_type("Kernel"));
        assert!(is_builtin_type("ContractAddress"));
        assert!(is_builtin_type("UserAddress"));
        assert!(is_builtin_type("ShieldedCoinInfo"));
        assert!(is_builtin_type("QualifiedShieldedCoinInfo"));
        assert!(is_builtin_type("ShieldedSendResult"));
        assert!(is_builtin_type("MerkleTreeDigest"));
        assert!(is_builtin_type("Maybe"));
        assert!(is_builtin_type("Either"));
        assert!(is_builtin_type("JubjubPoint"));
        assert!(is_builtin_type("MerkleTreePathEntry"));
        assert!(is_builtin_type("MerkleTreePath"));
        assert!(is_builtin_type("ZswapCoinPublicKey"));
    }

    #[test]
    fn test_is_builtin_type_negative() {
        assert!(!is_builtin_type("field"));
        assert!(!is_builtin_type("boolean"));
        assert!(!is_builtin_type("MyStruct"));
        assert!(!is_builtin_type("circuit"));
        assert!(!is_builtin_type(""));
        // Pre-0.21 names that were renamed/removed
        assert!(!is_builtin_type("CoinInfo"));
        assert!(!is_builtin_type("QualifiedCoinInfo"));
        assert!(!is_builtin_type("SendResult"));
        assert!(!is_builtin_type("CurvePoint"));
        assert!(!is_builtin_type("Void"));
        assert!(!is_builtin_type("Address"));
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
