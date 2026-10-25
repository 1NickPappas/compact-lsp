//! Unit tests for the builtins module.

use super::*;

#[test]
fn test_extract_base_type_simple() {
    assert_eq!(extract_base_type("Counter"), "Counter");
}

#[test]
fn test_extract_base_type_parameterized() {
    assert_eq!(extract_base_type("Map<Address, Uint<64>>"), "Map");
}

#[test]
fn test_extract_base_type_single_param() {
    assert_eq!(extract_base_type("Set<Address>"), "Set");
}

#[test]
fn test_methods_for_counter() {
    let methods = methods_for_type("Counter");
    assert_eq!(methods.len(), 5);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"increment"));
    assert!(names.contains(&"decrement"));
    assert!(names.contains(&"read"));
    assert!(names.contains(&"lessThan"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_cell() {
    let methods = methods_for_type("Cell");
    assert_eq!(methods.len(), 4);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"read"));
    assert!(names.contains(&"write"));
    assert!(names.contains(&"writeCoin"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_map() {
    let methods = methods_for_type("Map");
    assert_eq!(methods.len(), 9);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"insert"));
    assert!(names.contains(&"insertDefault"));
    assert!(names.contains(&"insertCoin"));
    assert!(names.contains(&"remove"));
    assert!(names.contains(&"lookup"));
    assert!(names.contains(&"member"));
    assert!(names.contains(&"isEmpty"));
    assert!(names.contains(&"size"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_set() {
    let methods = methods_for_type("Set");
    assert_eq!(methods.len(), 7);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"insert"));
    assert!(names.contains(&"insertCoin"));
    assert!(names.contains(&"remove"));
    assert!(names.contains(&"member"));
    assert!(names.contains(&"isEmpty"));
    assert!(names.contains(&"size"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_list() {
    let methods = methods_for_type("List");
    assert_eq!(methods.len(), 7);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"pushFront"));
    assert!(names.contains(&"pushFrontCoin"));
    assert!(names.contains(&"popFront"));
    assert!(names.contains(&"head"));
    assert!(names.contains(&"length"));
    assert!(names.contains(&"isEmpty"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_merkle_tree() {
    let methods = methods_for_type("MerkleTree");
    assert_eq!(methods.len(), 8);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"insert"));
    assert!(names.contains(&"insertIndex"));
    assert!(names.contains(&"insertHash"));
    assert!(names.contains(&"insertHashIndex"));
    assert!(names.contains(&"insertIndexDefault"));
    assert!(names.contains(&"checkRoot"));
    assert!(names.contains(&"isFull"));
    assert!(names.contains(&"resetToDefault"));
}

#[test]
fn test_methods_for_historic_merkle_tree() {
    let methods = methods_for_type("HistoricMerkleTree");
    assert_eq!(methods.len(), 9);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"insert"));
    assert!(names.contains(&"insertIndex"));
    assert!(names.contains(&"insertHash"));
    assert!(names.contains(&"insertHashIndex"));
    assert!(names.contains(&"insertIndexDefault"));
    assert!(names.contains(&"checkRoot"));
    assert!(names.contains(&"isFull"));
    assert!(names.contains(&"resetToDefault"));
    assert!(names.contains(&"resetHistory"));
}

#[test]
fn test_kernel_caller_gated_at_0_26_105() {
    let before = Some(LanguageVersion::new(0, 26, 104));
    let after = Some(LanguageVersion::new(0, 26, 105));
    assert!(find_method_by_name_for("Kernel", "caller", before).is_none());
    assert!(find_method_by_name_for("Kernel", "caller", after).is_some());
}

#[test]
fn test_methods_for_kernel() {
    let methods = methods_for_type("Kernel");
    assert_eq!(methods.len(), 17);
    let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
    assert!(names.contains(&"self"));
    assert!(names.contains(&"caller"));
    assert!(names.contains(&"balance"));
    assert!(names.contains(&"balanceGreaterThan"));
    assert!(names.contains(&"balanceLessThan"));
    assert!(names.contains(&"mintShielded"));
    assert!(names.contains(&"mintUnshielded"));
    assert!(!names.contains(&"mint"));
    assert!(names.contains(&"blockTimeGreaterThan"));
    assert!(names.contains(&"blockTimeLessThan"));
    assert!(names.contains(&"checkpoint"));
    assert!(names.contains(&"claimContractCall"));
    assert!(names.contains(&"claimZswapCoinReceive"));
    assert!(names.contains(&"claimZswapCoinSpend"));
    assert!(names.contains(&"claimZswapNullifier"));
    assert!(names.contains(&"claimUnshieldedCoinSpend"));
    assert!(names.contains(&"incUnshieldedInputs"));
    assert!(names.contains(&"incUnshieldedOutputs"));
}

#[test]
fn test_methods_for_unknown() {
    let methods = methods_for_type("Boolean");
    assert!(methods.is_empty());
}

#[test]
fn test_find_method_by_name_found() {
    let method = find_method_by_name("Counter", "increment");
    assert!(method.is_some());
    assert_eq!(method.unwrap().name, "increment");
}

#[test]
fn test_find_method_by_name_not_found() {
    assert!(find_method_by_name("Counter", "unknown_method").is_none());
}

#[test]
fn test_find_method_by_name_unknown_type() {
    assert!(find_method_by_name("Boolean", "increment").is_none());
}

#[test]
fn test_version_gated_method_lookup() {
    let mainnet = LanguageVersion::new(0, 23, 0);

    // No built-in method is version-gated yet, so the filtered lookup matches
    // the unfiltered one for both a detected version and an absent compiler.
    for language in [Some(mainnet), None] {
        let filtered = methods_for_type_for("Counter", language);
        assert_eq!(filtered.len(), methods_for_type("Counter").len());
        assert!(find_method_by_name_for("Counter", "increment", language).is_some());
        assert!(find_method_by_name_for("Counter", "unknown_method", language).is_none());
    }
}

#[test]
fn test_all_methods_have_required_fields() {
    for ptype in &registry().types {
        assert!(!ptype.name.is_empty(), "Type name must not be empty");
        assert!(
            !ptype.methods.is_empty(),
            "Type {} must have at least one method",
            ptype.name
        );
        for method in &ptype.methods {
            assert!(
                !method.name.is_empty(),
                "{}.{}: name must not be empty",
                ptype.name,
                method.name
            );
            assert!(
                !method.signature.is_empty(),
                "{}.{}: signature must not be empty",
                ptype.name,
                method.name
            );
            assert!(
                !method.snippet.is_empty(),
                "{}.{}: snippet must not be empty",
                ptype.name,
                method.name
            );
            assert!(
                !method.documentation.is_empty(),
                "{}.{}: documentation must not be empty",
                ptype.name,
                method.name
            );
        }
    }
}

#[test]
fn test_all_types_have_descriptions() {
    for ptype in &registry().types {
        assert!(
            !ptype.description.is_empty(),
            "Type {} must have a non-empty description",
            ptype.name
        );
        assert!(
            !ptype.doc_url.is_empty(),
            "Type {} must have a non-empty doc_url",
            ptype.name
        );
    }
}

#[test]
fn test_generate_doc_file_counter() {
    let info = doc_cache()
        .get("Counter")
        .expect("Counter doc should exist");
    let content = std::fs::read_to_string(&info.path).expect("Counter.md should be readable");

    assert!(content.starts_with("# Counter\n"));
    assert!(content.contains("Simple incrementing/decrementing counter"));
    assert!(content.contains("[Ledger data types]"));
    assert!(content.contains("## Methods"));
    assert!(content.contains("### `increment(amount: Uint<16>)`"));
    assert!(content.contains("### `decrement(amount: Uint<16>)`"));
    assert!(content.contains("### `read(): Uint<64>`"));
}

#[test]
fn test_get_builtin_type_doc_location() {
    // Types with methods should return Some
    let loc = get_builtin_type_doc_location("Counter");
    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert!(loc.uri.starts_with("file://"));
    assert!(loc.uri.ends_with("Counter.md"));
    assert_eq!(loc.line, 0);

    // Types without methods should return None
    assert!(get_builtin_type_doc_location("Boolean").is_none());
    assert!(get_builtin_type_doc_location("Field").is_none());
}

#[test]
fn test_get_builtin_method_doc_location() {
    // Known method should return Some with correct line
    let loc = get_builtin_method_doc_location("Counter", "increment");
    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert!(loc.uri.ends_with("Counter.md"));
    assert!(loc.line > 0);

    // Unknown method should return None
    assert!(get_builtin_method_doc_location("Counter", "nonexistent").is_none());

    // Unknown type should return None
    assert!(get_builtin_method_doc_location("Boolean", "increment").is_none());
}

#[test]
fn test_method_line_numbers_are_correct() {
    let info = doc_cache()
        .get("Counter")
        .expect("Counter doc should exist");
    let content = std::fs::read_to_string(&info.path).expect("Counter.md should be readable");
    let lines: Vec<&str> = content.lines().collect();

    for (method_name, &line_num) in &info.method_lines {
        let line = lines.get(line_num as usize).unwrap_or_else(|| {
            panic!(
                "Line {} for method {} is out of range (file has {} lines)",
                line_num,
                method_name,
                lines.len()
            )
        });
        assert!(
            line.starts_with("### `"),
            "Line {} for method {} should start with '### `', got: {}",
            line_num,
            method_name,
            line
        );
    }
}

#[test]
fn test_has_builtin_methods() {
    assert!(has_builtin_methods("Counter"));
    assert!(has_builtin_methods("Cell"));
    assert!(has_builtin_methods("Map"));
    assert!(has_builtin_methods("Kernel"));
    assert!(!has_builtin_methods("Boolean"));
    assert!(!has_builtin_methods("Field"));
    assert!(!has_builtin_methods("NonExistent"));
}

#[test]
fn test_type_param_names() {
    assert_eq!(type_param_names("Map"), vec!["K", "V"]);
    assert_eq!(type_param_names("List"), vec!["T"]);
    assert_eq!(type_param_names("MerkleTree"), vec!["n", "T"]);
    assert!(type_param_names("Counter").is_empty());
}

#[test]
fn test_method_return_type_generic_substitution() {
    let map_args = vec!["Address".to_string(), "Token".to_string()];
    assert_eq!(
        method_return_type("Map", &map_args, "lookup").as_deref(),
        Some("Token")
    );
    // A method with no declared return type.
    assert_eq!(method_return_type("Map", &map_args, "insert"), None);
    // A non-generic return type is passed through unchanged.
    assert_eq!(
        method_return_type("Map", &map_args, "member").as_deref(),
        Some("Boolean")
    );
    // A generic return type substitutes the receiver's argument.
    let list_args = vec!["Token".to_string()];
    assert_eq!(
        method_return_type("List", &list_args, "head").as_deref(),
        Some("Maybe<Token>")
    );
    // Unknown method / type.
    assert_eq!(method_return_type("Map", &map_args, "nope"), None);
    assert_eq!(method_return_type("Nope", &[], "lookup"), None);
}
