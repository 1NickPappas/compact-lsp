//! Unit tests for the stdlib module.

use super::*;

#[test]
fn test_all_structs_have_fields() {
    for st in all_stdlib_structs() {
        assert!(!st.name.is_empty(), "Struct name must not be empty");
        // Opaque/native types and type aliases have no public fields.
        if !matches!(
            st.name,
            "JubjubPoint"
                | "PublicAddress"
                | "Secp256r1Base"
                | "Secp256r1Scalar"
                | "Secp256r1Point"
                | "Curve25519Base"
                | "Curve25519Scalar"
                | "Curve25519Point"
        ) {
            assert!(
                !st.fields.is_empty(),
                "Struct {} must have at least one field",
                st.name
            );
        }
        assert!(
            !st.description.is_empty(),
            "Struct {} must have a description",
            st.name
        );
        assert!(
            !st.doc_url.is_empty(),
            "Struct {} must have a doc_url",
            st.name
        );
    }
}

#[test]
fn test_all_circuits_have_required_fields() {
    for circ in all_stdlib_circuits() {
        assert!(!circ.name.is_empty(), "Circuit name must not be empty");
        assert!(
            !circ.signature.is_empty(),
            "Circuit {} must have a signature",
            circ.name
        );
        assert!(
            !circ.snippet.is_empty(),
            "Circuit {} must have a snippet",
            circ.name
        );
        assert!(
            !circ.doc.is_empty(),
            "Circuit {} must have a doc",
            circ.name
        );
    }
}

#[test]
fn test_find_stdlib_struct_maybe() {
    let st = find_stdlib_struct("Maybe").expect("Maybe should exist");
    assert_eq!(st.name, "Maybe");
    assert_eq!(st.type_params, "<T>");
    let field_names: Vec<&str> = st.fields.iter().map(|f| f.name).collect();
    assert!(field_names.contains(&"is_some"));
    assert!(field_names.contains(&"value"));
}

#[test]
fn test_find_stdlib_struct_unknown() {
    assert!(find_stdlib_struct("NonExistent").is_none());
}

#[test]
fn test_find_stdlib_circuit_some() {
    let circ = find_stdlib_circuit("some").expect("some should exist");
    assert_eq!(circ.name, "some");
    assert!(circ.signature.contains("Maybe<T>"));
}

#[test]
fn test_find_stdlib_circuit_unknown() {
    assert!(find_stdlib_circuit("nonExistent").is_none());
}

#[test]
fn test_generate_doc_file_shielded_coin_info() {
    let info = doc_cache()
        .structs
        .get("ShieldedCoinInfo")
        .expect("ShieldedCoinInfo doc should exist");
    let content =
        std::fs::read_to_string(&info.path).expect("ShieldedCoinInfo.md should be readable");

    assert!(content.starts_with("# ShieldedCoinInfo\n"));
    assert!(content.contains("shielded coin"));
    assert!(content.contains("[Compact Standard Library]"));
    assert!(content.contains("## Fields"));
    assert!(content.contains("### `nonce: Bytes<32>`"));
    assert!(content.contains("### `color: Bytes<32>`"));
    assert!(content.contains("### `value: Uint<128>`"));
}

#[test]
fn test_generate_doc_file_circuits() {
    let cache = doc_cache();
    let content = std::fs::read_to_string(&cache.circuits.path)
        .expect("StdlibCircuits.md should be readable");

    assert!(content.starts_with("# Standard Library Circuits\n"));
    assert!(content.contains("## `circuit some<T>(value: T): Maybe<T>`"));
    assert!(content.contains("## `circuit sendShielded("));
    assert!(content.contains("## `circuit blockTimeLt("));
}

#[test]
fn test_get_stdlib_struct_doc_location() {
    let loc = get_stdlib_struct_doc_location("ShieldedCoinInfo");
    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert!(loc.uri.starts_with("file://"));
    assert!(loc.uri.ends_with("ShieldedCoinInfo.md"));
    assert_eq!(loc.line, 0);
}

#[test]
fn test_get_stdlib_circuit_doc_location() {
    let loc = get_stdlib_circuit_doc_location("sendShielded");
    assert!(loc.is_some());
    let loc = loc.unwrap();
    assert!(loc.uri.ends_with("StdlibCircuits.md"));
    assert!(loc.line > 0);
}

#[test]
fn test_version_gating_hides_keccak_on_023_0() {
    let mainnet = LanguageVersion::new(0, 23, 0);
    let newer = LanguageVersion::new(0, 23, 103);

    // keccak256 was added in language 0.23.103.
    assert!(find_stdlib_circuit_for("keccak256", Some(mainnet)).is_none());
    assert!(find_stdlib_circuit_for("keccak256", Some(newer)).is_some());

    // Stable symbols remain visible on 0.23.0.
    assert!(find_stdlib_circuit_for("persistentHash", Some(mainnet)).is_some());
    assert!(find_stdlib_struct_for("JubjubPoint", Some(mainnet)).is_some());

    // Unknown version (no compiler) does not filter.
    assert!(find_stdlib_circuit_for("keccak256", None).is_some());
}

#[test]
fn test_version_gating_listing() {
    let mainnet = LanguageVersion::new(0, 23, 0);
    let listed = all_stdlib_circuits_for(Some(mainnet));
    assert!(!listed.iter().any(|c| c.name == "keccak256"));
    assert!(listed.iter().any(|c| c.name == "persistentHash"));

    let all = all_stdlib_circuits_for(None);
    assert!(all.iter().any(|c| c.name == "keccak256"));
}

#[test]
fn test_is_available_gating_logic() {
    let detected = Some(LanguageVersion::new(0, 23, 0));

    // Empty bounds -> always available.
    assert!(is_available_range("", "", detected));
    assert!(is_available_range("", "", None));
    // Unparseable bounds -> err toward showing.
    assert!(is_available_range("not-a-version", "", detected));
    assert!(is_available_range("", "not-a-version", detected));
    // At or above the minimum -> available.
    assert!(is_available_range("0.23.0", "", detected));
    assert!(is_available_range("0.22.99", "", detected));
    // Above the minimum -> hidden.
    assert!(!is_available_range("0.23.1", "", detected));
    assert!(!is_available_range("0.23.103", "", detected));
    // Maximum is exclusive: a symbol removed in 0.24.0 is available on
    // 0.23.0 and hidden on 0.24.0 and newer.
    assert!(is_available_range("", "0.24.0", detected));
    assert!(!is_available_range("", "0.23.0", detected));
    assert!(is_available_range("0.23.0", "0.24.0", detected));
    // Unknown detected version (no compiler) -> gating disabled.
    assert!(is_available_range("0.23.103", "", None));
    assert!(is_available_range("", "0.23.0", None));
}

#[test]
fn test_version_gating_new_symbols() {
    let v230 = Some(LanguageVersion::new(0, 23, 0));
    let v26105 = Some(LanguageVersion::new(0, 26, 105));
    let v26106 = Some(LanguageVersion::new(0, 26, 106));
    let v270 = Some(LanguageVersion::new(0, 27, 0));

    // jubjubSchnorrVerify landed at language 0.23.104.
    assert!(find_stdlib_circuit_for("jubjubSchnorrVerify", v230).is_none());
    assert!(find_stdlib_struct_for("JubjubSchnorrSignature", v230).is_none());
    assert!(find_stdlib_circuit_for("jubjubSchnorrVerify", v26105).is_some());

    // PublicAddress / kernel.caller landed at language 0.26.105.
    assert!(
        find_stdlib_struct_for("PublicAddress", Some(LanguageVersion::new(0, 26, 104))).is_none()
    );
    assert!(find_stdlib_struct_for("PublicAddress", v26105).is_some());

    // sha512 landed at language 0.26.103; Curve25519 at 0.26.102.
    assert!(find_stdlib_circuit_for("sha512", Some(LanguageVersion::new(0, 26, 102))).is_none());
    assert!(find_stdlib_circuit_for("sha512", v26105).is_some());
    assert!(
        find_stdlib_struct_for("Curve25519Point", Some(LanguageVersion::new(0, 26, 101))).is_none()
    );
    assert!(find_stdlib_struct_for("Curve25519Point", v270).is_some());

    // secp256r1EcdsaVerify / ed25519Verify landed at language 0.26.106.
    assert!(
        find_stdlib_circuit_for(
            "secp256r1EcdsaVerify",
            Some(LanguageVersion::new(0, 26, 105))
        )
        .is_none()
    );
    assert!(find_stdlib_circuit_for("secp256r1EcdsaVerify", v26106).is_some());
    assert!(find_stdlib_circuit_for("ed25519Verify", v26106).is_some());

    // Unknown version (no compiler) shows everything.
    assert!(find_stdlib_circuit_for("ed25519Verify", None).is_some());
}

#[test]
fn test_registry_uses_current_names() {
    // 0.21+ names present.
    assert!(find_stdlib_struct("ShieldedCoinInfo").is_some());
    assert!(find_stdlib_struct("QualifiedShieldedCoinInfo").is_some());
    assert!(find_stdlib_struct("ShieldedSendResult").is_some());
    assert!(find_stdlib_struct("UserAddress").is_some());
    assert!(find_stdlib_struct("JubjubPoint").is_some());
    assert!(find_stdlib_circuit("sendShielded").is_some());
    assert!(find_stdlib_circuit("mintShieldedToken").is_some());
    assert!(find_stdlib_circuit("mintUnshieldedToken").is_some());
    assert!(find_stdlib_circuit("unshieldedBalance").is_some());
    // Added in language 0.23.10x.
    assert!(find_stdlib_circuit("keccak256").is_some());

    // Pre-0.21 names gone.
    assert!(find_stdlib_struct("CoinInfo").is_none());
    assert!(find_stdlib_struct("SendResult").is_none());
    assert!(find_stdlib_struct("CurvePoint").is_none());
    assert!(find_stdlib_circuit("send").is_none());
    assert!(find_stdlib_circuit("mintToken").is_none());
    assert!(find_stdlib_circuit("burnAddress").is_none());
}

#[test]
fn test_field_line_numbers_are_correct() {
    for (struct_name, info) in &doc_cache().structs {
        let content = std::fs::read_to_string(&info.path).expect("Doc file should be readable");
        let lines: Vec<&str> = content.lines().collect();

        for (field_name, &line_num) in &info.field_lines {
            let line = lines.get(line_num as usize).unwrap_or_else(|| {
                panic!(
                    "Line {} for {}.{} is out of range (file has {} lines)",
                    line_num,
                    struct_name,
                    field_name,
                    lines.len()
                )
            });
            assert!(
                line.starts_with("### `"),
                "Line {} for {}.{} should start with '### `', got: {}",
                line_num,
                struct_name,
                field_name,
                line
            );
        }
    }
}

#[test]
fn test_circuit_line_numbers_are_correct() {
    let cache = doc_cache();
    let content = std::fs::read_to_string(&cache.circuits.path)
        .expect("StdlibCircuits.md should be readable");
    let lines: Vec<&str> = content.lines().collect();

    for (circuit_name, &line_num) in &cache.circuits.circuit_lines {
        let line = lines.get(line_num as usize).unwrap_or_else(|| {
            panic!(
                "Line {} for circuit {} is out of range (file has {} lines)",
                line_num,
                circuit_name,
                lines.len()
            )
        });
        assert!(
            line.starts_with("## `circuit "),
            "Line {} for circuit {} should start with '## `circuit ', got: {}",
            line_num,
            circuit_name,
            line
        );
    }
}
