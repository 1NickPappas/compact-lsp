//! Unit tests for the text-based member-base parser.

use super::*;

fn path(parts: &[&str]) -> MemberBase {
    MemberBase::Path(parts.iter().map(|p| p.to_string()).collect())
}

#[test]
fn test_split_callable_type_args() {
    assert_eq!(
        split_callable_type_args("id<Uint<64>>"),
        ("id", vec!["Uint<64>".to_string()])
    );
    assert_eq!(
        split_callable_type_args("f<A, B>"),
        ("f", vec!["A".to_string(), "B".to_string()])
    );
    assert_eq!(split_callable_type_args("make"), ("make", Vec::new()));
    assert_eq!(
        split_callable_type_args(" id <Maybe<Field>> "),
        ("id", vec!["Maybe<Field>".to_string()])
    );
    // A `<…>` that is not a trailing suffix belongs to the text.
    assert_eq!(
        split_callable_type_args("a<b> + c"),
        ("a<b> + c", Vec::new())
    );
}

#[test]
fn test_split_type_args() {
    assert_eq!(
        split_type_args("Uint<64>, Maybe<Address>"),
        vec!["Uint<64>".to_string(), "Maybe<Address>".to_string()]
    );
    assert_eq!(split_type_args(""), Vec::<String>::new());
    assert_eq!(split_type_args(" , , "), Vec::<String>::new());
}

#[test]
fn test_parse_base_text_path() {
    assert_eq!(parse_base_text("w.token"), path(&["w", "token"]));
    assert_eq!(parse_base_text(" token "), path(&["token"]));
    assert_eq!(parse_base_text(""), MemberBase::Other);
}

#[test]
fn test_parse_base_text_call() {
    assert_eq!(
        parse_base_text("make()"),
        MemberBase::Call {
            callee: "make".to_string(),
            args: Vec::new(),
            type_args: Vec::new(),
        }
    );
    assert_eq!(
        parse_base_text("id<Uint<64>>(x)"),
        MemberBase::Call {
            callee: "id".to_string(),
            args: vec![path(&["x"])],
            type_args: vec!["Uint<64>".to_string()],
        }
    );
}

#[test]
fn test_parse_base_text_method_call() {
    assert_eq!(
        parse_base_text("factory.create()"),
        MemberBase::MethodCall(Box::new(path(&["factory"])), "create".to_string())
    );
}

#[test]
fn test_parse_base_text_index() {
    assert_eq!(
        parse_base_text("items[0]"),
        MemberBase::Index(Box::new(path(&["items"])))
    );
}

#[test]
fn test_parse_base_text_access_on_expression() {
    // `items[0].get().field` peels outside-in: field access on a method call
    // on an index on a path.
    assert_eq!(
        parse_base_text("items[0].get().field"),
        MemberBase::Access(
            Box::new(MemberBase::MethodCall(
                Box::new(MemberBase::Index(Box::new(path(&["items"])))),
                "get".to_string(),
            )),
            "field".to_string(),
        )
    );
}

#[test]
fn test_split_top_level_args() {
    assert_eq!(
        split_top_level_args("a, b.c, f(x)"),
        vec![path(&["a"]), path(&["b", "c"]), parse_base_text("f(x)")]
    );
    // Commas nested inside a call do not split.
    assert_eq!(
        split_top_level_args("f(a, b), c"),
        vec![
            MemberBase::Call {
                callee: "f".to_string(),
                args: vec![path(&["a"]), path(&["b"])],
                type_args: Vec::new(),
            },
            path(&["c"]),
        ]
    );
    assert_eq!(split_top_level_args(""), Vec::<MemberBase>::new());
}

#[test]
fn test_matching_open() {
    assert_eq!(matching_open("f(x)", '(', ')'), Some(1));
    assert_eq!(matching_open("items[0]", '[', ']'), Some(5));
    // The final closing bracket matches the final opening bracket.
    assert_eq!(matching_open("f(x)[y]", '[', ']'), Some(4));
    assert_eq!(matching_open("no brackets", '(', ')'), None);
}

#[test]
fn test_generic_args_and_element_type() {
    assert_eq!(
        generic_args("Map<Address, Token>"),
        vec!["Address".to_string(), "Token".to_string()]
    );
    assert_eq!(generic_args("Vector<10, Field>"), vec!["10", "Field"]);
    assert_eq!(generic_args("Field"), Vec::<String>::new());
    assert_eq!(generic_args("Map<>"), Vec::<String>::new());

    assert_eq!(
        element_type("Map<Address, Token>").as_deref(),
        Some("Token")
    );
    assert_eq!(element_type("Vector<10, Field>").as_deref(), Some("Field"));
    assert_eq!(element_type("Field"), None);
}

#[test]
fn test_return_type_of_signature() {
    assert_eq!(
        return_type_of_signature("(to: Address): Boolean").as_deref(),
        Some("Boolean")
    );
    assert_eq!(
        return_type_of_signature("transfer(to: Address, amount: Uint<64>): Boolean").as_deref(),
        Some("Boolean")
    );
    assert_eq!(return_type_of_signature("no return here"), None);
    assert_eq!(return_type_of_signature("(a): "), None);
}
