//! Resolution concerns for the parser engine.

use super::*;

impl ParserEngine {
    /// Look up the type of a variable by name.
    ///
    /// Returns the declared type annotation when present, otherwise a type
    /// inferred from a `const x = f();` initializer call. Same-file `type`
    /// aliases are followed to their target, so a contract reference is
    /// reachable even when it is written through an alias.
    pub fn get_variable_type(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let raw = self.find_variable_type(root, root, source_bytes, variable_name)?;
        Some(self.resolve_type_name(root, source_bytes, &raw))
    }

    /// Like [`Self::get_variable_type`], but returns the declared type text
    /// unchanged (generics preserved) — needed for element-type extraction.
    pub fn get_variable_type_raw(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        self.find_variable_type(root, root, source.as_bytes(), variable_name)
    }

    /// Resolve a written type name through same-file `type` aliases.
    pub fn resolve_alias(&mut self, source: &str, type_name: &str) -> String {
        match self.parse(source) {
            Some(tree) => self.resolve_type_name(tree.root_node(), source.as_bytes(), type_name),
            None => type_name.to_string(),
        }
    }

    /// Declared return type of a circuit/witness/contract-circuit named `name`.
    pub fn get_callable_return_type(&mut self, source: &str, name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        self.find_callable_return_type(tree.root_node(), source.as_bytes(), name)
    }

    /// Declared signature (parameter types, generic params, return type) of a
    /// circuit or witness by name (`circuit id<T>(x: T): T`).
    pub fn get_callable_signature(
        &mut self,
        source: &str,
        name: &str,
    ) -> Option<CallableSignature> {
        let tree = self.parse(source)?;
        self.find_callable_signature(tree.root_node(), source.as_bytes(), name)
    }

    pub(super) fn find_callable_signature(
        &self,
        node: Node,
        source: &[u8],
        name: &str,
    ) -> Option<CallableSignature> {
        if matches!(node.kind(), "cdefn" | "wdecl" | "ecdecl_circuit")
            && self.get_field_text(node, "id", source).as_deref() == Some(name)
        {
            let mut param_types = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if matches!(child.kind(), "parg" | "arg")
                    && let Some(ty) = self.get_field_text(child, "type", source)
                {
                    param_types.push(ty);
                }
            }
            return Some(CallableSignature {
                param_types,
                generic_params: self
                    .get_field_text(node, "gparams", source)
                    .unwrap_or_default(),
                return_type: self.get_field_text(node, "type", source),
            });
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_callable_signature(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// For an unannotated `const x = f();` binding, return the callee name `f`.
    /// Lets the caller infer `x`'s type when `f` is defined in another file.
    pub fn get_initializer_callee(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        self.find_initializer_callee(tree.root_node(), source.as_bytes(), variable_name)
    }

    /// Recursively search the AST for a declaration of `variable_name` and
    /// return its type (annotation, or inferred from the initializer).
    ///
    /// Handles ledger fields (`ldecl`), circuit/witness parameters (`parg`), and
    /// typed/`const` bindings (`const x: T = ...` / `const x = f()`). A contract
    /// reference is typically a `parg` or an `ldecl`, so this powers
    /// `ref.circuit(...)` member completion.
    pub(super) fn find_variable_type(
        &self,
        root: Node,
        node: Node,
        source: &[u8],
        variable_name: &str,
    ) -> Option<String> {
        match node.kind() {
            "ldecl" => {
                if self.get_field_text(node, "name", source).as_deref() == Some(variable_name) {
                    return self.get_field_text(node, "type", source);
                }
            }
            "parg" | "const_binding" | "cbinding" => {
                if let Some(pattern) = node.child_by_field_name("pattern")
                    && self.pattern_binds(pattern, source, variable_name)
                {
                    if let Some(ty) = self.get_field_text(node, "type", source) {
                        return Some(ty);
                    }
                    // `const t = f();` with no annotation: use `f`'s return type.
                    if let Some(value) = node.child_by_field_name("value")
                        && let Some(ty) = self.infer_call_return_type(root, source, value)
                    {
                        return Some(ty);
                    }
                }
            }
            _ => {}
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(result) = self.find_variable_type(root, child, source, variable_name) {
                return Some(result);
            }
        }

        None
    }

    /// Follow same-file `type Name = Target;` aliases (chained) to the final
    /// head type name.
    pub(super) fn resolve_type_name(&self, root: Node, source: &[u8], type_text: &str) -> String {
        let mut current = Self::head_type_name(type_text);
        let mut seen = std::collections::HashSet::new();
        // Bounded by `seen` (cycles) and a hard cap as a safety net.
        while seen.insert(current.clone()) && seen.len() <= 32 {
            match self.find_type_alias(root, source, &current) {
                Some(target) => current = Self::head_type_name(&target),
                None => break,
            }
        }
        current
    }

    /// The head identifier of a type expression (`Foo<Bar>` -> `Foo`).
    pub(super) fn head_type_name(type_text: &str) -> String {
        type_text
            .split('<')
            .next()
            .unwrap_or(type_text)
            .trim()
            .to_string()
    }

    /// Find a non-generic `type_alias_declaration` named `name` and return its
    /// target type text. Generic aliases are skipped (we cannot substitute args).
    pub(super) fn find_type_alias(&self, node: Node, source: &[u8], name: &str) -> Option<String> {
        if node.kind() == "type_alias_declaration"
            && node.child_by_field_name("gparams").is_none()
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return self.get_field_text(node, "type", source);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_type_alias(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// If `value` is (a parenthesized) call to a circuit declared in this file,
    /// return that circuit's declared return type.
    pub(super) fn infer_call_return_type(
        &self,
        root: Node,
        source: &[u8],
        value: Node,
    ) -> Option<String> {
        let call = self.find_call_term(value)?;
        let fun = call.child_by_field_name("fun")?;
        let callee = fun.child_by_field_name("id")?;
        let callee_name = self.node_text(callee, source);
        self.find_callable_return_type(root, source, &callee_name)
    }

    /// Descend through transparent expression wrappers to the first call term.
    pub(super) fn find_call_term<'a>(&self, node: Node<'a>) -> Option<Node<'a>> {
        match node.kind() {
            "function_call_term" => return Some(node),
            "expr" | "term" | "expr_seq" | "expr_seq_term" => {}
            _ => return None,
        }
        let mut cursor = node.walk();
        node.named_children(&mut cursor)
            .find_map(|child| self.find_call_term(child))
    }

    /// Find a circuit/witness/contract-circuit definition named `name` and
    /// return its declared return type.
    pub(super) fn find_callable_return_type(
        &self,
        node: Node,
        source: &[u8],
        name: &str,
    ) -> Option<String> {
        if matches!(node.kind(), "cdefn" | "wdecl" | "ecdecl_circuit")
            && self.get_field_text(node, "id", source).as_deref() == Some(name)
        {
            return self.get_field_text(node, "type", source);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_callable_return_type(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Find an unannotated `const x = f();` binding of `variable_name` and
    /// return the callee name `f`.
    pub(super) fn find_initializer_callee(
        &self,
        node: Node,
        source: &[u8],
        variable_name: &str,
    ) -> Option<String> {
        if matches!(node.kind(), "const_binding" | "cbinding")
            && node.child_by_field_name("type").is_none()
            && let Some(pattern) = node.child_by_field_name("pattern")
            && self.pattern_binds(pattern, source, variable_name)
            && let Some(value) = node.child_by_field_name("value")
            && let Some(call) = self.find_call_term(value)
            && let Some(fun) = call.child_by_field_name("fun")
            && let Some(id) = fun.child_by_field_name("id")
        {
            return Some(self.node_text(id, source));
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_initializer_callee(child, source, variable_name) {
                return Some(found);
            }
        }
        None
    }

    /// Whether a `pattern` node binds `name` (simple or destructuring).
    pub(super) fn pattern_binds(&self, pattern: Node, source: &[u8], name: &str) -> bool {
        if let Some(id) = pattern.child_by_field_name("id") {
            return self.node_text(id, source) == name;
        }
        let mut cursor = pattern.walk();
        pattern
            .children(&mut cursor)
            .any(|child| self.pattern_binds(child, source, name))
    }

    /// Find a contract type (`contract Name { … }`) by name and return its
    /// declared circuits. Used for cross-contract-call intelligence.
    pub fn contract_type(&mut self, source: &str, name: &str) -> Option<ContractTypeInfo> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let ecdecl = self.find_ecdecl(root, source.as_bytes(), name)?;
        Some(self.contract_type_info(ecdecl, source.as_bytes()))
    }

    /// Recursively find an `ecdecl` with the given name.
    pub(super) fn find_ecdecl<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        name: &str,
    ) -> Option<Node<'a>> {
        if node.kind() == "ecdecl"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_ecdecl(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Build the info for an `ecdecl` node, including its declared circuits.
    pub(super) fn contract_type_info(&self, node: Node, source: &[u8]) -> ContractTypeInfo {
        let name = self
            .get_field_text(node, "name", source)
            .unwrap_or_default();
        let mut circuits = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() != "ecdecl_circuit" {
                continue;
            }
            let Some(circuit_name) = self.get_field_text(child, "id", source) else {
                continue;
            };
            let params = self.extract_params(child, source);
            let return_type = self
                .get_field_text(child, "type", source)
                .unwrap_or_default();
            let detail = format!("({}): {}", params, return_type);
            let signature = format!("circuit {}{}", circuit_name, detail);
            let documentation = self
                .extract_doc_comment(child, source)
                .map(|doc| format!("---\n\n{}", doc));
            let name_location = child
                .child_by_field_name("id")
                .map(|id| self.node_to_symbol_location(id))
                .unwrap_or_else(|| self.node_to_symbol_location(child));
            circuits.push(ContractCircuit {
                name: circuit_name,
                detail,
                signature,
                documentation,
                location: self.node_to_symbol_location(child),
                name_location,
            });
        }
        ContractTypeInfo {
            name,
            circuits,
            location: self.node_to_symbol_location(node),
        }
    }
}
