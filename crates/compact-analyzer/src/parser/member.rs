//! Member concerns for the parser engine.

use super::*;

impl ParserEngine {
    /// Get member access context at a cursor position.
    ///
    /// If the cursor is on `increment` in `round.increment(1)`, returns
    /// `MemberAccessContext { base_name: "round", member_name: "increment", ... }`.
    pub fn get_member_access_context(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<MemberAccessContext> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        let point = self.point_from_utf16(line, character);

        // Find the deepest node at this position
        let node = root.descendant_for_point_range(point, point)?;

        // The node should be an identifier
        if node.kind() != "id" {
            return None;
        }

        // Its parent should be a member_access_expr
        let parent = node.parent()?;
        if parent.kind() != "member_access_expr" {
            return None;
        }

        // Verify this node is the member field (not the base)
        let member_node = parent.child_by_field_name("member")?;
        if member_node.id() != node.id() {
            return None;
        }

        // Extract the base expression (`w.token`, `make()`, `arr[i]`, …).
        let base_node = parent.child_by_field_name("base")?;
        let base = self.member_base(base_node, source_bytes);
        if base == MemberBase::Other {
            return None;
        }
        let base_name = Self::member_base_display(&base);
        let member_name = self.node_text(node, source_bytes);
        let member_range = self.node_range(node);

        Some(MemberAccessContext {
            base_name,
            base,
            member_name,
            member_range,
        })
    }

    /// Extract the identifier name from a base expression node.
    ///
    /// For simple identifiers this returns the text directly.
    /// For more complex expressions, tries to find an `id` child.
    /// Recursively collect member accesses named `member_name`.
    pub(super) fn collect_member_accesses_rec(
        &self,
        node: Node,
        source: &[u8],
        member_name: &str,
        out: &mut Vec<MemberAccessContext>,
    ) {
        if node.kind() == "member_access_expr"
            && let Some(member) = node.child_by_field_name("member")
            && self.node_text(member, source) == member_name
            && let Some(base) = node.child_by_field_name("base")
        {
            let member_base = self.member_base(base, source);
            if member_base != MemberBase::Other {
                out.push(MemberAccessContext {
                    base_name: Self::member_base_display(&member_base),
                    base: member_base,
                    member_name: member_name.to_string(),
                    member_range: self.node_range(member),
                });
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_member_accesses_rec(child, source, member_name, out);
        }
    }

    /// Model a member-access base expression (`w.token`, `make()`, `arr[i]`).
    pub(super) fn member_base(&self, node: Node, source: &[u8]) -> MemberBase {
        match node.kind() {
            "id" => MemberBase::Path(vec![self.node_text(node, source)]),
            "member_access_expr" => {
                let base = node
                    .child_by_field_name("base")
                    .map(|base| self.member_base(base, source));
                let member = node
                    .child_by_field_name("member")
                    .map(|member| self.node_text(member, source));
                // A `member_access_expr` with `arguments` is a method call
                // (`factory.create()`, `map.lookup(k)`), not a field access.
                let is_call = node.child_by_field_name("arguments").is_some();
                match (base, member) {
                    (Some(base), Some(member)) => {
                        if is_call {
                            MemberBase::MethodCall(Box::new(base), member)
                        } else {
                            match base {
                                MemberBase::Path(mut path) => {
                                    path.push(member);
                                    MemberBase::Path(path)
                                }
                                MemberBase::Other => MemberBase::Other,
                                other => MemberBase::Access(Box::new(other), member),
                            }
                        }
                    }
                    _ => MemberBase::Other,
                }
            }
            "function_call_term" => {
                let fun = node.child_by_field_name("fun");
                let callee = fun
                    .and_then(|fun| fun.child_by_field_name("id"))
                    .map(|id| self.node_text(id, source));
                match callee {
                    Some(callee) => {
                        // Explicit type arguments: `id<Uint<64>>(x)`. Compact
                        // requires these for generic calls, so read them rather
                        // than relying solely on argument inference.
                        let type_args = fun
                            .and_then(|fun| fun.child_by_field_name("gargs"))
                            .map(|gargs| {
                                let mut cursor = gargs.walk();
                                gargs
                                    .children_by_field_name("garg", &mut cursor)
                                    .map(|garg| self.node_text(garg, source).trim().to_string())
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        let mut args = Vec::new();
                        let mut cursor = node.walk();
                        for arg in node.children_by_field_name("expr", &mut cursor) {
                            args.push(self.member_base(arg, source));
                        }
                        MemberBase::Call {
                            callee,
                            args,
                            type_args,
                        }
                    }
                    None => MemberBase::Other,
                }
            }
            "index_access_expr" => match node.child_by_field_name("base") {
                Some(base) => MemberBase::Index(Box::new(self.member_base(base, source))),
                None => MemberBase::Other,
            },
            "term" | "expr" | "expr_seq" | "expr_seq_term" => {
                let mut cursor = node.walk();
                node.named_children(&mut cursor)
                    .next()
                    .map(|child| self.member_base(child, source))
                    .unwrap_or(MemberBase::Other)
            }
            _ => MemberBase::Other,
        }
    }

    /// Human-readable rendering of a member base, for hover/definition text.
    pub(super) fn member_base_display(base: &MemberBase) -> String {
        match base {
            MemberBase::Path(path) => path.join("."),
            MemberBase::Call { callee, .. } => format!("{}()", callee),
            MemberBase::MethodCall(receiver, method) => {
                format!("{}.{}()", Self::member_base_display(receiver), method)
            }
            MemberBase::Access(receiver, field) => {
                format!("{}.{}", Self::member_base_display(receiver), field)
            }
            MemberBase::Index(inner) => format!("{}[]", Self::member_base_display(inner)),
            MemberBase::Other => String::new(),
        }
    }

    /// Whether a `struct` named `name` is declared in this file.
    pub fn struct_exists(&mut self, source: &str, name: &str) -> bool {
        let Some(tree) = self.parse(source) else {
            return false;
        };
        let root = tree.root_node();
        let resolved = self.resolve_type_name(root, source.as_bytes(), name);
        self.find_struct(root, source.as_bytes(), &resolved)
            .is_some()
    }

    /// Whether an `enum` named `name` is declared in this file.
    pub fn enum_exists(&mut self, source: &str, name: &str) -> bool {
        let Some(tree) = self.parse(source) else {
            return false;
        };
        let root = tree.root_node();
        let resolved = self.resolve_type_name(root, source.as_bytes(), name);
        self.find_enum(root, source.as_bytes(), &resolved).is_some()
    }

    /// Declared type of a `struct` field (`struct S { field: T }`), following
    /// same-file aliases for the struct name.
    pub fn get_struct_field_type(
        &mut self,
        source: &str,
        struct_name: &str,
        field_name: &str,
    ) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, source_bytes, struct_name);
        let struct_node = self.find_struct(root, source_bytes, &resolved)?;
        let mut cursor = struct_node.walk();
        for child in struct_node.children(&mut cursor) {
            if child.kind() == "arg"
                && self.get_field_text(child, "id", source_bytes).as_deref() == Some(field_name)
            {
                return self.get_field_text(child, "type", source_bytes);
            }
        }
        None
    }

    /// Declared generic parameters of a `struct` (`struct Box<T>` → `"<T>"`),
    /// or `None` when the struct is not generic.
    pub fn get_struct_type_params(&mut self, source: &str, struct_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, source_bytes, struct_name);
        let struct_node = self.find_struct(root, source_bytes, &resolved)?;
        self.get_field_text(struct_node, "gparams", source_bytes)
    }

    /// If the position is on a struct-field declaration (`arg` inside a
    /// `struct`) or an enum-variant declaration (`id` inside an `enumdef`),
    /// return the owning type name, the member name, and the kind.
    #[allow(clippy::type_complexity)]
    pub fn type_member_at_position(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<(String, String, TypeMemberKind)> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let bytes = source.as_bytes();
        let point = self.point_from_utf16(line, character);
        let node = root.descendant_for_point_range(point, point)?;
        if node.kind() != "id" {
            return None;
        }
        let parent = node.parent()?;
        // Struct field: id → arg → struct.
        if parent.kind() == "arg" {
            let owner = parent.parent()?;
            if owner.kind() != "struct" {
                return None;
            }
            let owner_name = self.get_field_text(owner, "name", bytes)?;
            return Some((
                owner_name,
                self.node_text(node, bytes),
                TypeMemberKind::Field,
            ));
        }
        // Enum variant: id → enumdef.
        if parent.kind() == "enumdef" {
            let owner_name = self.get_field_text(parent, "name", bytes)?;
            return Some((
                owner_name,
                self.node_text(node, bytes),
                TypeMemberKind::Variant,
            ));
        }
        None
    }

    /// Range of a struct-field or enum-variant declaration's identifier.
    pub fn find_member_definition_location(
        &mut self,
        source: &str,
        owner_name: &str,
        member_name: &str,
        kind: TypeMemberKind,
    ) -> Option<Range> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, bytes, owner_name);
        match kind {
            TypeMemberKind::Field => {
                let struct_node = self.find_struct(root, bytes, &resolved)?;
                let mut cursor = struct_node.walk();
                for child in struct_node.children(&mut cursor) {
                    if child.kind() == "arg"
                        && self.get_field_text(child, "id", bytes).as_deref() == Some(member_name)
                        && let Some(id) = child.child_by_field_name("id")
                    {
                        return Some(self.node_range(id));
                    }
                }
                None
            }
            TypeMemberKind::Variant => {
                let enum_node = self.find_enum(root, bytes, &resolved)?;
                let mut cursor = enum_node.walk();
                for child in enum_node.children(&mut cursor) {
                    if child.kind() == "id" && self.node_text(child, bytes) == member_name {
                        return Some(self.node_range(child));
                    }
                }
                None
            }
        }
    }

    /// Recursively find a `struct` named `name`.
    pub(super) fn find_struct<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        name: &str,
    ) -> Option<Node<'a>> {
        if node.kind() == "struct"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_struct(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Recursively find an `enum` named `name`.
    pub(super) fn find_enum<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        name: &str,
    ) -> Option<Node<'a>> {
        if node.kind() == "enumdef"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_enum(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// All member accesses of `member_name` in the file, with their base names.
    /// Used to gate Find References / Rename by the base's contract type.
    pub fn collect_member_accesses(
        &mut self,
        source: &str,
        member_name: &str,
    ) -> Vec<MemberAccessContext> {
        let Some(tree) = self.parse(source) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        self.collect_member_accesses_rec(
            tree.root_node(),
            source.as_bytes(),
            member_name,
            &mut out,
        );
        out
    }

    /// If the position is on a circuit declared by a contract type
    /// (`ecdecl_circuit` inside `ecdecl`), return `(contract_name, circuit_name)`.
    pub fn circuit_at_position(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<(String, String)> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let point = self.point_from_utf16(line, character);
        let node = root.descendant_for_point_range(point, point)?;
        let parent = node.parent()?;
        if parent.kind() != "ecdecl_circuit" {
            return None;
        }
        // Ensure the cursor is on the circuit's name, not a type in its signature.
        let id_node = parent.child_by_field_name("id")?;
        if id_node.start_byte() != node.start_byte() || id_node.end_byte() != node.end_byte() {
            return None;
        }
        let circuit = self.node_text(id_node, source_bytes);
        let mut ancestor = parent.parent();
        while let Some(current) = ancestor {
            if current.kind() == "ecdecl" {
                let contract = self.get_field_text(current, "name", source_bytes)?;
                return Some((contract, circuit));
            }
            ancestor = current.parent();
        }
        None
    }

    /// Like [`Self::get_member_access_context`], but works anywhere inside the
    /// call's argument list. Used by signature help for `reference.circuit(...)`.
    pub fn get_enclosing_member_call(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<MemberAccessContext> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let point = self.point_from_utf16(line, character);
        let mut node = root.descendant_for_point_range(point, point)?;
        loop {
            if node.kind() == "member_access_expr"
                && node.child_by_field_name("arguments").is_some()
            {
                let member_node = node.child_by_field_name("member")?;
                let base_node = node.child_by_field_name("base")?;
                let base = self.member_base(base_node, source.as_bytes());
                if base == MemberBase::Other {
                    return None;
                }
                let base_name = Self::member_base_display(&base);
                let member_name = self.node_text(member_node, source.as_bytes());
                return Some(MemberAccessContext {
                    base_name,
                    base,
                    member_name,
                    member_range: self.node_range(member_node),
                });
            }
            node = node.parent()?;
        }
    }
}
