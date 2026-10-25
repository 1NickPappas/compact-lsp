//! Tokens concerns for the parser engine.

use super::*;

impl ParserEngine {
    /// Get syntax errors from tree-sitter parsing.
    ///
    /// Returns immediate syntax errors detected by tree-sitter.
    /// These are lightweight and fast - suitable for live diagnostics on every keystroke.
    pub fn get_syntax_errors(&mut self, source: &str) -> Vec<SyntaxError> {
        let tree = match self.parse(source) {
            Some(t) => t,
            None => return vec![],
        };

        let mut errors = Vec::new();
        self.collect_syntax_errors(tree.root_node(), source.as_bytes(), &mut errors);
        errors
    }

    /// Recursively collect syntax errors from the AST.
    pub(super) fn collect_syntax_errors(
        &self,
        node: Node,
        source: &[u8],
        errors: &mut Vec<SyntaxError>,
    ) {
        if node.is_error() {
            // ERROR node - unexpected token or invalid syntax
            let text = self.node_text(node, source);
            let message = if text.trim().is_empty() {
                "Syntax error: unexpected token".to_string()
            } else {
                format!(
                    "Syntax error: unexpected '{}'",
                    text.chars().take(30).collect::<String>()
                )
            };
            errors.push(SyntaxError {
                message,
                range: self.node_range(node),
            });
        } else if node.is_missing() {
            // MISSING node - expected token not found
            let kind = node.kind();
            let message = format!("Syntax error: missing {}", kind);
            errors.push(SyntaxError {
                message,
                range: self.node_range(node),
            });
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_syntax_errors(child, source, errors);
        }
    }

    /// Get semantic tokens for syntax highlighting.
    ///
    /// Returns tokens for circuits, types, parameters, etc. with semantic meaning.
    pub fn get_semantic_tokens(&mut self, source: &str) -> Vec<SemanticToken> {
        let tree = match self.parse(source) {
            Some(t) => t,
            None => return vec![],
        };

        let mut tokens = Vec::new();
        self.collect_semantic_tokens(tree.root_node(), source.as_bytes(), &mut tokens);

        // Sort by position (line, then character) - required for LSP delta encoding
        tokens.sort_by(|a, b| {
            a.range
                .start
                .line
                .cmp(&b.range.start.line)
                .then(a.range.start.character.cmp(&b.range.start.character))
        });

        tokens
    }

    /// Recursively collect semantic tokens from the AST.
    pub(super) fn collect_semantic_tokens(
        &self,
        node: Node,
        source: &[u8],
        tokens: &mut Vec<SemanticToken>,
    ) {
        match node.kind() {
            // Circuit/function definitions
            "cdefn" | "wdecl" => {
                // Get the function_name node which contains the actual name
                if let Some(name_node) = node
                    .children(&mut node.walk())
                    .find(|n| n.kind() == "function_name")
                {
                    tokens.push(SemanticToken {
                        range: self.node_range(name_node),
                        token_type: SemanticTokenType::Function,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                };
            }

            // Struct definitions
            "struct" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Struct,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
            }

            // Enum definitions
            "enumdef" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Enum,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
                // Also collect enum variants
                let mut cursor = node.walk();
                let enum_name = node
                    .child_by_field_name("name")
                    .map(|n| self.node_text(n, source));
                for child in node.children(&mut cursor) {
                    if child.kind() == "id" {
                        let text = self.node_text(child, source);
                        // Skip the enum name itself
                        if Some(&text) != enum_name.as_ref() {
                            tokens.push(SemanticToken {
                                range: self.node_range(child),
                                token_type: SemanticTokenType::EnumMember,
                                modifiers: vec![SemanticTokenModifier::Declaration],
                            });
                        }
                    }
                }
            }

            // Parameters (parg for circuits)
            // parg has: pattern (which contains id), type
            "parg" => {
                if let Some(pattern) = node.child_by_field_name("pattern") {
                    // Pattern can be an id, tuple, or struct pattern
                    // For simple identifiers, get the id field
                    if let Some(id) = pattern.child_by_field_name("id") {
                        tokens.push(SemanticToken {
                            range: self.node_range(id),
                            token_type: SemanticTokenType::Parameter,
                            modifiers: vec![],
                        });
                    }
                }
            }

            // Struct fields (arg within struct)
            "arg" => {
                // Check if parent is a struct
                let is_struct_field = node.parent().map(|p| p.kind()) == Some("struct");
                if let Some(name) = node.child_by_field_name("id") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: if is_struct_field {
                            SemanticTokenType::Property
                        } else {
                            SemanticTokenType::Parameter
                        },
                        modifiers: vec![],
                    });
                }
            }

            // Type references (user-defined types like struct names)
            "tref" => {
                if let Some(name) = node.child_by_field_name("id") {
                    let text = self.node_text(name, source);
                    let modifiers = if is_builtin_type(&text) {
                        vec![SemanticTokenModifier::DefaultLibrary]
                    } else {
                        vec![]
                    };
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Type,
                        modifiers,
                    });
                }
            }

            // Built-in types (these are literal string nodes in the grammar)
            // In tree-sitter, these show up as anonymous nodes or as type children
            "type" => {
                // Check for built-in type keywords as direct children
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    let kind = child.kind();
                    if kind == "Boolean" || kind == "Field" {
                        tokens.push(SemanticToken {
                            range: self.node_range(child),
                            token_type: SemanticTokenType::Type,
                            modifiers: vec![SemanticTokenModifier::DefaultLibrary],
                        });
                    }
                }
            }

            // Module definitions
            "mdefn" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Namespace,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
            }

            // Ledger declarations
            "ldecl" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Property,
                        modifiers: vec![
                            SemanticTokenModifier::Declaration,
                            SemanticTokenModifier::Readonly,
                        ],
                    });
                }
            }

            // Member access: `ref.transfer(...)` (method) or `info.value` (field).
            "member_access_expr" => {
                if let Some(member) = node.child_by_field_name("member") {
                    let is_call = node.child_by_field_name("arguments").is_some();
                    tokens.push(SemanticToken {
                        range: self.node_range(member),
                        token_type: if is_call {
                            SemanticTokenType::Function
                        } else {
                            SemanticTokenType::Property
                        },
                        modifiers: vec![],
                    });
                }
            }

            // New language keywords from grammar 0.3.0. The grammar models
            // these as anonymous literal children, so emit explicit keyword
            // tokens for them.
            "emit_term" => {
                self.push_keyword_token(node, "emit", tokens);
            }
            "contract_implements_declaration" => {
                self.push_keyword_token(node, "contract", tokens);
                self.push_keyword_token(node, "implements", tokens);
            }

            // Const bindings: `const <binding>, … ;` where each binding holds a
            // `pattern` field (multi-binding `const a = 1, b = 2;`). The crates.io
            // grammar names the binding `const_binding`; the old
            // `feat/grammar-v0.30.0` branch names it `cbinding`. There is no `let`.
            "const_stmt" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if matches!(child.kind(), "cbinding" | "const_binding")
                        && let Some(pattern) = child.child_by_field_name("pattern")
                    {
                        self.collect_binding_id_tokens(pattern, tokens);
                    }
                }
            }

            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_semantic_tokens(child, source, tokens);
        }
    }

    /// Push a `Keyword` semantic token for the direct child of `node` whose
    /// kind is `kind`. Used for the anonymous literal nodes of keywords.
    pub(super) fn push_keyword_token(
        &self,
        node: Node,
        kind: &str,
        tokens: &mut Vec<SemanticToken>,
    ) {
        let mut cursor = node.walk();
        if let Some(child) = node.children(&mut cursor).find(|c| c.kind() == kind) {
            tokens.push(SemanticToken {
                range: self.node_range(child),
                token_type: SemanticTokenType::Keyword,
                modifiers: vec![],
            });
        }
    }

    /// Emit a readonly variable-declaration token for every `id` bound by a
    /// `pattern` node. Handles simple (`const x = ...`) and destructuring
    /// (`const [a, b] = ...`, `const { f } = ...`) bindings.
    pub(super) fn collect_binding_id_tokens(&self, pattern: Node, tokens: &mut Vec<SemanticToken>) {
        if pattern.kind() == "id" {
            tokens.push(SemanticToken {
                range: self.node_range(pattern),
                token_type: SemanticTokenType::Variable,
                modifiers: vec![
                    SemanticTokenModifier::Declaration,
                    SemanticTokenModifier::Readonly,
                ],
            });
            return;
        }
        let mut cursor = pattern.walk();
        for child in pattern.children(&mut cursor) {
            self.collect_binding_id_tokens(child, tokens);
        }
    }
}
