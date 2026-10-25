//! Signature concerns for the parser engine.

use super::*;

impl ParserEngine {
    /// Get signature help for a function call at the given position.
    ///
    /// Returns signature information if the cursor is inside a function call.
    pub fn signature_help(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<SignatureInfo> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        // Convert LSP position to tree-sitter point
        let point = self.point_from_utf16(line, character);

        // Find the node at this position
        let node = root.descendant_for_point_range(point, point)?;

        // Walk up to find a function call expression
        let (call_node, func_name) = self.find_enclosing_call(node, source_bytes, point)?;

        // Count which parameter we're in (count commas before cursor)
        let active_param = self.count_active_parameter(call_node, point);

        // Find the function definition
        let def_node = self.find_definition_node(&func_name, root, source_bytes)?;

        // Build signature info
        self.build_signature_info(def_node, source_bytes, active_param)
    }

    /// Find an enclosing function call expression.
    pub(super) fn find_enclosing_call<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
        let mut current = Some(node);

        while let Some(n) = current {
            let kind = n.kind();

            // Check for function call patterns (Compact uses function_call_term)
            if kind == "function_call_term"
                && let Some(name) = self.get_call_function_name(n, source)
            {
                return Some((n, name));
            }

            // For blocks, search children for ERROR nodes with function calls
            // This handles incomplete code while typing
            if kind == "block"
                && let Some((error_node, name)) =
                    self.find_error_call_in_block(n, source, cursor_point)
            {
                return Some((error_node, name));
            }

            current = n.parent();
        }

        None
    }

    /// Search a block for ERROR nodes containing function calls before cursor.
    pub(super) fn find_error_call_in_block<'a>(
        &self,
        block: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
        let mut cursor = block.walk();

        for child in block.children(&mut cursor) {
            // Look for ERROR nodes or stmt containing ERROR
            if let Some(result) = self.find_error_call_recursive(child, source, cursor_point) {
                return Some(result);
            }
        }

        None
    }

    /// Recursively search for ERROR nodes with function calls.
    pub(super) fn find_error_call_recursive<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
        let kind = node.kind();

        if kind == "ERROR" {
            // Check if this ERROR has a function call and the cursor is after the opening paren
            if let Some(name) = self.get_call_function_name(node, source) {
                // Check if cursor is within or after this error node's range
                let start = node.start_position();
                let end = node.end_position();

                let after_start = cursor_point.row > start.row
                    || (cursor_point.row == start.row && cursor_point.column >= start.column);
                let before_end = cursor_point.row < end.row + 1
                    || (cursor_point.row == end.row && cursor_point.column <= end.column + 10);

                if after_start && before_end {
                    return Some((node, name));
                }
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(result) = self.find_error_call_recursive(child, source, cursor_point) {
                return Some(result);
            }
        }

        None
    }

    /// Get the function name from a call expression.
    pub(super) fn get_call_function_name(&self, node: Node, source: &[u8]) -> Option<String> {
        let kind = node.kind();

        // Handle both complete function_call_term and incomplete ERROR nodes
        if kind == "function_call_term" || kind == "ERROR" {
            // Look for a "fun" child containing "id"
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "fun" {
                    // Get the id inside fun
                    let mut inner_cursor = child.walk();
                    for inner_child in child.children(&mut inner_cursor) {
                        if inner_child.kind() == "id" {
                            return Some(self.node_text(inner_child, source));
                        }
                    }
                }
            }
        }

        None
    }

    /// Count which parameter the cursor is in (0-based).
    pub(super) fn count_active_parameter(
        &self,
        call_node: Node,
        cursor_point: tree_sitter::Point,
    ) -> u32 {
        let mut comma_count = 0;
        let mut in_args = false;

        let mut cursor = call_node.walk();
        for child in call_node.children(&mut cursor) {
            let child_kind = child.kind();

            // Start counting after opening paren
            if child_kind == "(" {
                in_args = true;
                continue;
            }

            // Stop at closing paren
            if child_kind == ")" {
                break;
            }

            if in_args && child_kind == "," {
                // Only count commas before the cursor
                if child.start_position().row < cursor_point.row
                    || (child.start_position().row == cursor_point.row
                        && child.start_position().column < cursor_point.column)
                {
                    comma_count += 1;
                }
            }
        }

        comma_count
    }

    /// Build SignatureInfo from a definition node.
    pub(super) fn build_signature_info(
        &self,
        def_node: Node,
        source: &[u8],
        active_param: u32,
    ) -> Option<SignatureInfo> {
        let kind = def_node.kind();

        let (prefix, name, doc) = match kind {
            "cdefn" => {
                // cdefn uses "function_name" for the circuit name
                let name = self.get_function_name(def_node, source)?;
                ("circuit", name, "Circuit function")
            }
            "wdecl" => {
                let name = self.get_function_name(def_node, source)?;
                ("witness", name, "Witness function")
            }
            _ => return None,
        };

        // Extract parameters
        let params = self.extract_param_infos(def_node, source);

        // Get return type
        let return_type = self.get_type_text(def_node, source).unwrap_or_default();

        // Build signature label
        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();
        let label = format!(
            "{} {}({}): {}",
            prefix,
            name,
            params_str.join(", "),
            return_type
        );

        Some(SignatureInfo {
            label,
            documentation: Some(doc.to_string()),
            parameters: params,
            active_parameter: active_param,
        })
    }

    /// Get function name from a cdefn/wdecl node.
    pub(super) fn get_function_name(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "function_name" {
                return Some(self.node_text(child, source));
            }
        }
        None
    }

    /// Get return type from a cdefn/wdecl node.
    pub(super) fn get_type_text(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type" {
                return Some(self.node_text(child, source));
            }
        }
        None
    }

    /// Extract parameter info for signature help.
    pub(super) fn extract_param_infos(&self, node: Node, source: &[u8]) -> Vec<ParameterInfo> {
        let mut params = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            // Compact uses "parg" for circuit parameters
            if child.kind() == "parg" || child.kind() == "arg" {
                let param_text = self.node_text(child, source);
                params.push(ParameterInfo { label: param_text });
            }
        }

        params
    }
}
