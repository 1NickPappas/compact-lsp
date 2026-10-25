//! Resolver helpers for the server.

use super::*;

impl CompactLanguageServer {
    /// Resolve a contract type by name, looking first in the current file and
    /// then in imported/workspace files. Returns the defining URI and its info.
    pub(super) fn resolve_contract_type(
        &self,
        current_uri: &str,
        type_name: &str,
    ) -> Option<(String, ContractTypeInfo)> {
        // 1. The file itself (open buffer or cached scan).
        if let Some(content) = self.source_for(current_uri)
            && let Some(info) = self.parse_contract_type(&content, type_name)
        {
            return Some((current_uri.to_string(), info));
        }

        // 2. An imported symbol (handles `prefix` imports).
        if let Some((file_uri, symbol)) = self.find_imported_symbol(current_uri, type_name)
            && symbol.kind == CompletionSymbolKind::Contract
            && let Some(content) = self.source_for(&file_uri)
            && let Some(info) = self.parse_contract_type(&content, &symbol.name)
        {
            return Some((file_uri, info));
        }

        // 3. Any workspace file that declares a contract type with this name.
        let candidates: Vec<String> = self
            .symbol_cache
            .iter()
            .filter(|entry| {
                entry
                    .value()
                    .iter()
                    .any(|s| s.kind == CompletionSymbolKind::Contract && s.name == type_name)
            })
            .map(|entry| entry.key().clone())
            .collect();
        for file_uri in candidates {
            let content = self.source_for(&file_uri);
            if let Some(content) = content
                && let Some(info) = self.parse_contract_type(&content, type_name)
            {
                return Some((file_uri, info));
            }
        }
        None
    }

    pub(super) fn parse_contract_type(
        &self,
        content: &str,
        name: &str,
    ) -> Option<ContractTypeInfo> {
        let mut parser = self.parser_engine.lock().unwrap();
        parser.contract_type(content, name)
    }

    /// Resolve a bare symbol name: a `prefix` import first, then any workspace file.
    pub(super) fn find_symbol_anywhere(
        &self,
        current_uri: &str,
        name: &str,
    ) -> Option<(String, CompletionSymbol)> {
        if let Some(found) = self.find_imported_symbol(current_uri, name) {
            return Some(found);
        }
        for entry in self.symbol_cache.iter() {
            if let Some(symbol) = entry.value().iter().find(|s| s.name == name) {
                return Some((entry.key().clone(), symbol.clone()));
            }
        }
        None
    }

    /// Resolve the contract type of a value named `var_name` in `content`.
    ///
    /// Handles a written annotation, same-file aliases, and inference from a
    /// `const x = f();` initializer — including when `f` is defined in another
    /// file, whose return type is then resolved **in that file's scope** (type
    /// names are file-scoped, so the callee's file decides what the name means).
    pub(super) fn resolve_variable_contract_type(
        &self,
        uri: &str,
        content: &str,
        var_name: &str,
    ) -> Option<(String, ContractTypeInfo)> {
        // 1. Same-file resolution (annotation / alias / local call inference).
        let type_name = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_variable_type(content, var_name)
        };
        if let Some(type_name) = type_name
            && let Some(found) = self.resolve_contract_type(uri, &type_name)
        {
            return Some(found);
        }

        // 2. Cross-file inference: `const x = callee();`.
        let callee = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_initializer_callee(content, var_name)
        }?;
        let (raw, scope_uri) = self.resolve_callee_raw_type(uri, &callee)?;
        let name = self.resolve_alias_in(&scope_uri, &raw);
        self.resolve_contract_type(&scope_uri, &name)
    }

    /// Resolve a callee's declared return type and the file scope it lives in.
    pub(super) fn resolve_callee_raw_type(
        &self,
        uri: &str,
        callee: &str,
    ) -> Option<(String, String)> {
        let (callee_uri, callee_symbol) = self.find_symbol_anywhere(uri, callee)?;
        let callee_content = self.source_for(&callee_uri)?;
        let raw = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_callable_return_type(&callee_content, &callee_symbol.name)?
        };
        Some((raw, callee_uri))
    }

    /// Resolve a (possibly generic) call's return type, instantiating generic
    /// parameters from the argument types at this call site. Type names stay in
    /// the callee's file scope.
    pub(super) fn resolve_call_raw_type(
        &self,
        uri: &str,
        content: &str,
        callee: &str,
        args: &[MemberBase],
        explicit: &[String],
    ) -> Option<(String, String)> {
        let (callee_uri, callee_symbol) = self.find_symbol_anywhere(uri, callee)?;
        let callee_content = self.source_for(&callee_uri)?;
        let signature = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_callable_signature(&callee_content, &callee_symbol.name)?
        };
        let return_type = signature.return_type?;
        if signature.generic_params.is_empty() {
            return Some((return_type, callee_uri));
        }
        let params = builtins::parse_type_params(&signature.generic_params);
        let mut type_args: Vec<String> = params.iter().map(|p| p.to_string()).collect();
        let mut bound = vec![false; params.len()];
        // 1. Explicit type arguments (`first<Uint<64>>(m)`) bind in order. Compact
        //    requires these for generic calls, and they cover parameters that
        //    can't be inferred from arguments (`none<T>()`, `Maybe<T>` params).
        //    They are written in the caller's scope, so the result is resolved
        //    there (handled by returning `uri` when anything was substituted).
        for (i, ta) in explicit.iter().enumerate() {
            if i >= params.len() {
                break;
            }
            let ta = ta.trim();
            if !ta.is_empty() {
                type_args[i] = ta.to_string();
                bound[i] = true;
            }
        }
        // 2. Infer any remaining parameter from an argument whose declared
        //    parameter type is exactly that parameter (`circuit id<T>(x: T)` +
        //    `id(token)`). This is a superset of the compiler, which requires
        //    explicit arguments, but keeps existing call sites working.
        for (i, param_type) in signature.param_types.iter().enumerate() {
            let param_type = param_type.trim();
            if let Some(idx) = params.iter().position(|p| *p == param_type)
                && !bound[idx]
                && let Some(Some(actual)) = args.get(i).map(|arg| {
                    self.resolve_base_raw_type(uri, content, arg)
                        .map(|(t, _)| t)
                })
            {
                type_args[idx] = actual;
                bound[idx] = true;
            }
        }
        let instantiated = builtins::substitute_generic_params(
            &return_type,
            &signature.generic_params,
            &type_args,
        );
        // Substituted text was authored in the caller's file; only an
        // unsubstituted return type belongs to the callee's scope.
        let scope = if bound.iter().any(|b| *b) {
            uri.to_string()
        } else {
            callee_uri
        };
        Some((instantiated, scope))
    }

    /// Resolve a member-access base to its declared type text and the file scope
    /// in which that text is written. Generics are preserved (needed to extract
    /// a container's element type for index access).
    pub(super) fn resolve_base_raw_type(
        &self,
        uri: &str,
        content: &str,
        base: &MemberBase,
    ) -> Option<(String, String)> {
        match base {
            MemberBase::Path(chain) => {
                let root = chain.first()?;
                let mut raw = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.get_variable_type_raw(content, root)?
                };
                let mut scope_uri = uri.to_string();
                let mut scope_content = content.to_string();
                for member in &chain[1..] {
                    let args = generic_args(&raw);
                    let (struct_uri, struct_content, struct_name) =
                        self.find_struct_home(&scope_uri, &scope_content, &raw)?;
                    let (field_raw, params) = {
                        let mut parser = self.parser_engine.lock().unwrap();
                        (
                            parser.get_struct_field_type(&struct_content, &struct_name, member)?,
                            parser
                                .get_struct_type_params(&struct_content, &struct_name)
                                .unwrap_or_default(),
                        )
                    };
                    raw = builtins::substitute_generic_params(&field_raw, &params, &args);
                    scope_uri = struct_uri;
                    scope_content = struct_content;
                }
                Some((raw, scope_uri))
            }
            MemberBase::Call {
                callee,
                args,
                type_args,
            } => self.resolve_call_raw_type(uri, content, callee, args, type_args),
            MemberBase::MethodCall(receiver, method) => {
                let (recv_raw, scope_uri) = self.resolve_base_raw_type(uri, content, receiver)?;
                let head = builtins::extract_base_type(&recv_raw).trim();
                let aliased = self.resolve_alias_in(&scope_uri, head);
                // (a) The receiver is a contract type: use the called circuit's
                //     declared return type, written in the contract's own file.
                if let Some((contract_uri, contract)) =
                    self.resolve_contract_type(&scope_uri, &aliased)
                    && let Some(circuit) = contract.circuits.iter().find(|c| c.name == *method)
                    && let Some(ret) = return_type_of_signature(&circuit.detail)
                {
                    return Some((ret, contract_uri));
                }
                // (b) The receiver is a built-in container: use the registry's
                //     declared return type with generic arguments substituted.
                let args = generic_args(&recv_raw);
                let ret = builtins::method_return_type(head, &args, method)?;
                Some((ret, scope_uri))
            }
            MemberBase::Index(inner) => {
                let (container, scope_uri) = self.resolve_base_raw_type(uri, content, inner)?;
                Some((element_type(&container)?, scope_uri))
            }
            MemberBase::Access(receiver, field) => {
                let (recv_raw, scope_uri) = self.resolve_base_raw_type(uri, content, receiver)?;
                let head = builtins::extract_base_type(&recv_raw).trim();
                // (a) A struct declared in the workspace.
                let scope_content = self
                    .source_for(&scope_uri)
                    .unwrap_or_else(|| content.to_string());
                if let Some((struct_uri, struct_content, struct_name)) =
                    self.find_struct_home(&scope_uri, &scope_content, &recv_raw)
                {
                    let args = generic_args(&recv_raw);
                    let (field_raw, params) = {
                        let mut parser = self.parser_engine.lock().unwrap();
                        (
                            parser.get_struct_field_type(&struct_content, &struct_name, field)?,
                            parser
                                .get_struct_type_params(&struct_content, &struct_name)
                                .unwrap_or_default(),
                        )
                    };
                    let field_type =
                        builtins::substitute_generic_params(&field_raw, &params, &args);
                    return Some((field_type, struct_uri));
                }
                // (b) A standard-library struct (`Maybe`, `Either`, …), with
                //     the receiver's generic arguments substituted.
                let args = generic_args(&recv_raw);
                let field_type = stdlib::struct_field_type(head, &args, field)?;
                Some((field_type, scope_uri))
            }
            MemberBase::Other => None,
        }
    }

    /// Follow aliases for a type name written in `scope_uri`'s file.
    pub(super) fn resolve_alias_in(&self, scope_uri: &str, name: &str) -> String {
        match self.source_for(scope_uri) {
            Some(content) => {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.resolve_alias(&content, name)
            }
            None => name.to_string(),
        }
    }

    /// Locate the file that declares a struct `name` (following aliases),
    /// returning `(file_uri, file_content, struct_name)`.
    pub(super) fn find_struct_home(
        &self,
        uri: &str,
        content: &str,
        name: &str,
    ) -> Option<(String, String, String)> {
        let aliased = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.resolve_alias(content, name)
        };
        let local = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.struct_exists(content, &aliased)
        };
        if local {
            return Some((uri.to_string(), content.to_string(), aliased));
        }
        let (file_uri, symbol) = self.find_symbol_anywhere(uri, &aliased)?;
        let file_content = self.source_for(&file_uri)?;
        let local = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.resolve_alias(&file_content, &symbol.name)
        };
        Some((file_uri, file_content, local))
    }

    /// Locate the file that declares an `enum` `name` (following aliases),
    /// returning `(file_uri, file_content, enum_name)`.
    pub(super) fn find_enum_home(
        &self,
        uri: &str,
        content: &str,
        name: &str,
    ) -> Option<(String, String, String)> {
        let aliased = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.resolve_alias(content, name)
        };
        let local = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.enum_exists(content, &aliased)
        };
        if local {
            return Some((uri.to_string(), content.to_string(), aliased));
        }
        let (file_uri, symbol) = self.find_symbol_anywhere(uri, &aliased)?;
        if symbol.kind != CompletionSymbolKind::Enum {
            return None;
        }
        let file_content = self.source_for(&file_uri)?;
        let local = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.resolve_alias(&file_content, &symbol.name)
        };
        Some((file_uri, file_content, local))
    }

    /// Resolve the contract type of a member-access base expression.
    pub(super) fn resolve_base_contract_type(
        &self,
        uri: &str,
        content: &str,
        base: &MemberBase,
    ) -> Option<(String, ContractTypeInfo)> {
        if let MemberBase::Path(chain) = base
            && chain.len() == 1
        {
            return self.resolve_variable_contract_type(uri, content, &chain[0]);
        }
        let (raw, scope_uri) = self.resolve_base_raw_type(uri, content, base)?;
        let name = self.resolve_alias_in(&scope_uri, &raw);
        self.resolve_contract_type(&scope_uri, &name)
    }

    /// Get the symbol names to search for in a given file (handles import prefixes).
    pub(super) fn get_search_names_for_file(
        &self,
        searching_file: &str,
        defining_file: &str,
        symbol_name: &str,
    ) -> Vec<String> {
        let mut names = Vec::new();

        let content = match self.source_cache.get(searching_file) {
            Some(c) => c.clone(),
            None => return names,
        };

        let file_imports = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(&content)
        };

        for import in file_imports {
            if !import.is_file {
                continue;
            }
            if let Some(resolved) = imports::resolve_import_path(searching_file, &import.path)
                && resolved == defining_file
            {
                // Selective imports may bind the symbol under an alias.
                for (element, alias) in &import.elements {
                    if element == symbol_name {
                        names.push(alias.clone().unwrap_or_else(|| element.clone()));
                    }
                }
                let prefixed_name = match &import.prefix {
                    Some(prefix) => format!("{}{}", prefix, symbol_name),
                    None => symbol_name.to_string(),
                };
                names.push(prefixed_name);
            }
        }

        if !names.contains(&symbol_name.to_string()) {
            names.push(symbol_name.to_string());
        }

        names
    }
    pub(super) fn find_imported_symbol(
        &self,
        current_uri: &str,
        name: &str,
    ) -> Option<(String, CompletionSymbol)> {
        let file_imports = {
            let content = self.source_for(current_uri)?;
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(&content)
        };

        for import in file_imports {
            if !import.is_file {
                continue;
            }

            let resolved_uri = match imports::resolve_import_path(current_uri, &import.path) {
                Some(uri) => uri,
                None => continue,
            };

            // Selective import: `import { Sym as Alias } from "./mod"`.
            // The local name is the alias (or the symbol name when unaliased).
            for (element, alias) in &import.elements {
                let bound = alias.as_deref().unwrap_or(element);
                if bound == name
                    && let Some(symbols) = self.symbol_cache.get(&resolved_uri)
                    && let Some(symbol) = symbols.iter().find(|s| s.name == *element)
                {
                    return Some((resolved_uri, symbol.clone()));
                }
            }

            // Prefix import: `import "./mod" prefix P_`.
            let prefix = match &import.prefix {
                Some(p) => p,
                None => continue,
            };

            if !name.starts_with(prefix) {
                continue;
            }

            let symbol_name = &name[prefix.len()..];
            if symbol_name.is_empty() {
                continue;
            }

            if let Some(symbols) = self.symbol_cache.get(&resolved_uri) {
                for symbol in symbols.iter() {
                    if symbol.name == symbol_name {
                        return Some((resolved_uri, symbol.clone()));
                    }
                }
            }
        }

        None
    }
}
