//! Signature-help handling for the server.

use super::*;

impl CompactLanguageServer {
    /// Build a SignatureHelp response from SignatureInfo.
    pub(super) fn build_signature_help_response(
        &self,
        info: compact_analyzer::SignatureInfo,
    ) -> SignatureHelp {
        let parameters: Vec<ParameterInformation> = info
            .parameters
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.label.clone()),
                documentation: None,
            })
            .collect();

        let signature = SignatureInformation {
            label: info.label,
            documentation: info.documentation.map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d,
                })
            }),
            parameters: Some(parameters),
            active_parameter: Some(info.active_parameter),
        };

        SignatureHelp {
            signatures: vec![signature],
            active_signature: Some(0),
            active_parameter: Some(info.active_parameter),
        }
    }
    pub(super) async fn handle_signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let sig_info = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.signature_help(&content, position.line, position.character)
        };

        if let Some(info) = sig_info {
            return Ok(Some(self.build_signature_help_response(info)));
        }

        // Cross-contract call: `reference.circuit(...)` where `reference` is a
        // value of a contract type.
        let member_call = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_enclosing_member_call(&content, position.line, position.character)
        };
        if let Some(ctx) = member_call
            && let Some((_file, contract)) =
                self.resolve_base_contract_type(&uri, &content, &ctx.base)
            && let Some(circuit) = contract.circuits.iter().find(|c| c.name == ctx.member_name)
        {
            let active_param =
                utils::count_commas_before_cursor(&content, position.line, position.character);
            let params = utils::parse_params_from_detail(&circuit.detail);
            let parameters: Vec<ParameterInformation> = params
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(p.clone()),
                    documentation: None,
                })
                .collect();
            let signature = SignatureInformation {
                label: format!(
                    "{}.{}",
                    contract.name,
                    circuit
                        .signature
                        .strip_prefix("circuit ")
                        .unwrap_or(&circuit.signature)
                ),
                documentation: circuit.documentation.clone().map(|d| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: d,
                    })
                }),
                parameters: Some(parameters),
                active_parameter: Some(active_param),
            };
            return Ok(Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            }));
        }

        let func_name =
            match utils::get_function_call_name(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        if let Some((_file_uri, symbol)) = self.find_imported_symbol(&uri, &func_name)
            && let Some(detail) = &symbol.detail
        {
            let active_param =
                utils::count_commas_before_cursor(&content, position.line, position.character);
            let params = utils::parse_params_from_detail(detail);

            let parameters: Vec<ParameterInformation> = params
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(p.clone()),
                    documentation: None,
                })
                .collect();

            let label = format!("circuit {}{}", func_name, detail);
            let signature = SignatureInformation {
                label,
                documentation: symbol.documentation.map(|d| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: d,
                    })
                }),
                parameters: Some(parameters),
                active_parameter: Some(active_param),
            };

            return Ok(Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            }));
        }

        // Stdlib circuit function signature help
        let active_param =
            utils::count_commas_before_cursor(&content, position.line, position.character);
        if let Some(help) = stdlib_signature_help(&func_name, active_param, self.language_version) {
            return Ok(Some(help));
        }

        Ok(None)
    }
}
