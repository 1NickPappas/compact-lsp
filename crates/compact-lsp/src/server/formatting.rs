//! Formatting handlers for the server.

use super::*;

impl CompactLanguageServer {
    pub(super) async fn handle_formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let formatted = match self.formatter_engine.format(&content).await {
            Ok(formatted) => formatted,
            Err(e) => {
                self.client
                    .show_message(MessageType::ERROR, format!("Formatting failed: {}", e))
                    .await;
                return Ok(None);
            }
        };

        if formatted == content {
            return Ok(Some(vec![]));
        }

        let line_count = content.lines().count();
        let last_line = content.lines().last().unwrap_or("");

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: line_count as u32,
                character: last_line.len() as u32,
            },
        };

        Ok(Some(vec![TextEdit {
            range,
            new_text: formatted,
        }]))
    }

    pub(super) async fn handle_range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        // `format-compact` only formats whole files, so format everything and
        // keep only the changes that intersect the requested range.
        let formatted = match self.formatter_engine.format(&content).await {
            Ok(formatted) => formatted,
            Err(e) => {
                self.client
                    .show_message(MessageType::ERROR, format!("Formatting failed: {}", e))
                    .await;
                return Ok(None);
            }
        };

        if formatted == content {
            return Ok(Some(vec![]));
        }

        Ok(Some(range_format_edits(
            &content,
            &formatted,
            params.range.start.line,
            params.range.end.line,
        )))
    }
}
