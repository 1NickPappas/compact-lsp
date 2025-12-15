//! Compact LSP - Language Server for the Compact smart contract language
//!
//! # How this works
//!
//! 1. This binary is started by the editor (e.g., Neovim)
//! 2. Communication happens over stdin/stdout using JSON-RPC
//! 3. The editor sends requests (initialize, textDocument/*, etc.)
//! 4. We respond with results or send notifications (diagnostics, etc.)
//!
//! # Why we use stderr for logging
//!
//! Since stdin/stdout are used for the LSP protocol, we CANNOT use
//! println!() for debugging. Instead, we use the `tracing` crate
//! which writes to stderr.

mod server;

use tower_lsp::{LspService, Server};
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() {
    // Initialize logging to stderr
    // Set RUST_LOG=debug to see debug messages
    // Example: RUST_LOG=compact_lsp=debug cargo run
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("info"))
        )
        .with_writer(std::io::stderr) // IMPORTANT: write to stderr, not stdout
        .init();

    tracing::info!("Starting compact-lsp server");

    // Create stdin/stdout handles for the LSP transport
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    // Build the LSP service
    // LspService::build takes a closure that receives a Client
    // The Client is used to send notifications TO the editor (e.g., diagnostics)
    let (service, socket) = LspService::build(|client| {
        server::CompactLanguageServer::new(client)
    })
    .finish();

    // Start the server - this runs until the editor disconnects
    Server::new(stdin, stdout, socket).serve(service).await;

    tracing::info!("compact-lsp server stopped");
}
