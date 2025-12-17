# compact-lsp

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![CI](https://github.com/1NickPappas/compact-lsp/actions/workflows/ci.yml/badge.svg)](https://github.com/1NickPappas/compact-lsp/actions/workflows/ci.yml)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)

> ⚠️ **Note:** This is an experimental project developed in my personal free time for educational purposes — primarily to learn how Language Server Protocol implementations work. While functional, it is not officially supported. Feedback and contributions are welcome!

Language Server Protocol implementation for the [Compact](https://docs.midnight.network/develop/reference/compact/lang-ref) smart contract language (Midnight network).

## Features

| Feature | Description |
|---------|-------------|
| **Diagnostics** | Real-time syntax errors + compiler errors on save |
| **Semantic Tokens** | Rich syntax highlighting (functions, types, parameters, etc.) |
| **Completion** | Keywords, types, snippets, local and imported symbols |
| **Hover** | Documentation for keywords, types, and symbols |
| **Go to Definition** | Jump to symbol definitions (local and imported) |
| **Find References** | Find all usages of a symbol (local and cross-file) |
| **Rename** | Rename symbols across the workspace |
| **Signature Help** | Parameter hints while typing function calls |
| **Document Symbols** | Outline view (circuits, structs, enums, modules) |
| **Formatting** | Code formatting via `format-compact` |
| **Folding Ranges** | Code folding for blocks and functions |
| **Cross-file Errors** | Errors propagate to dependent files on save |

### Cross-Project Support

Works with Compact's import system:

```compact
import "./Utils" prefix Utils_;

Utils_add(5, 5);  // Completion, hover, go-to-def, find refs, rename, signature help all work
```

### Missing Things

| Feature | Status |
|---------|--------|
| Code Actions | TODO |

## Requirements

- Rust toolchain (for building)
- `compactc` compiler (for diagnostics)
- `format-compact` (optional, for formatting)

## Building

```bash
cargo build --release
```

Binary: `target/release/compact-lsp`

## Compiler Location

The LSP auto-detects `compactc.bin`:
1. `COMPACT_COMPILER` environment variable
2. `~/compactc/compactc.bin`
3. `compactc.bin` in PATH

## Related Projects

- [compact.vim](https://github.com/1NickPappas/compact.vim) - Vim/Neovim syntax highlighting
- [compact-tree-sitter](https://github.com/midnightntwrk/compact-tree-sitter) - Tree-sitter grammar

## Neovim Setup

### 1. Create LSP config

Create `~/.config/nvim/lua/lsp/compact.lua`:

```lua
return {
    cmd = { vim.fn.expand("~/path/to/compact-lsp/target/release/compact-lsp") },
    filetypes = { "compact" },
    root_markers = { ".git", "compact.toml", "package.json" },
}
```

### 2. Register filetype and enable LSP

Add to your Neovim config:

```lua
-- Register .compact filetype
vim.filetype.add({
    extension = { compact = "compact" },
})

-- Load and register compact LSP
local compact_config = require("lsp.compact")
vim.lsp.config("compact_lsp", compact_config)

-- Auto-enable for .compact files
vim.api.nvim_create_autocmd("FileType", {
    pattern = "compact",
    callback = function()
        vim.lsp.enable("compact_lsp")
    end,
})
```

### 3. Verify

Open a `.compact` file and run:
```vim
:LspInfo
```

### Optional: Enable semantic highlighting

Add to your config to use LSP semantic tokens for highlighting:

```lua
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client and client.server_capabilities.semanticTokensProvider then
            vim.lsp.semantic_tokens.start(args.buf, args.data.client_id)
        end
    end,
})
```

## License

MIT
