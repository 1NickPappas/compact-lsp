# compact-lsp

Language Server Protocol implementation for the [Compact](https://docs.midnight.network/develop/reference/compact/lang-ref) smart contract language (Midnight network).

## Features

### Implemented

| Feature | Description |
|---------|-------------|
| Diagnostics | Shows compiler errors and warnings from `compactc` on save |
| Document Sync | Tracks file changes with incremental updates |
| Completion | Keywords, built-in types, snippets, and cross-project symbols via imports |
| Formatting | Format document using `format-compact` |
| Document Symbols | Outline view of circuits, structs, enums, modules, etc. |
| Folding Ranges | Code folding for blocks, functions, control flow |
| Hover | Documentation for keywords, types, and symbols (including imported) |
| Go to Definition | Navigate to symbol definitions (same file and imported files) |
| Signature Help | Function parameter hints (including imported functions) |

### Cross-Project Support

The LSP supports Compact's import system with prefixes:

```compact
import "./Utils" prefix Utils_;

// These all work cross-project:
// - Completion: type "Utils_" to see Utils_add
// - Hover: hover on Utils_add to see signature
// - Go to Definition: jump to add() in Utils.compact
// - Signature Help: see (a: Field, b: Field) while typing
Utils_add(5, 5);
```

### Roadmap

| Feature | Status | Description |
|---------|--------|-------------|
| Semantic Tokens | Planned | Rich syntax highlighting via LSP |
| Find References | Planned | Find all usages of a symbol |
| Rename | Planned | Rename symbols across files |
| Code Actions | Planned | Quick fixes and refactoring |

## Requirements

- Rust toolchain (for building)
- `compactc` compiler (from Midnight devtools)

## Building

```bash
cargo build --release
```

The binary will be at `target/release/compact-lsp`.

## Neovim Setup

### 1. Create LSP config

Add to `~/.config/nvim/lua/core/lsp/configs/compact_lsp.lua`:

```lua
return {
    cmd = { vim.fn.expand("~/path/to/compact-lsp/target/release/compact-lsp") },
    filetypes = { "compact" },
    root_markers = { ".git", "compact.toml", "package.json" },
}
```

### 2. Register filetype and enable LSP

Add to your LSP configuration:

```lua
-- Register .compact file extension
vim.filetype.add({
    extension = {
        compact = "compact",
    },
})

-- Load and register compact_lsp
local compact_config = require("core.lsp.configs.compact_lsp")
vim.lsp.config("compact_lsp", compact_config)

-- Enable for .compact files
vim.api.nvim_create_autocmd("FileType", {
    pattern = { "compact" },
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

## Compiler Location

The LSP auto-detects `compactc.bin` in this order:
1. `COMPACT_COMPILER` environment variable
2. `~/compactc/compactc.bin`
3. `compactc.bin` in PATH

## Related Projects

- [compact.vim](../compact.vim) - Vim/Neovim syntax highlighting and filetype support
- [tree-sitter-compact](https://github.com/midnight-ntwrk/tree-sitter-compact) - Tree-sitter grammar for Compact

## License

Apache-2.0
