# Neovim Configuration for compact-lsp

## Setup

### 1. Add LSP config file

Create `~/.config/nvim/lua/core/lsp/configs/compact_lsp.lua`:

```lua
return {
    cmd = { vim.fn.expand("~/Documents/midnight/lsp/compact-lsp/target/release/compact-lsp") },
    filetypes = { "compact" },
    root_markers = { ".git", "compact.toml", "package.json" },
    settings = {
        compact = {},
    },
}
```

### 2. Register filetype and enable LSP

Add to your LSP configuration (e.g., `lua/core/lsp.lua`):

```lua
-- Register .compact file extension
vim.filetype.add({
    extension = {
        compact = "compact",
    },
})

-- Register compact_lsp configuration
local compact_config = require("core.lsp.configs.compact_lsp")
compact_config.on_attach = on_attach
compact_config.capabilities = capabilities
vim.lsp.config("compact_lsp", compact_config)

-- Enable compact_lsp for .compact files
vim.api.nvim_create_autocmd("FileType", {
    pattern = { "compact" },
    callback = function()
        vim.lsp.enable("compact_lsp")
    end,
})
```

## Usage

1. Open any `.compact` file
2. Check connection: `:LspInfo`
3. View logs: `:LspLog`

## Features

- **Diagnostics** - Compiler errors and warnings on save
- **Completion** - Keywords, types, snippets, and cross-project symbols
- **Hover** - Documentation for keywords, types, and symbols
- **Go to Definition** - Navigate to definitions (same file and imports)
- **Signature Help** - Parameter hints while typing function calls
- **Document Symbols** - Outline view (circuits, structs, enums, etc.)
- **Folding** - Code folding for blocks and functions
- **Formatting** - Format with `format-compact`

## Keymaps

Standard LSP keymaps apply:
- `gd` - Go to definition
- `K` - Hover documentation
- `[d` / `]d` - Navigate diagnostics
- `<leader>ld` - Show diagnostic float
- `<C-Space>` - Trigger completion (if configured)
