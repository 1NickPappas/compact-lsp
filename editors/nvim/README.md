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

## Features (Phase 1)

- Diagnostics from `compactc` compiler on save
- Document synchronization

## Keymaps

Standard LSP keymaps apply:
- `gd` - Go to definition (future)
- `K` - Hover info (future)
- `[d` / `]d` - Navigate diagnostics
- `<leader>ld` - Show diagnostic float
