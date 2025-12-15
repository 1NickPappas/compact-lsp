# compact-lsp

Language Server Protocol implementation for the [Compact](https://docs.midnight.network/develop/reference/compact/lang-ref) smart contract language (Midnight network).

## Features

- **Diagnostics** - Shows compiler errors and warnings from `compactc`
- **Document Sync** - Tracks file changes for real-time feedback

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

## License

Apache-2.0
