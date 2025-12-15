# Contributing to compact-lsp

Thank you for your interest in contributing to compact-lsp! This document provides guidelines and instructions for contributing.

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/compact-lsp.git`
3. Create a branch: `git checkout -b feature/your-feature-name`

## Development Setup

### Prerequisites

- Rust 1.70+ (install via [rustup](https://rustup.rs/))
- `compactc` compiler (for diagnostics)
- `format-compact` (for formatting)

### Building

```bash
cargo build
```

### Running Tests

```bash
cargo test
```

### Code Style

This project uses `rustfmt` and `clippy` for code formatting and linting:

```bash
# Format code
cargo fmt

# Check formatting (CI will fail if not formatted)
cargo fmt --check

# Run linter
cargo clippy -- -D warnings
```

## Making Changes

### Code Guidelines

- Follow Rust idioms and best practices
- Keep functions focused and small
- Add tests for new functionality
- Update documentation for API changes

### Commit Messages

- Use clear, descriptive commit messages
- Start with a verb (Add, Fix, Update, Remove, etc.)
- Keep the first line under 72 characters

Good examples:
- `Add hover support for ledger declarations`
- `Fix goto definition for imported symbols`
- `Update README with Neovim setup instructions`

## Submitting Changes

1. Ensure all tests pass: `cargo test`
2. Ensure code is formatted: `cargo fmt --check`
3. Ensure no clippy warnings: `cargo clippy -- -D warnings`
4. Push to your fork
5. Open a Pull Request

### Pull Request Guidelines

- Provide a clear description of the changes
- Reference any related issues
- Include screenshots for UI changes
- Add tests for new features

## Reporting Bugs

When reporting bugs, please include:

- Rust version (`rustc --version`)
- Operating system
- Editor/IDE and version
- Steps to reproduce
- Expected vs actual behavior
- Relevant log output (set `RUST_LOG=debug`)

## Requesting Features

Feature requests are welcome! Please:

- Check if the feature already exists or is planned
- Describe the use case
- Explain why this would benefit users

## Code of Conduct

Be respectful and inclusive. We're all here to build great software together.

## Questions?

Feel free to open an issue for any questions about contributing.
