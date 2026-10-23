# Releasing `compact-lsp`

Releases are cut by pushing a `vX.Y.Z` tag. [`.github/workflows/release.yml`](./.github/workflows/release.yml)
then builds four platform binaries and publishes a GitHub Release with generated notes.

## Versioning

- Semantic Versioning (`MAJOR.MINOR.PATCH`); the project is pre-1.0, so breaking changes take a
  `MINOR` bump.
- The git tag and the workspace `version` in `Cargo.toml` must match (`v0.2.0` ↔ `0.2.0`).
- Keep [`CHANGELOG.md`](./CHANGELOG.md) current: each release closes the `## [Unreleased]` section.

## Cutting a release

```bash
# 1. Land everything on dev, then promote to main (dev is the integration line).
git checkout dev && git merge --ff-only main      # keep in sync
# ... develop, commit ...
git checkout main && git merge --ff-only dev

# 2. Make sure CHANGELOG.md has entries under "## [Unreleased]".

# 3. Bump, close the changelog, commit, and tag (dry-run first to preview).
scripts/release.sh 0.2.0 --dry-run
scripts/release.sh 0.2.0

# 4. Push main and the tag. The tag triggers two workflows:
#      release.yml  -> platform binaries + GitHub release
#      publish.yml  -> crates.io (compact-analyzer, then compact-lsp)
git push origin main
git push origin v0.2.0
```

The helper refuses to run unless you are on `main` with a clean tree, the tag does not already
exist, and the changelog has a non-empty `Unreleased` section.

### Manual equivalent

```bash
# edit Cargo.toml version, update CHANGELOG.md, then:
cargo metadata --format-version 1 >/dev/null   # refresh Cargo.lock
git add Cargo.toml Cargo.lock CHANGELOG.md
git commit -m "release: v0.2.0"
git tag -a v0.2.0 -m "compact-lsp v0.2.0"
git push origin main && git push origin v0.2.0
```

### With `gh` (after pushing the tag)

```bash
gh run watch "$(gh run list --workflow=release.yml --limit 1 --json databaseId --jq '.[0].databaseId')"
gh release view v0.2.0
```

## crates.io publishing (automated)

[`.github/workflows/publish.yml`](./.github/workflows/publish.yml) runs on tag push and publishes
both workspace crates **in order**: `compact-analyzer` first, then (after it appears on the index)
`compact-lsp`. It verifies the tag matches the workspace version and reruns the test gates first.

**Prerequisite (one-time):** add a repository secret
`CARGO_REGISTRY_TOKEN` (a crates.io API token with `publish-update` scope) at
`Settings → Secrets and variables → Actions`. Optionally create a `crates` environment to require
approval before publishing.

Manual run / preview:

```bash
# Validate packaging without uploading (workflow_dispatch with dry-run):
gh workflow run publish.yml -f dry-run=true
```

Publishing is idempotent: a crate version already on crates.io is skipped, so re-running after a
partial failure is safe. Manual fallback:

```bash
cargo login
cargo publish -p compact-analyzer
# wait for the index, then:
cargo publish -p compact-lsp
```

## Before tagging — checklist

- [ ] `cargo fmt --all --check`
- [ ] `cargo clippy --all-targets --all-features -- -D warnings`
- [ ] `cargo test` (incl. the `compiler-probe` job locally when a compiler is present)
- [ ] `CHANGELOG.md` `Unreleased` section is accurate and user-facing
- [ ] Version chosen per SemVer; `Cargo.toml` + tag agree
- [ ] `CARGO_REGISTRY_TOKEN` repository secret is set (for the automated crates.io publish)
- [ ] Upstream target in [`UPSTREAM_RELEASES.md`](./UPSTREAM_RELEASES.md) is current
