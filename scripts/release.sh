#!/usr/bin/env bash
#
# Cut a compact-lsp release: bump the workspace version, close the CHANGELOG
# Unreleased section, commit, and create an annotated tag.
#
# Pushing the tag triggers .github/workflows/release.yml (builds four platform
# binaries and publishes a GitHub Release).
#
# Usage:
#   scripts/release.sh <version> [--dry-run] [--push]
#
# Examples:
#   scripts/release.sh 0.2.0 --dry-run     # preview only
#   scripts/release.sh 0.2.0               # commit + tag locally
#   scripts/release.sh 0.2.0 --push        # also push main and the tag
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

die() {
    echo "error: $*" >&2
    exit 1
}

VERSION="${1:-}"
[ -n "$VERSION" ] || die "usage: scripts/release.sh <version> [--dry-run] [--push]"
[[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] \
    || die "version must be MAJOR.MINOR.PATCH (got: $VERSION)"

DRY_RUN=0
PUSH=0
for arg in "${@:2}"; do
    case "$arg" in
        --dry-run) DRY_RUN=1 ;;
        --push) PUSH=1 ;;
        *) die "unknown flag: $arg" ;;
    esac
done

TAG="v$VERSION"

# --- Preconditions ---------------------------------------------------------
BRANCH="$(git symbolic-ref --quiet --short HEAD || true)"
[ "$BRANCH" = "main" ] || die "must be on main (currently on '${BRANCH:-detached HEAD}')"

[ -z "$(git status --porcelain)" ] || die "working tree is dirty; commit or stash first"

if git rev-parse -q --verify "refs/tags/$TAG" >/dev/null 2>&1; then
    die "tag $TAG already exists locally"
fi
if git ls-remote --exit-code --tags origin "refs/tags/$TAG" >/dev/null 2>&1; then
    die "tag $TAG already exists on origin"
fi

CURRENT="$(awk '
    /^\[workspace\.package\]/ { inblock = 1; next }
    inblock && /^version = / { gsub(/[" ]/, "", $3); print $3; exit }
' Cargo.toml)"
[ -n "$CURRENT" ] || die "could not read workspace version from Cargo.toml"
[ "$CURRENT" != "$VERSION" ] || die "Cargo.toml is already at $VERSION"

grep -q '^## \[Unreleased\]' CHANGELOG.md || die "CHANGELOG.md has no [Unreleased] section"
UNRELEASED_BODY="$(awk '
    /^## \[Unreleased\]/ { inblock = 1; next }
    inblock && /^## \[/ { exit }
    inblock { print }
' CHANGELOG.md | tr -d '[:space:]')"
[ -n "$UNRELEASED_BODY" ] || die "CHANGELOG [Unreleased] is empty; add entries before releasing"

DATE="$(date +%Y-%m-%d)"

echo "Releasing compact-lsp ${CURRENT} -> ${VERSION}"
echo "  tag:  $TAG"
echo "  date: $DATE"

if [ "$DRY_RUN" = 1 ]; then
    echo "dry run: would update Cargo.toml + CHANGELOG.md, commit, and tag $TAG"
    exit 0
fi

# --- Apply -----------------------------------------------------------------
awk -v ver="$VERSION" '
    /^\[workspace\.package\]/ { inblock = 1; print; next }
    inblock && /^version = / { print "version = \"" ver "\""; inblock = 0; next }
    { print }
' Cargo.toml > Cargo.toml.tmp
mv Cargo.toml.tmp Cargo.toml

awk -v ver="$VERSION" -v date="$DATE" '
    /^## \[Unreleased\]/ { print; print ""; print "## [" ver "] - " date; next }
    { print }
' CHANGELOG.md > CHANGELOG.md.tmp
mv CHANGELOG.md.tmp CHANGELOG.md

# Refresh Cargo.lock for the new workspace version.
cargo metadata --format-version 1 >/dev/null

git add Cargo.toml Cargo.lock CHANGELOG.md
git commit -m "release: v$VERSION"
git tag -a "$TAG" -m "compact-lsp $TAG"

echo
echo "Committed and tagged $TAG."
if [ "$PUSH" = 1 ]; then
    git push origin main
    git push origin "$TAG"
    echo "Pushed. release.yml will now build and publish the GitHub Release."
else
    echo "Next steps:"
    echo "  git push origin main"
    echo "  git push origin $TAG"
fi
