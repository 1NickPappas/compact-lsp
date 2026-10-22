#!/usr/bin/env bash
#
# Report whether a newer upstream Compact toolchain has been released.
#
# Usage:
#   scripts/check-upstream.sh        # print a report
#   scripts/check-upstream.sh --ci   # also file a GitHub issue when newer
#
# The "known" version is read from .github/upstream-known.txt; bump it (and the
# table in UPSTREAM_RELEASES.md) when a release has been reviewed.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

UPSTREAM_REPO="midnightntwrk/compact"
KNOWN_FILE=".github/upstream-known.txt"

CI=0
[ "${1:-}" = "--ci" ] && CI=1

command -v gh >/dev/null 2>&1 || {
    echo "error: the GitHub CLI (gh) is required" >&2
    exit 1
}
[ -f "$KNOWN_FILE" ] || {
    echo "error: $KNOWN_FILE not found" >&2
    exit 1
}
KNOWN="$(tr -d '[:space:]' < "$KNOWN_FILE")"

# Newest toolchain release (tag `compactc-vX.Y.Z`), not the devtools (`compact-vX.Y.Z`).
LATEST_TAG="$(gh release list -R "$UPSTREAM_REPO" --limit 100 \
    --json tagName,publishedAt \
    --jq '[.[] | select(.tagName | startswith("compactc-v"))] | sort_by(.publishedAt) | last | .tagName')"
LATEST="${LATEST_TAG#compactc-v}"
[ -n "$LATEST" ] && [ "$LATEST" != "null" ] || {
    echo "error: could not determine the latest upstream release" >&2
    exit 1
}

echo "Tracked (reviewed) latest: $KNOWN"
echo "Latest upstream release:   $LATEST ($LATEST_TAG)"

# Returns success (0) when $1 > $2.
ver_gt() {
    awk -v a="$1" -v b="$2" 'BEGIN {
        n = split(a, A, "."); m = split(b, B, ".");
        k = (n > m) ? n : m;
        for (i = 1; i <= k; i++) {
            x = A[i] + 0; y = B[i] + 0;
            if (x > y) exit 0;
            if (x < y) exit 1;
        }
        exit 1;
    }'
}

if ver_gt "$LATEST" "$KNOWN"; then
    echo
    echo ">>> A newer Compact toolchain is available: $LATEST"
    echo "    Review: https://github.com/$UPSTREAM_REPO/releases/tag/$LATEST_TAG"
    echo "    Process: UPSTREAM_RELEASES.md"

    if [ "$CI" = 1 ]; then
        SLUG="${GITHUB_REPOSITORY:-$(gh repo view --json nameWithOwner --jq .nameWithOwner)}"
        TITLE="Upstream Compact toolchain $LATEST released"

        if gh issue list -R "$SLUG" --state open --search "$TITLE" \
            --json title --jq '.[].title' 2>/dev/null | grep -Fxq "$TITLE"; then
            echo "An open issue already exists; not filing a duplicate."
        else
            body="$(cat <<EOF
A newer Compact toolchain release was detected upstream.

- Latest upstream: **$LATEST** (\`$LATEST_TAG\`)
- Currently tracked: **$KNOWN**
- Release: https://github.com/$UPSTREAM_REPO/releases/tag/$LATEST_TAG
- Release notes: https://docs.midnight.network/relnotes/compact

### Checklist

- [ ] Read the release notes / changelog.
- [ ] Check the support matrix for the new public-network (mainnet) pin:
      https://docs.midnight.network/relnotes/support-matrix.md
- [ ] Update \`UPSTREAM_RELEASES.md\` and \`.github/upstream-known.txt\`.
- [ ] Update registries (\`stdlib.toml\`, \`builtins.toml\`) and \`min_language_version\` gates if the
      language version moved.
- [ ] If syntax moved, update the \`compact-tree-sitter\` pin first.
- [ ] Re-run the probe tests against the new toolchain (\`compiler-probe\` job or locally).
- [ ] Land a PR and close this issue.

_Filed automatically by \`.github/workflows/upstream-watch.yml\`._
EOF
)"
            gh issue create -R "$SLUG" --title "$TITLE" --body "$body"
            echo "Filed issue: $TITLE"
        fi
    fi
else
    echo "Up to date: no newer toolchain than $KNOWN."
fi
