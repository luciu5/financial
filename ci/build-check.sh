#!/usr/bin/env bash
# Dependency-stack CI for `financial` (Phase 9).
#
# Certifies `financial` against a PINNED antitrust SHA, not a mutable branch.
# Record of exact source SHAs is in ci/PINNED_SHAS. This script:
#   1. installs the pinned antitrust commit into an isolated library
#   2. builds and R CMD checks `financial` against it
#
# Usage: ci/build-check.sh [ANTITRUST_REPO_URL_OR_PATH]
# Env overrides: ANTITRUST_SHA, FINANCIAL_LIB
set -euo pipefail

FINANCIAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ANTITRUST_SHA="${ANTITRUST_SHA:-$(grep '^antitrust ' "$FINANCIAL_DIR/ci/PINNED_SHAS" | awk '{print $2}')}"
ANTITRUST_SRC="${1:-${ANTITRUST_SRC:-$HOME/Projects/ioverse/antitrust}}"
FINANCIAL_LIB="${FINANCIAL_LIB:-$FINANCIAL_DIR/.Rlib}"

echo "== financial dependency-stack CI =="
echo "financial dir : $FINANCIAL_DIR"
echo "antitrust src : $ANTITRUST_SRC"
echo "antitrust SHA : $ANTITRUST_SHA (pinned)"
echo "target lib    : $FINANCIAL_LIB"

mkdir -p "$FINANCIAL_LIB"

# --- 1. Install the pinned antitrust commit into an isolated worktree/library ---
WORKTREE="$(mktemp -d)/antitrust-pinned"
git -C "$ANTITRUST_SRC" worktree add -f --detach "$WORKTREE" "$ANTITRUST_SHA"
trap 'git -C "$ANTITRUST_SRC" worktree remove --force "$WORKTREE" 2>/dev/null || true' EXIT

BUILT_SHA="$(git -C "$WORKTREE" rev-parse HEAD)"
if [ "$BUILT_SHA" != "$ANTITRUST_SHA" ] && [ "${ANTITRUST_SHA}" != "${BUILT_SHA:0:${#ANTITRUST_SHA}}" ]; then
  echo "FATAL: checked-out antitrust SHA $BUILT_SHA != pinned $ANTITRUST_SHA" >&2
  exit 1
fi

R CMD INSTALL --no-multiarch --with-keep.source -l "$FINANCIAL_LIB" "$WORKTREE"

# Confirm the pinned build actually exposes the contract surface financial needs.
R_LIBS="$FINANCIAL_LIB:$(Rscript -e 'cat(paste(.libPaths(), collapse=":"))')" \
  Rscript -e '
    suppressMessages(library(antitrust))
    stopifnot(as.character(packageVersion("antitrust")) >= "0.99.38")
    need_cls <- c("StructuralFit","AntitrustFit","CounterfactualPath","Antitrust")
    stopifnot(all(need_cls %in% getClasses("package:antitrust")))
    need_fn <- c("simulate","simulate_steps","respecify","HHI","ownerToMatrix","ownerToVec")
    stopifnot(all(need_fn %in% getNamespaceExports("antitrust")))
    cat("antitrust contract surface OK (", as.character(packageVersion("antitrust")), ")\n")
  '

# --- 2. Build + check financial against the pinned antitrust ---
export R_LIBS="$FINANCIAL_LIB:$(Rscript -e 'cat(paste(.libPaths(), collapse=":"))')"
cd "$FINANCIAL_DIR"
R CMD build . --no-build-vignettes --no-manual
TARBALL="$(ls -t financial_*.tar.gz | head -1)"
echo "built: $TARBALL"

# _R_CHECK_FORCE_SUGGESTS_=false: testthat is the only Suggests we require; posterior/cmdstanr are optional.
_R_CHECK_FORCE_SUGGESTS_=false R CMD check "$TARBALL" --no-manual --no-vignettes --no-build-vignettes

echo "== CI PASSED against pinned antitrust $ANTITRUST_SHA =="
