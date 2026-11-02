#!/usr/bin/env bash
# Build in an isolated directory without changing source repository Git state.
set -euo pipefail

FINANCIAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEVELOPMENT_REF="$(awk '$1 == "antitrust" {print $2}' "$FINANCIAL_DIR/ci/DEVELOPMENT_REFS")"
RELEASE_CERTIFICATION="${RELEASE_CERTIFICATION:-false}"
if [[ "$RELEASE_CERTIFICATION" == true || "$RELEASE_CERTIFICATION" == 1 ]]; then
  ANTITRUST_REQUESTED_REF="${ANTITRUST_RELEASE_SHA:-}"
  if [[ ! "$ANTITRUST_REQUESTED_REF" =~ ^[[:xdigit:]]{40}$ ]]; then
    echo "Release certification requires ANTITRUST_RELEASE_SHA as an explicit full 40-character SHA; mutable refs such as main/refactor are rejected." >&2
    exit 1
  fi
else
  ANTITRUST_REQUESTED_REF="${ANTITRUST_REF:-$DEVELOPMENT_REF}"
fi

ANTITRUST_SRC="${1:-${ANTITRUST_SRC:-https://github.com/luciu5/antitrust.git}}"
BUILD_ROOT="${FINANCIAL_OUTPUT_DIR:-$(mktemp -d /tmp/financial-release-check-XXXXXX)}"
mkdir -p "$BUILD_ROOT" "$BUILD_ROOT/lib" "$BUILD_ROOT/antitrust"
ANTITRUST_CLONED="false"

if [[ ! -d "$ANTITRUST_SRC" ]]; then
  git clone --quiet --no-checkout "$ANTITRUST_SRC" "$BUILD_ROOT/dependency-repo"
  ANTITRUST_SRC="$BUILD_ROOT/dependency-repo"
  git -C "$ANTITRUST_SRC" fetch --quiet origin "$ANTITRUST_REQUESTED_REF"
  git -C "$ANTITRUST_SRC" checkout --quiet --detach FETCH_HEAD
  ANTITRUST_CLONED="true"
fi

if [[ "$RELEASE_CERTIFICATION" == true || "$RELEASE_CERTIFICATION" == 1 || "$ANTITRUST_CLONED" == true ]]; then
  RESOLVED_SHA="$(git -C "$ANTITRUST_SRC" rev-parse --verify HEAD)"
else
  RESOLVED_SHA=""
  for candidate in \
    "$ANTITRUST_REQUESTED_REF" \
    "refs/remotes/origin/${ANTITRUST_REQUESTED_REF#refs/heads/}" \
    "refs/heads/${ANTITRUST_REQUESTED_REF#refs/heads/}" \
    "refs/tags/${ANTITRUST_REQUESTED_REF#refs/tags/}"; do
    if RESOLVED_SHA="$(git -C "$ANTITRUST_SRC" rev-parse --verify --end-of-options "${candidate}^{commit}" 2>/dev/null)"; then
      break
    fi
    RESOLVED_SHA=""
  done
  if [[ -z "$RESOLVED_SHA" ]]; then
    echo "Could not resolve antitrust development ref '$ANTITRUST_REQUESTED_REF' in local repository $ANTITRUST_SRC" >&2
    exit 1
  fi
fi
if [[ ( "$RELEASE_CERTIFICATION" == true || "$RELEASE_CERTIFICATION" == 1 ) && "${RESOLVED_SHA,,}" != "${ANTITRUST_REQUESTED_REF,,}" ]]; then
  echo "Checked-out antitrust SHA $RESOLVED_SHA does not match requested release candidate $ANTITRUST_REQUESTED_REF" >&2
  exit 1
fi
git -C "$ANTITRUST_SRC" archive "$RESOLVED_SHA" | tar -x -C "$BUILD_ROOT/antitrust"

export R_LIBS="$BUILD_ROOT/lib:$(Rscript --vanilla -e 'cat(paste(.libPaths(), collapse=":"))')"
{
  echo "release_certification=$RELEASE_CERTIFICATION"
  echo "antitrust_requested_ref=$ANTITRUST_REQUESTED_REF"
  echo "antitrust_resolved_sha=$RESOLVED_SHA"
  echo "financial_source=$FINANCIAL_DIR"
  echo "financial_extended=${FINANCIAL_EXTENDED_TESTS:-0}"
  Rscript --vanilla -e 'cat(R.version.string,"\n")'
  if git -C "$FINANCIAL_DIR" rev-parse HEAD >/dev/null 2>&1; then
    echo "financial_head=$(git -C "$FINANCIAL_DIR" rev-parse HEAD)"
    git -C "$FINANCIAL_DIR" diff --binary HEAD | sha256sum | sed 's/^/financial_worktree_diff_sha256=/'
  fi
} > "$BUILD_ROOT/provenance.txt"

R CMD INSTALL --library="$BUILD_ROOT/lib" "$BUILD_ROOT/antitrust"
Rscript --vanilla - "$BUILD_ROOT/antitrust" "$RELEASE_CERTIFICATION" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
expected <- unname(read.dcf(file.path(args[[1L]], "DESCRIPTION"))[1L, "Version"])
actual <- unname(as.character(utils::packageVersion("antitrust")))
stopifnot(identical(actual, expected),
          all(c("StructuralFit", "Antitrust", "CounterfactualPath") %in%
              methods::getClasses(where = asNamespace("antitrust"))))
if (args[[2L]] %in% c("true", "1")) {
  stopifnot(utils::packageVersion("antitrust") >= "0.99.43")
}
cat("antitrust installed version:", actual, "\n")
RSCRIPT

cd "$BUILD_ROOT"
R CMD build --no-manual --no-resave-data "$FINANCIAL_DIR"
sha256sum financial_*.tar.gz >> "$BUILD_ROOT/provenance.txt"
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual financial_*.tar.gz
echo "Evidence: $BUILD_ROOT"
