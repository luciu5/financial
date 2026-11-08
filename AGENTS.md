# Working in financial

Use `main`, the repository default branch, for routine work. The `refactor` migration is integrated; do not recreate that branch. Start any necessary isolated task branch from `main`.

Preserve uncommitted local changes and unrelated branches or worktrees.

Preserve the package economic definitions and numerical validity checks when changing debt or pricing calculations.

Test economic behavior and cross-package integration where relevant. Run proportionate checks and report what actually ran; distinguish ordinary checks from extended statistical validation.
