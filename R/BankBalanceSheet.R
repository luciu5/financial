#' Bank balance-sheet state: liquidity friction `psi` and Stan-posterior ingestion
#'
#' Implements the `derive_stan_primitives`-equivalent transform
#' (`design/ARCHITECTURE.md`'s "the Stan-posterior ingestion requirement in
#' the Bank module bullets"; `design/STAN-BANKFIT-MAPPING.md`) on the R side,
#' plus the row-level liquidity-friction shifter `psi` (the endogenous
#' marginal-cost shifter's friction parameter, priced by `BankRisk.R`'s
#' `calc_portfolio_liquidity()`). No Stan estimation and no DB/FRED/QCEW
#' ingestion are migrated -- these functions consume ALREADY-FITTED Stan
#' parameter draws (a named numeric vector, or a matrix/data.frame of draws)
#' plus caller-supplied structural data, per the two-argument
#' `bank_fit(parameters =, data =)` contract implemented in
#' `BankCounterfactual.R`.
#'
#' @section Simplifications for a fresh caller (no original Stan/G-group panel):
#' `design/STAN-BANKFIT-MAPPING.md` flags an UNKNOWN: "How a fresh `BankFit`
#' caller supplies `Z_psi` rows without the original G-group panel structure."
#' The resolution adopted here (see the migration report for the full
#' rationale) is to let the caller define their OWN grouping directly via a
#' row-level `group_id` (defaulting to `bank_id` -- i.e. "one liquidity-
#' friction group per bank", the natural degenerate case of the original
#' Stan `G`-group structure) and a `Z_psi` design matrix with one row per
#' distinct group, in `sort(unique(group_id))` order. This is a strict
#' generalization of the original contract (the original G-group structure
#' is recovered exactly when the caller's `group_id` happens to equal the
#' Stan panel's `group_id`), not a narrowing of it.
#'
#' The same simplification is applied, for the same reason, to the
#' outside-share design data: legacy `stability` deduplicates outside-share
#' observations onto a separate `out_id` index (`H_out_L`/`H_out_D` are
#' `N_out x K_out_{L,D}`, `N_out < N`) because its Stan panel spans multiple
#' bank-market ROWS sharing one outside-share observation. A fresh `BankFit`
#' caller has no such panel to deduplicate against, so `derive_bank_primitives()`
#' accepts `H_out_L`/`H_out_D`/`out_iv_L`/`out_iv_D`/`market_state_region_id`
#' at ROW granularity (length `N`, one entry per bank-market row) instead of
#' the deduplicated `out_id`/`market`-level granularity. Supplying the same
#' design row for every row that shares an outside-share observation
#' reproduces the legacy economics exactly; this is a superset of the
#' legacy contract, not a behavioral change to the transform itself (the
#' `plogis(alpha_out + H \%*\% theta_out - rho_out * out_iv)` formula is
#' applied verbatim, row-by-row, exactly as `stan_primitives.R`'s
#' `derive_stan_primitives()` applies it index-by-index).
#'
#' @name Bank-balance-sheet
#' @keywords internal
#' @include BankClasses.R
NULL

## ---------------------------------------------------------------------
## Stan posterior parameter extraction: scalar/vector accessors supporting
## both `name[i]` (bracket) and `name.i` (dot) column conventions, per
## design/STAN-BANKFIT-MAPPING.md point 2 ("MUST accept both conventions").
## Migrated from stability/code/lib/stan_io.R's stan_param_scalar/
## stan_param_vector (SHA cc95e2e0), generalized to operate on a single
## named-numeric-vector "draw" (point estimate and single draw are the SAME
## code path, per the mapping doc).
## ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
bank_param_scalar <- function(params, name) {
  if (!(name %in% names(params))) {
    stop("Missing bank parameter: ", name)
  }
  as.numeric(params[[name]])
}

#' @keywords internal
#' @noRd
bank_param_vector <- function(params, prefix, n) {
  names_bracket <- paste0(prefix, "[", seq_len(n), "]")
  names_dot <- paste0(prefix, ".", seq_len(n))

  if (all(names_bracket %in% names(params))) {
    out <- as.numeric(params[names_bracket])
  } else if (all(names_dot %in% names(params))) {
    out <- as.numeric(params[names_dot])
  } else if (n == 1L && prefix %in% names(params)) {
    out <- as.numeric(params[[prefix]])
  } else {
    stop(
      "Missing bank parameter vector '", prefix, "' of length ", n,
      " (looked for '", prefix, "[1..", n, "]' and '", prefix, ".1..", n, "')"
    )
  }

  if (length(out) != n || any(!is.finite(out))) {
    stop("Parameter '", prefix, "' is not a finite vector of length ", n)
  }
  out
}

## ---------------------------------------------------------------------
## derive_bank_primitives(): the derive_stan_primitives-equivalent transform,
## migrated from stability/code/lib/stan_primitives.R (SHA cc95e2e0), operating
## at ROW granularity (see the "Simplifications for a fresh caller" note
## above) instead of the legacy out_id/market-region-index granularity.
## ---------------------------------------------------------------------

#' Derive bank Stan-posterior primitives (row-level)
#'
#' R-side equivalent of `stability`'s `derive_stan_primitives()`
#' (`code/lib/stan_primitives.R`, SHA `cc95e2e0`): maps a single Stan
#' parameter draw (named numeric vector) plus caller-supplied structural
#' design data to the row-level demand/participation primitives the bank
#' Nash-in-rates solver needs -- the price coefficients `alpha_L`/`alpha_D`
#' and the Beta-participation primitives `outside_mu_L`/`outside_mu_D`/
#' `phi_out_L`/`phi_out_D`.
#'
#' @param params A named numeric vector: one posterior draw or a point
#'   estimate (see `design/STAN-BANKFIT-MAPPING.md`).
#' @param data A list with the row-level design data: `H_out_L`, `H_out_D`
#'   (`N x K_out_L`/`N x K_out_D` matrices), `out_iv_L`, `out_iv_D` (length
#'   `N`), `market_state_region_id` (length `N`, integer region index into
#'   `alpha_state_raw_{L,D}`), and optionally `rho_out_L`, `rho_out_D`
#'   (default `1`, matching the legacy default).
#' @return A list with `alpha_L`, `alpha_D`, `outside_mu_L`, `outside_mu_D`,
#'   `phi_out_L`, `phi_out_D` -- all length `N` (row-level).
#' @keywords internal
#' @noRd
derive_bank_primitives <- function(params, data) {
  n <- length(data$market_state_region_id)
  rho_out_L <- data$rho_out_L %||% 1
  rho_out_D <- data$rho_out_D %||% 1

  K_out_L <- ncol(data$H_out_L)
  K_out_D <- ncol(data$H_out_D)
  theta_out_L <- bank_param_vector(params, "theta_out_L", K_out_L)
  theta_out_D <- bank_param_vector(params, "theta_out_D", K_out_D)

  alpha_out_L <- bank_param_scalar(params, "alpha_out_L")
  alpha_out_D <- bank_param_scalar(params, "alpha_out_D")
  outside_eta_L <- alpha_out_L +
    as.numeric(data$H_out_L %*% theta_out_L) -
    rho_out_L * data$out_iv_L
  outside_eta_D <- alpha_out_D +
    as.numeric(data$H_out_D %*% theta_out_D) -
    rho_out_D * data$out_iv_D

  R_state <- max(data$market_state_region_id)
  alpha_bar_L <- bank_param_scalar(params, "alpha_bar_L")
  alpha_bar_D <- bank_param_scalar(params, "alpha_bar_D")
  sigma_alpha_state_L <- bank_param_scalar(params, "sigma_alpha_state_L")
  sigma_alpha_state_D <- bank_param_scalar(params, "sigma_alpha_state_D")
  alpha_state_raw_L <- bank_param_vector(params, "alpha_state_raw_L", R_state)
  alpha_state_raw_D <- bank_param_vector(params, "alpha_state_raw_D", R_state)

  alpha_logabs_L <- alpha_bar_L +
    sigma_alpha_state_L * alpha_state_raw_L[data$market_state_region_id]
  alpha_logabs_D <- alpha_bar_D +
    sigma_alpha_state_D * alpha_state_raw_D[data$market_state_region_id]
  alpha_L <- -exp(alpha_logabs_L)
  alpha_D <- exp(alpha_logabs_D)

  log_phi_bar_L <- bank_param_scalar(params, "log_phi_bar_L")
  log_phi_bar_D <- bank_param_scalar(params, "log_phi_bar_D")

  list(
    outside_mu_L = rep_len(pmin(pmax(stats::plogis(outside_eta_L), 1e-9), 1 - 1e-9), n),
    outside_mu_D = rep_len(pmin(pmax(stats::plogis(outside_eta_D), 1e-9), 1 - 1e-9), n),
    phi_out_L = rep(exp(pmin(pmax(log_phi_bar_L, log(1e-3)), log(1e5))), n),
    phi_out_D = rep(exp(pmin(pmax(log_phi_bar_D, log(1e-3)), log(1e5))), n),
    alpha_L = rep_len(pmin(pmax(alpha_L, -1000), -1e-6), n),
    alpha_D = rep_len(pmin(pmax(alpha_D, 1e-6), 1000), n)
  )
}

## ---------------------------------------------------------------------
## calc_bank_psi(): the liquidity-friction shifter, migrated verbatim from
## simulate_merger_exposure.R's inline psi computation (lines 663-674):
##   psi_index <- sum(Z_psi[group_row, ] * gamma_psi)
##   psi_index <- 20 * tanh(psi_index / 20)  # Stan's smooth cap
##   psi <- exp(psi_index)
## ---------------------------------------------------------------------

#' Compute the per-group liquidity-friction parameter `psi`
#'
#' `psi = exp(20 * tanh((Z_psi \%*\% gamma_psi) / 20))` -- the smooth-capped
#' exponential link `stability`'s Stan model uses for the liquidity-friction
#' parameter that scales the convex liquidity-shortfall penalty (see
#' `BankRisk.R`'s `calc_portfolio_liquidity()`). Migrated verbatim from
#' `simulate_merger_exposure.R` (SHA `cc95e2e0`, lines 663-674).
#'
#' @param Z_psi A `G x K_psi` design matrix, one row per liquidity-friction
#'   group.
#' @param gamma_psi A length `K_psi` numeric vector (a Stan parameter draw
#'   component).
#' @return A length `G` numeric vector of `psi` values, one per group (row
#'   of `Z_psi`).
#' @keywords internal
#' @noRd
calc_bank_psi <- function(Z_psi, gamma_psi) {
  psi_index <- as.numeric(Z_psi %*% gamma_psi)
  psi_index <- 20 * tanh(psi_index / 20)
  exp(psi_index)
}

## ---------------------------------------------------------------------
## Posterior-draw ingestion: accepted input shapes per
## design/STAN-BANKFIT-MAPPING.md point "Accepted input shapes".
## ---------------------------------------------------------------------

#' Normalize a `bank_fit(parameters = ...)` argument to a draws matrix
#'
#' Accepts, per `design/STAN-BANKFIT-MAPPING.md`:
#' \enumerate{
#'   \item a named numeric vector (point estimate / single draw);
#'   \item a plain matrix or data.frame, `N_draws x N_params`, with columns
#'     named using either the `name[i]` (bracket) or `name.i` (dot)
#'     vector-element convention;
#'   \item (optional, `Suggests`-only convenience) a `posterior::draws_df`
#'     or a `cmdstanr::CmdStanMCMC` fit object, coerced internally via
#'     `posterior::as_draws_df()` with `.chain`/`.iteration`/`.draw` columns
#'     dropped. Neither `posterior` nor `cmdstanr` is a hard dependency;
#'     this path is skipped (with an informative error) if `posterior` is
#'     not installed.
#' }
#'
#' @param parameters The `bank_fit(parameters = ...)` argument.
#' @return A numeric matrix, `N_draws x N_params`, with `N_draws >= 1`.
#' @keywords internal
#' @noRd
normalize_bank_draws <- function(parameters) {
  if (is.numeric(parameters) && is.null(dim(parameters))) {
    if (is.null(names(parameters)) || any(!nzchar(names(parameters)))) {
      stop("A vector 'parameters' argument to bank_fit() must be a NAMED numeric vector.")
    }
    return(matrix(parameters, nrow = 1, dimnames = list(NULL, names(parameters))))
  }

  if (is.matrix(parameters)) {
    if (is.null(colnames(parameters))) {
      stop("A matrix 'parameters' argument to bank_fit() must have column names.")
    }
    storage.mode(parameters) <- "double"
    return(parameters)
  }

  if (is.data.frame(parameters)) {
    return(as.matrix(parameters))
  }

  if (inherits(parameters, "CmdStanMCMC") || inherits(parameters, "CmdStanMLE")) {
    if (!requireNamespace("posterior", quietly = TRUE)) {
      stop("Package 'posterior' is required to ingest a CmdStanR fit object directly; ",
        "install 'posterior' or pass a plain matrix/data.frame of draws instead.")
    }
    draws_df <- posterior::as_draws_df(parameters$draws())
    draws_df <- as.data.frame(draws_df, check.names = FALSE)
    draws_df <- draws_df[, !names(draws_df) %in% c(".chain", ".iteration", ".draw"), drop = FALSE]
    return(as.matrix(draws_df))
  }

  if (inherits(parameters, "draws_array") || inherits(parameters, "draws_df") || inherits(parameters, "draws_matrix")) {
    if (!requireNamespace("posterior", quietly = TRUE)) {
      stop("Package 'posterior' is required to ingest a 'posterior::draws' object directly; ",
        "install 'posterior' or pass a plain matrix/data.frame of draws instead.")
    }
    draws_df <- posterior::as_draws_df(parameters)
    draws_df <- as.data.frame(draws_df, check.names = FALSE)
    draws_df <- draws_df[, !names(draws_df) %in% c(".chain", ".iteration", ".draw"), drop = FALSE]
    return(as.matrix(draws_df))
  }

  stop(
    "Unsupported 'parameters' argument to bank_fit(): must be a named numeric ",
    "vector, a matrix/data.frame of draws, or (optionally) a posterior::draws ",
    "object / cmdstanr fit. Got class: ", paste(class(parameters), collapse = "/")
  )
}

#' Extract a named-vector "draw" from a draws matrix (bracket/dot both supported)
#'
#' @param draws_mat A draws matrix as returned by `normalize_bank_draws()` (internal).
#' @param i A row index.
#' @return A named numeric vector (a single draw).
#' @keywords internal
#' @noRd
bank_draw_row <- function(draws_mat, i) {
  row <- draws_mat[i, ]
  stats::setNames(as.numeric(row), colnames(draws_mat))
}
