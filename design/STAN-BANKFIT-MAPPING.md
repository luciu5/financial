# Stan posterior -> BankFit ingestion contract

Derived from stability@cc95e2e0. `financial` consumes Stan output; it does NOT run Stan.

## Minimal sufficient parameter set (authoritative)

From `merger_required_draw_variables()` (stability code/lib/merger_common.R:21-32) — 15 params:
```
alpha_bar_L, alpha_bar_D, sigma_alpha_state_L, sigma_alpha_state_D,
alpha_state_raw_L[1..R_state], alpha_state_raw_D[1..R_state],
theta_out_L[1..K_out_L], theta_out_D[1..K_out_D], alpha_out_L, alpha_out_D,
log_phi_bar_L, log_phi_bar_D, eta_L[1..K_cost], eta_D[1..K_cost], gamma_psi[1..K_psi]
```
By role: demand price coefs (alpha_bar/sigma/state_raw -> per-market alpha_L/alpha_D);
Beta participation (theta_out/alpha_out -> outside_mu; log_phi_bar -> phi_out precision);
cost shifters (eta_L/eta_D -> cost via W matrices); liquidity friction (gamma_psi -> psi via Z_psi).

NOT needed: `beta_L`/`beta_D` (solver uses observed shares, not re-predicted utility), `sigma_err` (likelihood-only).

## Per-draw unit = named numeric vector
The unit flowing into the deterministic solver is a NAMED NUMERIC VECTOR (names `alpha_bar_L`, `gamma_psi[1]`, `eta_L[2]`) — identical shape to a point estimate. So point-estimate and single-draw are the SAME code path (stan_io.R stan_param_scalar/vector). This makes "degenerate 1-draw posterior == point estimate" a natural parity test.

## Two-argument contract: bank_fit(parameters=, data=)
Parameters (draws) are NOT sufficient. The solver also needs DATA not in the posterior, mirroring legacy load_merger_inputs (merger_common.R:5-19):
- to reconstruct derive_stan_primitives (stan_primitives.R:3-41): H_out_L/D, out_iv_L/D, rho_out_L/D, market_state_region_id
- to drive the solver: Z_psi, W_L, W_D, market_size_L/D, baseline s_cond_L/D, r_f, equity, liquid_assets, liquidity_pressure, group_id, bank/market ids
- calibrated residuals omega_L/omega_D are derived per-draw by the R solver from data+params (NOT Stan params).

## Accepted input shapes (no cmdstanr/posterior dependency)
1. named numeric vector (point estimate / single draw)
2. plain matrix or data.frame N_draws x N_params, columns = canonical names. MUST accept both `name[i]` (bracket) and `name.i` (dot) vector-element conventions on ingestion (stan_draw_column_matches, merger_common.R:34-43).
3. OPTIONAL (Suggests only): posterior::draws_df / cmdstanr CmdStanMCMC -> coerce internally via as_draws_df, drop .chain/.iteration/.draw.

## Aggregation recipe (gather_merger_results.R:119-188)
Per output quantity: mean, sd, q05, q50, q95. For any scenario-difference quantity (Liquidity+Competition minus Competition-only): additionally posterior_prob_positive = mean(x>0) and posterior_odds_positive. Tag n_draws.

## UNKNOWNs to design around
- How a fresh BankFit caller supplies Z_psi rows without the original G-group panel structure (simulate_merger_exposure.R:669) — needs package-level doc/validation.
- Whether dims K_psi/R_state/K_out/K_cost are inferred from column names or passed explicitly (legacy always had stan_data to supply them).
- dot-form column convention may be legacy CSV cruft vs needed.
