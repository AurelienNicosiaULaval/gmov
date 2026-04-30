#' Red deer generative validation example
#'
#' A compact red deer example derived from the empirical iSSA workflow in
#' Nicosia (2026). The object contains an observed red deer trajectory, 99
#' simulated trajectories generated from the fitted iSSA workflow used in the
#' article, and the article scorecard for the empirical case study.
#'
#' The observed trajectory is based on the `deer` data distributed with `amt`.
#' It is included here only to make the package vignette render with visible
#' results even when users do not re-run the full `amt` fitting and simulation
#' workflow.
#'
#' @format A list with five components:
#' \describe{
#'   \item{observed_track}{A data frame with `x_`, `y_`, and `t_` columns.}
#'   \item{simulated_tracks}{A list of 99 simulated track data frames with
#'     `x_` and `y_` columns.}
#'   \item{article_scorecard}{A data frame containing the empirical scorecard
#'     reported by the article workflow.}
#'   \item{n_sims}{The number of simulated trajectories.}
#'   \item{source}{A short provenance note.}
#' }
#'
#' @references
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' Signer, J., Fieberg, J., and Avgar, T. (2019). Animal movement tools (`amt`):
#' R package for managing tracking data and conducting habitat selection
#' analyses. *Ecology and Evolution*, 9, 880-890.
#' <https://doi.org/10.1002/ece3.4823>
"red_deer_gmov"
