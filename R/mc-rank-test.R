#' Monte Carlo rank test
#'
#' Computes a rank-based Monte Carlo p-value with the standard +1 correction.
#' The helper is intended for comparing a scalar observed statistic with the
#' corresponding statistics from simulated trajectories.
#'
#' @param observed Numeric scalar observed statistic.
#' @param simulated Numeric vector of simulated statistics.
#' @param alternative Character. One of `"two.sided"`, `"less"`, or
#'   `"greater"`. For `"greater"`, small p-values indicate that `observed` is
#'   unusually large relative to `simulated`. For `"less"`, small p-values
#'   indicate that `observed` is unusually small.
#'
#' @return A list with `observed`, `simulated`, `rank`, `p_value`, and
#'   `alternative`.
#' @export
#'
#' @references
#' North, B. V., Curtis, D., and Sham, P. C. (2002). A note on the calculation
#' of empirical P values from Monte Carlo procedures. *American Journal of
#' Human Genetics*, 71(2), 439-441. <https://doi.org/10.1086/341527>
#'
#' @examples
#' mc_rank_test(5, c(1, 2, 3, 4), alternative = "greater")
mc_rank_test <- function(observed, simulated, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  if (!is.numeric(observed) || length(observed) != 1 || !is.finite(observed)) {
    stop("`observed` must be one finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(simulated)) {
    stop("`simulated` must be numeric.", call. = FALSE)
  }

  simulated <- simulated[is.finite(simulated)]
  if (length(simulated) < 1) {
    stop("`simulated` must contain at least one finite value.", call. = FALSE)
  }

  k <- length(simulated)
  p_less <- (sum(simulated <= observed) + 1) / (k + 1)
  p_greater <- (sum(simulated >= observed) + 1) / (k + 1)

  p_value <- switch(
    alternative,
    less = p_less,
    greater = p_greater,
    two.sided = min(1, 2 * min(p_less, p_greater))
  )

  out <- list(
    observed = observed,
    simulated = simulated,
    rank = sum(simulated <= observed) + 1,
    p_value = p_value,
    alternative = alternative
  )
  class(out) <- c("gmov_mc_rank_test", "list")
  out
}
