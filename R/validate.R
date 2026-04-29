#' Validate generative behavior of fitted SSF or iSSF models
#'
#' Runs a set of trajectory-level generative validation diagnostics on an
#' observed track and simulated tracks. The fitted model is not required by this
#' function: users fit and simulate from SSF or iSSF models with tools such as
#' `amt`, then pass the observed and simulated tracks to gmov.
#'
#' @param observed Observed track-like object compatible with [as_gmov_track()].
#' @param simulated A list of simulated track-like objects, or a data frame with
#'   one row per simulated location and a simulation identifier column.
#' @param metrics Character vector of metrics to compute. Supported values are
#'   `"ud"`, `"msd"`, `"sinuosity"`, and `"barrier"`.
#' @param barrier Optional `sf` LINESTRING or MULTILINESTRING object used for
#'   barrier crossing validation. It is assumed to use the same planar
#'   coordinate system as the tracks.
#' @param ud_args,msd_args,sinuosity_args,barrier_args Lists of additional
#'   arguments passed to the corresponding metric functions.
#' @param ... Passed to coercion helpers.
#'
#' @return An object of class `c("gmov_generative", "list")` containing the
#'   function call, track summaries, metric names, metric results, and settings.
#' @export
#'
#' @references
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' @examples
#' observed <- data.frame(x = cumsum(c(0, 1, 1, 0)), y = c(0, 0, 1, 1))
#' simulated <- list(
#'   data.frame(x = cumsum(c(0, 1, 1, 1)), y = c(0, 0, 0, 0)),
#'   data.frame(x = cumsum(c(0, 0, 1, 1)), y = c(0, 1, 1, 1))
#' )
#'
#' res <- validate_ssf_generative(
#'   observed = observed,
#'   simulated = simulated,
#'   metrics = c("msd", "sinuosity")
#' )
#' summary(res)
validate_ssf_generative <- function(
  observed,
  simulated,
  metrics = c("ud", "msd", "sinuosity", "barrier"),
  barrier = NULL,
  ud_args = list(),
  msd_args = list(),
  sinuosity_args = list(),
  barrier_args = list(),
  ...
) {
  call <- match.call()
  metrics <- validate_metric_names(metrics)

  if ("barrier" %in% metrics && is.null(barrier)) {
    warning("Skipping `barrier` because `barrier = NULL`.", call. = FALSE)
    metrics <- setdiff(metrics, "barrier")
  }

  if (length(metrics) < 1L) {
    stop("No validation metrics remain after checking inputs.", call. = FALSE)
  }

  obs <- as_gmov_track(observed, ...)
  sims <- as_gmov_simulations(simulated, ...)

  metric_results <- list()
  if ("ud" %in% metrics) {
    metric_results$ud <- do.call(
      validate_ud,
      c(list(observed = obs, simulated = sims), ud_args)
    )
  }
  if ("msd" %in% metrics) {
    metric_results$msd <- do.call(
      validate_msd,
      c(list(observed = obs, simulated = sims), msd_args)
    )
  }
  if ("sinuosity" %in% metrics) {
    metric_results$sinuosity <- do.call(
      validate_sinuosity,
      c(list(observed = obs, simulated = sims), sinuosity_args)
    )
  }
  if ("barrier" %in% metrics) {
    metric_results$barrier <- do.call(
      validate_barrier_crossing,
      c(list(observed = obs, simulated = sims, barrier = barrier), barrier_args)
    )
  }

  out <- list(
    call = call,
    observed_summary = summarize_track(obs),
    simulated_summary = summarize_simulations(sims),
    metrics = names(metric_results),
    metric_results = metric_results,
    settings = list(
      ud_args = ud_args,
      msd_args = msd_args,
      sinuosity_args = sinuosity_args,
      barrier_args = barrier_args
    )
  )
  class(out) <- c("gmov_generative", "list")
  out
}

validate_metric_names <- function(metrics) {
  valid <- c("ud", "msd", "sinuosity", "barrier")
  metrics <- unique(tolower(metrics))
  unknown <- setdiff(metrics, valid)

  if (length(unknown) > 0) {
    stop("Unknown metric(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  metrics
}
