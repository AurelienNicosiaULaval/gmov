`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

track_coords <- function(x) {
  trk <- as_gmov_track(x)
  as.matrix(trk[, c("x_", "y_"), drop = FALSE])
}

summarize_track <- function(x) {
  trk <- as_gmov_track(x)
  tibble::tibble(
    n_locations = nrow(trk),
    x_min = min(trk$x_),
    x_max = max(trk$x_),
    y_min = min(trk$y_),
    y_max = max(trk$y_),
    straightness = straightness_index(trk)
  )
}

summarize_simulations <- function(sims) {
  n_locations <- vapply(sims, nrow, integer(1))
  tibble::tibble(
    n_simulations = length(sims),
    min_locations = min(n_locations),
    median_locations = stats::median(n_locations),
    max_locations = max(n_locations)
  )
}

check_simulation_count <- function(sims, minimum = 2) {
  if (length(sims) < minimum) {
    stop(
      "At least ", minimum, " simulated tracks are required for this validation.",
      call. = FALSE
    )
  }
}

metric_p_value <- function(x) {
  if (is.null(x$rank_test)) {
    return(NA_real_)
  }
  x$rank_test$p_value
}
