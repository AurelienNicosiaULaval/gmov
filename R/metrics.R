straightness_index <- function(track) {
  trk <- as_gmov_track(track)
  dx <- diff(trk$x_)
  dy <- diff(trk$y_)
  path_length <- sum(sqrt(dx^2 + dy^2))

  if (!is.finite(path_length) || path_length <= 0) {
    return(NA_real_)
  }

  net_displacement <- sqrt(
    (trk$x_[nrow(trk)] - trk$x_[1])^2 +
      (trk$y_[nrow(trk)] - trk$y_[1])^2
  )

  net_displacement / path_length
}

msd_curve <- function(track, max_lag = NULL) {
  trk <- as_gmov_track(track)
  n <- nrow(trk)

  if (is.null(max_lag)) {
    max_lag <- n - 1L
  }
  max_lag <- min(as.integer(max_lag), n - 1L)

  if (!is.finite(max_lag) || max_lag < 1L) {
    stop("`max_lag` must be at least 1 and smaller than the track length.", call. = FALSE)
  }

  values <- vapply(seq_len(max_lag), function(lag_i) {
    dx <- trk$x_[(lag_i + 1L):n] - trk$x_[seq_len(n - lag_i)]
    dy <- trk$y_[(lag_i + 1L):n] - trk$y_[seq_len(n - lag_i)]
    mean(dx^2 + dy^2)
  }, numeric(1))

  tibble::tibble(lag = seq_len(max_lag), msd = values)
}

#' Validate emergent utilization distributions
#'
#' Compares observed and simulated emergent space use with empirical grid
#' utilization distributions and the 1-Wasserstein distance between those
#' discretized empirical distributions. This is not a kernel density estimate
#' and should not be interpreted as the exact Wasserstein distance between
#' continuous utilization distributions. The observed statistic is the mean
#' Wasserstein distance between the observed grid UD and each simulated grid UD.
#' Simulated reference statistics are leave-one-out mean
#' simulation-to-simulation Wasserstein distances.
#'
#' @param observed Observed track-like object.
#' @param simulated A list of simulated track-like objects or a data frame with
#'   a simulation identifier column.
#' @param grid_size Integer scalar or length-two integer vector giving the
#'   number of grid cells in the x and y directions.
#' @param bounds Optional named numeric vector with `xmin`, `xmax`, `ymin`, and
#'   `ymax`. If `NULL`, bounds are computed from observed and simulated tracks.
#'   Coordinate units should be meaningful Euclidean distance units.
#' @param ... Passed to [as_gmov_simulations()].
#'
#' @return A list with observed and simulated empirical grid UDs, Wasserstein
#'   distances between those grid UDs, discrepancy statistics, and a Monte Carlo
#'   rank test.
#' @export
#'
#' @references
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' @examples
#' observed <- data.frame(x = c(0, 1, 1), y = c(0, 0, 1))
#' simulated <- list(
#'   data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
#'   data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
#' )
#' validate_ud(observed, simulated, grid_size = 4)
validate_ud <- function(observed, simulated, grid_size = 50, bounds = NULL, ...) {
  obs <- as_gmov_track(observed)
  sims <- as_gmov_simulations(simulated, ...)
  check_simulation_count(sims, minimum = 2)

  grid_size <- normalize_grid_size(grid_size)
  bounds <- validate_bounds(bounds %||% combined_bounds(obs, sims))

  ud_obs <- empirical_grid_ud(obs, grid_size = grid_size, bounds = bounds)
  ud_sims <- lapply(sims, empirical_grid_ud, grid_size = grid_size, bounds = bounds)

  obs_distances <- vapply(
    ud_sims,
    function(ud_sim) wasserstein_ud(ud_obs, ud_sim),
    numeric(1)
  )

  n_sims <- length(ud_sims)
  distance_matrix <- matrix(0, nrow = n_sims, ncol = n_sims)
  for (i in seq_len(n_sims - 1L)) {
    for (j in seq.int(i + 1L, n_sims)) {
      distance_matrix[i, j] <- wasserstein_ud(ud_sims[[i]], ud_sims[[j]])
      distance_matrix[j, i] <- distance_matrix[i, j]
    }
  }

  sim_statistics <- vapply(seq_len(n_sims), function(i) {
    mean(distance_matrix[i, -i], na.rm = TRUE)
  }, numeric(1))
  observed_statistic <- mean(obs_distances, na.rm = TRUE)

  out <- list(
    metric = "ud",
    method = "empirical_grid_wasserstein",
    grid_size = grid_size,
    bounds = bounds,
    observed_ud = ud_obs,
    simulated_uds = ud_sims,
    observed_distances = tibble::tibble(
      sim_id = names(sims),
      distance = unname(obs_distances)
    ),
    simulated_statistics = tibble::tibble(
      sim_id = names(sims),
      statistic = unname(sim_statistics)
    ),
    statistic = observed_statistic,
    rank_test = mc_rank_test(observed_statistic, sim_statistics, alternative = "greater")
  )
  class(out) <- c("gmov_metric_ud", "list")
  out
}

#' Validate mean squared displacement
#'
#' Compares the observed mean squared displacement curve with the envelope and
#' expected curve from simulated trajectories. The discrepancy statistic is the
#' integrated squared error between the observed MSD curve and the mean
#' simulated MSD curve.
#'
#' @param observed Observed track-like object.
#' @param simulated A list of simulated track-like objects or a data frame with
#'   a simulation identifier column.
#' @param max_lag Optional maximum lag. If `NULL`, the largest common lag across
#'   observed and simulated tracks is used.
#' @param envelope_probs Numeric vector of length two giving lower and upper
#'   envelope quantiles.
#' @param ... Passed to [as_gmov_simulations()].
#'
#' @return A list with MSD curves, simulation envelope, discrepancy statistics,
#'   and a Monte Carlo rank test.
#' @export
#'
#' @references
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' @examples
#' observed <- data.frame(x = 0:4, y = 0)
#' simulated <- list(
#'   data.frame(x = 0:4, y = 0),
#'   data.frame(x = 0:4, y = c(0, 0, 1, 1, 1))
#' )
#' validate_msd(observed, simulated, max_lag = 2)
validate_msd <- function(
  observed,
  simulated,
  max_lag = NULL,
  envelope_probs = c(0.025, 0.975),
  ...
) {
  obs <- as_gmov_track(observed)
  sims <- as_gmov_simulations(simulated, ...)
  check_simulation_count(sims, minimum = 2)

  common_max_lag <- min(c(nrow(obs), vapply(sims, nrow, integer(1)))) - 1L
  max_lag <- validate_max_lag(max_lag %||% common_max_lag, common_max_lag = common_max_lag)

  envelope_probs <- validate_envelope_probs(envelope_probs)

  obs_curve <- msd_curve(obs, max_lag = max_lag)
  sim_curves <- lapply(sims, msd_curve, max_lag = max_lag)
  sim_matrix <- do.call(cbind, lapply(sim_curves, `[[`, "msd"))
  colnames(sim_matrix) <- names(sims)

  sim_mean <- rowMeans(sim_matrix)
  observed_statistic <- sum((obs_curve$msd - sim_mean)^2)
  sim_statistics <- vapply(seq_len(ncol(sim_matrix)), function(i) {
    reference_mean <- rowMeans(sim_matrix[, -i, drop = FALSE])
    sum((sim_matrix[, i] - reference_mean)^2)
  }, numeric(1))

  envelope <- tibble::tibble(
    lag = seq_len(max_lag),
    mean = sim_mean,
    lo = apply(sim_matrix, 1, stats::quantile, probs = envelope_probs[1]),
    hi = apply(sim_matrix, 1, stats::quantile, probs = envelope_probs[2])
  )

  simulated_curves <- tibble::tibble(
    sim_id = rep(names(sims), each = max_lag),
    lag = rep(seq_len(max_lag), times = length(sims)),
    msd = as.vector(sim_matrix)
  )

  out <- list(
    metric = "msd",
    max_lag = max_lag,
    observed_curve = obs_curve,
    simulated_curves = simulated_curves,
    envelope = envelope,
    simulated_statistics = tibble::tibble(
      sim_id = names(sims),
      statistic = unname(sim_statistics)
    ),
    statistic = observed_statistic,
    rank_test = mc_rank_test(observed_statistic, sim_statistics, alternative = "greater")
  )
  class(out) <- c("gmov_metric_msd", "list")
  out
}

#' Validate path sinuosity
#'
#' Compares observed and simulated path structure using the straightness index,
#' defined as net displacement divided by total path length. Low values indicate
#' more tortuous trajectories.
#'
#' @param observed Observed track-like object.
#' @param simulated A list of simulated track-like objects or a data frame with
#'   a simulation identifier column.
#' @param ... Passed to [as_gmov_simulations()].
#'
#' @return A list with observed and simulated straightness indices, discrepancy
#'   statistics, and a Monte Carlo rank test.
#' @export
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. *Journal of Theoretical Biology*, 229(2), 209-220.
#' <https://doi.org/10.1016/j.jtbi.2004.03.016>
#'
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' @examples
#' observed <- data.frame(x = c(0, 1, 1), y = c(0, 0, 1))
#' simulated <- list(
#'   data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
#'   data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
#' )
#' validate_sinuosity(observed, simulated)
validate_sinuosity <- function(observed, simulated, ...) {
  obs <- as_gmov_track(observed)
  sims <- as_gmov_simulations(simulated, ...)
  check_simulation_count(sims, minimum = 2)

  observed_value <- straightness_index(obs)
  simulated_values <- vapply(sims, straightness_index, numeric(1))

  if (!is.finite(observed_value) || any(!is.finite(simulated_values))) {
    stop("Straightness index is undefined for at least one zero-length path.", call. = FALSE)
  }

  observed_statistic <- abs(observed_value - mean(simulated_values, na.rm = TRUE))
  sim_statistics <- vapply(seq_along(simulated_values), function(i) {
    abs(simulated_values[i] - mean(simulated_values[-i], na.rm = TRUE))
  }, numeric(1))

  out <- list(
    metric = "sinuosity",
    measure = "straightness_index",
    observed_value = observed_value,
    simulated_values = tibble::tibble(
      sim_id = names(sims),
      value = unname(simulated_values)
    ),
    statistic = observed_statistic,
    simulated_statistics = tibble::tibble(
      sim_id = names(sims),
      statistic = unname(sim_statistics)
    ),
    rank_test = mc_rank_test(observed_statistic, sim_statistics, alternative = "greater")
  )
  class(out) <- c("gmov_metric_sinuosity", "list")
  out
}

#' Validate barrier crossing behavior
#'
#' Counts movement segments that intersect a known linear barrier. This is a
#' segment-intersection proxy for barrier crossing: a segment that touches,
#' overlaps, or crosses the barrier is counted once, even if it intersects
#' multiple barrier features. This diagnostic is intended for settings where a
#' barrier is specified before validation. It does not discover unknown barriers.
#'
#' @param observed Observed track-like object.
#' @param simulated A list of simulated track-like objects or a data frame with
#'   a simulation identifier column.
#' @param barrier An `sf` or `sfc` object containing LINESTRING or
#'   MULTILINESTRING geometries. The barrier and track coordinates are assumed
#'   to use the same planar coordinate system.
#' @param alternative Character passed to [mc_rank_test()]. The default
#'   `"less"` tests whether observed crossings are unusually few relative to
#'   simulations, as in an over-permeable fitted model.
#' @param ... Passed to [as_gmov_simulations()].
#'
#' @return A list with observed and simulated crossing counts and a Monte Carlo
#'   rank test.
#' @export
#'
#' @references
#' Nicosia, A. (2026). Beyond the next step: A multi-criteria generative
#' validation framework for step selection functions. *Methods in Ecology and
#' Evolution*. <https://doi.org/10.1111/2041-210x.70313>
#'
#' @examples
#' barrier <- sf::st_sfc(
#'   sf::st_linestring(matrix(c(0.5, -1, 0.5, 1), ncol = 2, byrow = TRUE)),
#'   crs = 3857
#' )
#' observed <- data.frame(x = c(0, 0.25, 0.4), y = c(0, 0, 0))
#' simulated <- list(
#'   data.frame(x = c(0, 1), y = c(0, 0)),
#'   data.frame(x = c(0, 0.75), y = c(0.2, 0.2))
#' )
#' validate_barrier_crossing(observed, simulated, barrier)
validate_barrier_crossing <- function(
  observed,
  simulated,
  barrier,
  alternative = c("less", "greater", "two.sided"),
  ...
) {
  alternative <- match.arg(alternative)

  if (missing(barrier) || is.null(barrier)) {
    stop("`barrier` is required for barrier crossing validation.", call. = FALSE)
  }

  obs <- as_gmov_track(observed)
  sims <- as_gmov_simulations(simulated, ...)
  barrier_geom <- validate_barrier_geometry(barrier)

  observed_count <- count_barrier_crossings(obs, barrier_geom)
  simulated_counts <- vapply(sims, count_barrier_crossings, integer(1), barrier_geom = barrier_geom)

  out <- list(
    metric = "barrier",
    observed_count = observed_count,
    simulated_counts = tibble::tibble(
      sim_id = names(sims),
      count = unname(simulated_counts)
    ),
    rank_test = mc_rank_test(observed_count, simulated_counts, alternative = alternative),
    barrier = barrier
  )
  class(out) <- c("gmov_metric_barrier", "list")
  out
}

normalize_grid_size <- function(grid_size) {
  if (!is.numeric(grid_size) || !length(grid_size) %in% c(1L, 2L)) {
    stop("`grid_size` must be an integer scalar or length-two integer vector.", call. = FALSE)
  }

  if (any(!is.finite(grid_size)) || any(grid_size != floor(grid_size))) {
    stop("`grid_size` values must be whole numbers.", call. = FALSE)
  }

  grid_size <- as.integer(grid_size)
  if (length(grid_size) == 1L) {
    grid_size <- rep(grid_size, 2)
  }

  if (any(!is.finite(grid_size)) || any(grid_size < 2L)) {
    stop("`grid_size` values must be at least 2.", call. = FALSE)
  }

  grid_size
}

validate_bounds <- function(bounds) {
  required <- c("xmin", "xmax", "ymin", "ymax")
  if (!is.numeric(bounds) || !all(required %in% names(bounds))) {
    stop("`bounds` must be a named numeric vector with xmin, xmax, ymin, and ymax.", call. = FALSE)
  }

  bounds <- bounds[required]
  if (any(!is.finite(bounds))) {
    stop("All `bounds` values must be finite.", call. = FALSE)
  }

  if (bounds["xmin"] >= bounds["xmax"] || bounds["ymin"] >= bounds["ymax"]) {
    stop("`bounds` must satisfy xmin < xmax and ymin < ymax.", call. = FALSE)
  }

  bounds
}

validate_max_lag <- function(max_lag, common_max_lag) {
  if (!is.numeric(max_lag) || length(max_lag) != 1L || !is.finite(max_lag)) {
    stop("`max_lag` must be one finite whole number.", call. = FALSE)
  }

  if (max_lag != floor(max_lag)) {
    stop("`max_lag` must be a whole number.", call. = FALSE)
  }

  max_lag <- as.integer(max_lag)
  if (max_lag < 1L || max_lag > common_max_lag) {
    stop("`max_lag` must be at least 1 and no larger than the largest common lag.", call. = FALSE)
  }

  max_lag
}

validate_envelope_probs <- function(envelope_probs) {
  if (!is.numeric(envelope_probs) || length(envelope_probs) != 2L) {
    stop("`envelope_probs` must be a numeric vector of length two.", call. = FALSE)
  }

  if (any(!is.finite(envelope_probs)) || any(envelope_probs < 0) || any(envelope_probs > 1)) {
    stop("`envelope_probs` values must be finite probabilities.", call. = FALSE)
  }

  if (envelope_probs[1] >= envelope_probs[2]) {
    stop("`envelope_probs` must be ordered from lower to upper probability.", call. = FALSE)
  }

  envelope_probs
}

combined_bounds <- function(obs, sims) {
  all_x <- c(obs$x_, unlist(lapply(sims, `[[`, "x_"), use.names = FALSE))
  all_y <- c(obs$y_, unlist(lapply(sims, `[[`, "y_"), use.names = FALSE))
  bounds <- c(
    xmin = min(all_x),
    xmax = max(all_x),
    ymin = min(all_y),
    ymax = max(all_y)
  )

  if (bounds["xmin"] == bounds["xmax"]) {
    bounds[c("xmin", "xmax")] <- bounds[c("xmin", "xmax")] + c(-0.5, 0.5)
  }
  if (bounds["ymin"] == bounds["ymax"]) {
    bounds[c("ymin", "ymax")] <- bounds[c("ymin", "ymax")] + c(-0.5, 0.5)
  }

  bounds
}

empirical_grid_ud <- function(track, grid_size, bounds) {
  trk <- as_gmov_track(track)
  x_breaks <- seq(bounds["xmin"], bounds["xmax"], length.out = grid_size[1] + 1L)
  y_breaks <- seq(bounds["ymin"], bounds["ymax"], length.out = grid_size[2] + 1L)

  x_bin <- pmin(
    pmax(findInterval(trk$x_, x_breaks, all.inside = TRUE), 1L),
    grid_size[1]
  )
  y_bin <- pmin(
    pmax(findInterval(trk$y_, y_breaks, all.inside = TRUE), 1L),
    grid_size[2]
  )

  cells <- tibble::tibble(x_bin = x_bin, y_bin = y_bin)
  counts <- as.data.frame(table(cells$x_bin, cells$y_bin), stringsAsFactors = FALSE)
  names(counts) <- c("x_bin", "y_bin", "n")
  counts$x_bin <- as.integer(as.character(counts$x_bin))
  counts$y_bin <- as.integer(as.character(counts$y_bin))
  counts <- counts[counts$n > 0, , drop = FALSE]

  tibble::tibble(
    x = x_breaks[counts$x_bin] + diff(x_breaks)[counts$x_bin] / 2,
    y = y_breaks[counts$y_bin] + diff(y_breaks)[counts$y_bin] / 2,
    mass = counts$n / sum(counts$n)
  )
}

wasserstein_ud <- function(ud_a, ud_b) {
  a <- transport::wpp(as.matrix(ud_a[, c("x", "y")]), ud_a$mass)
  b <- transport::wpp(as.matrix(ud_b[, c("x", "y")]), ud_b$mass)
  transport::wasserstein(a, b, p = 1, method = "networkflow")
}

validate_barrier_geometry <- function(barrier) {
  if (!inherits(barrier, "sf") && !inherits(barrier, "sfc")) {
    stop("`barrier` must be an sf or sfc LINESTRING or MULTILINESTRING object.", call. = FALSE)
  }

  barrier_geom <- if (inherits(barrier, "sf")) sf::st_geometry(barrier) else barrier
  geom_type <- unique(as.character(sf::st_geometry_type(barrier_geom)))

  if (!all(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("`barrier` must contain only LINESTRING or MULTILINESTRING geometries.", call. = FALSE)
  }

  if (is.na(sf::st_crs(barrier_geom))) {
    warning("`barrier` has no CRS. Assuming it uses the same coordinates as the tracks.", call. = FALSE)
  }

  barrier_geom
}

count_barrier_crossings <- function(track, barrier_geom) {
  trk <- as_gmov_track(track)
  if (nrow(trk) < 2L) {
    return(0L)
  }

  barrier_crs <- sf::st_crs(barrier_geom)
  segment_list <- lapply(seq_len(nrow(trk) - 1L), function(i) {
    sf::st_linestring(matrix(
      c(trk$x_[i], trk$y_[i], trk$x_[i + 1L], trk$y_[i + 1L]),
      ncol = 2,
      byrow = TRUE
    ))
  })

  segments <- sf::st_sfc(segment_list, crs = barrier_crs)
  intersections <- sf::st_intersects(segments, barrier_geom, sparse = FALSE)
  as.integer(sum(rowSums(intersections) > 0))
}
