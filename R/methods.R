#' @export
print.gmov_generative <- function(x, ...) {
  cat("gmov generative validation\n")
  cat("Observed locations:", x$observed_summary$n_locations, "\n")
  cat("Simulated tracks:", x$simulated_summary$n_simulations, "\n")
  cat("Validation pillars:", paste(metric_pillar(x$metrics), collapse = ", "), "\n\n")
  print(generative_print_table(x), n = Inf, width = Inf)
  invisible(x)
}

#' Print or plot individual gmov diagnostic results
#'
#' These methods provide concise printing and simple ggplot2 displays for
#' individual diagnostic objects returned by [validate_ud()], [validate_msd()],
#' [validate_sinuosity()], and [validate_barrier_crossing()].
#'
#' @param x A gmov diagnostic result.
#' @param ... Currently unused.
#'
#' @return Print methods return `x` invisibly. Plot methods return the ggplot
#'   object invisibly after printing it.
#' @name gmov_metric_methods
NULL

#' @rdname gmov_metric_methods
#' @export
print.gmov_metric_ud <- function(x, ...) {
  print_metric_header("gmov utilization distribution diagnostic")
  cat("Method:", x$method %||% "empirical_grid_wasserstein", "\n")
  cat("Simulated tracks:", nrow(x$simulated_statistics), "\n")
  cat("Statistic:", format_number(x$statistic), "\n")
  cat("Monte Carlo p-value:", format_p(metric_p_value(x)), "\n")
  cat("Alternative:", x$rank_test$alternative %||% NA_character_, "\n")
  invisible(x)
}

#' @rdname gmov_metric_methods
#' @export
print.gmov_metric_msd <- function(x, ...) {
  print_metric_header("gmov mean squared displacement diagnostic")
  cat("Maximum lag:", x$max_lag, "\n")
  cat("Simulated tracks:", nrow(x$simulated_statistics), "\n")
  cat("Statistic:", format_number(x$statistic), "\n")
  cat("Monte Carlo p-value:", format_p(metric_p_value(x)), "\n")
  cat("Alternative:", x$rank_test$alternative %||% NA_character_, "\n")
  invisible(x)
}

#' @rdname gmov_metric_methods
#' @export
print.gmov_metric_sinuosity <- function(x, ...) {
  print_metric_header("gmov sinuosity diagnostic")
  cat("Measure:", x$measure %||% "straightness_index", "\n")
  cat("Simulated tracks:", nrow(x$simulated_values), "\n")
  cat("Observed value:", format_number(x$observed_value), "\n")
  cat("Discrepancy statistic:", format_number(x$statistic), "\n")
  cat("Monte Carlo p-value:", format_p(metric_p_value(x)), "\n")
  cat("Alternative:", x$rank_test$alternative %||% NA_character_, "\n")
  invisible(x)
}

#' @rdname gmov_metric_methods
#' @export
print.gmov_metric_barrier <- function(x, ...) {
  print_metric_header("gmov barrier interaction diagnostic")
  cat("Method:", x$method %||% "segment_intersection_count", "\n")
  cat("Simulated tracks:", nrow(x$simulated_counts), "\n")
  cat("Observed count:", x$observed_count, "\n")
  cat("Monte Carlo p-value:", format_p(metric_p_value(x)), "\n")
  cat("Alternative:", x$rank_test$alternative %||% NA_character_, "\n")
  invisible(x)
}

#' @export
summary.gmov_generative <- function(object, ...) {
  rows <- lapply(names(object$metric_results), function(metric_name) {
    result <- object$metric_results[[metric_name]]
    observed_statistic <- switch(
      metric_name,
      ud = result$statistic,
      msd = result$statistic,
      sinuosity = result$observed_value,
      barrier = result$observed_count,
      NA_real_
    )

    discrepancy_statistic <- switch(
      metric_name,
      ud = result$statistic,
      msd = result$statistic,
      sinuosity = result$statistic,
      barrier = result$observed_count,
      NA_real_
    )

    statistic_name <- switch(
      metric_name,
      ud = "mean observed-simulated grid W1",
      msd = "MSD integrated squared error",
      sinuosity = "absolute straightness deviation",
      barrier = "segment-intersection count",
      NA_character_
    )

    tibble::tibble(
      metric = metric_name,
      statistic_name = statistic_name,
      observed_statistic = observed_statistic,
      discrepancy_statistic = discrepancy_statistic,
      p_value = metric_p_value(result),
      alternative = result$rank_test$alternative %||% NA_character_
    )
  })

  tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame)))
}

#' Plot generative validation results
#'
#' Produces simple ggplot2 summaries for available validation metrics.
#'
#' @param x An object returned by [validate_ssf_generative()].
#' @param metric Optional metric to plot. If `NULL`, all available plots are
#'   printed and returned as a named list.
#' @param ... Currently unused.
#'
#' @return A ggplot object when `metric` is supplied, otherwise an invisible
#'   named list of ggplot objects.
#' @export
plot.gmov_generative <- function(x, metric = NULL, ...) {
  plots <- lapply(x$metric_results, plot_metric)

  if (!is.null(metric)) {
    metric <- match.arg(metric, names(plots))
    print(plots[[metric]])
    return(invisible(plots[[metric]]))
  }

  for (plot_i in plots) {
    print(plot_i)
  }
  invisible(plots)
}

#' Plot an article-style generative validation dashboard
#'
#' Draws a compact multi-panel dashboard for a `gmov_generative` result. When
#' observed and simulated tracks are supplied, the first panel shows observed
#' and simulated trajectories. The remaining panels show the available
#' diagnostic plots using the same plotting methods as [plot.gmov_generative()].
#' This is a visualization helper only; it does not compute additional
#' validation metrics.
#'
#' @param x An object returned by [validate_ssf_generative()].
#' @param observed Optional observed track-like object. If supplied with
#'   `simulated`, a trajectory panel is included.
#' @param simulated Optional simulated tracks, in any format supported by
#'   [as_gmov_simulations()]. If supplied with `observed`, a trajectory panel is
#'   included.
#' @param n_simulations Maximum number of simulated tracks shown in the
#'   trajectory panel.
#' @param ... Passed to [as_gmov_track()] and [as_gmov_simulations()] when
#'   building the trajectory panel.
#'
#' @return Invisibly returns a named list of ggplot objects after drawing the
#'   dashboard.
#' @export
#'
#' @examples
#' observed <- data.frame(x = c(0, 1, 1, 2), y = c(0, 0, 1, 1))
#' simulated <- list(
#'   data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0)),
#'   data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 2)),
#'   data.frame(x = c(0, 1, 1, 1), y = c(0, 0, 1, 2))
#' )
#' res <- validate_ssf_generative(
#'   observed,
#'   simulated,
#'   metrics = c("msd", "sinuosity"),
#'   msd_args = list(max_lag = 2)
#' )
#' plot_gmov_dashboard(res, observed, simulated)
plot_gmov_dashboard <- function(x, observed = NULL, simulated = NULL, n_simulations = 25, ...) {
  if (!inherits(x, "gmov_generative")) {
    stop("`x` must be a `gmov_generative` object.", call. = FALSE)
  }

  metric_order <- intersect(c("ud", "msd", "sinuosity", "barrier"), names(x$metric_results))
  plots <- lapply(x$metric_results[metric_order], plot_metric)

  if (!is.null(observed) || !is.null(simulated)) {
    if (is.null(observed) || is.null(simulated)) {
      stop("Both `observed` and `simulated` must be supplied for the trajectory panel.", call. = FALSE)
    }
    plots <- c(
      list(trajectories = plot_dashboard_tracks(observed, simulated, n_simulations, ...)),
      plots
    )
  }

  draw_gmov_dashboard(plots)
  invisible(plots)
}

#' @rdname gmov_metric_methods
#' @export
plot.gmov_metric_ud <- function(x, ...) {
  plot_i <- plot_metric_ud(x)
  print(plot_i)
  invisible(plot_i)
}

#' @rdname gmov_metric_methods
#' @export
plot.gmov_metric_msd <- function(x, ...) {
  plot_i <- plot_metric_msd(x)
  print(plot_i)
  invisible(plot_i)
}

#' @rdname gmov_metric_methods
#' @export
plot.gmov_metric_sinuosity <- function(x, ...) {
  plot_i <- plot_metric_sinuosity(x)
  print(plot_i)
  invisible(plot_i)
}

#' @rdname gmov_metric_methods
#' @export
plot.gmov_metric_barrier <- function(x, ...) {
  plot_i <- plot_metric_barrier(x)
  print(plot_i)
  invisible(plot_i)
}

plot_metric <- function(x) {
  if (inherits(x, "gmov_metric_ud")) {
    return(plot_metric_ud(x))
  }
  if (inherits(x, "gmov_metric_msd")) {
    return(plot_metric_msd(x))
  }
  if (inherits(x, "gmov_metric_sinuosity")) {
    return(plot_metric_sinuosity(x))
  }
  if (inherits(x, "gmov_metric_barrier")) {
    return(plot_metric_barrier(x))
  }
  stop("No plot method is available for this metric result.", call. = FALSE)
}

plot_metric_ud <- function(x) {
  cols <- gmov_plot_colours()
  ggplot2::ggplot(x$simulated_statistics, ggplot2::aes(x = statistic)) +
    ggplot2::geom_histogram(bins = 15, fill = cols$simulated, color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = x$statistic, color = cols$observed, linewidth = 1) +
    theme_gmov_diagnostic() +
    ggplot2::labs(
      x = "Mean 1-Wasserstein distance",
      y = "Number of simulations",
      title = "Emergent utilization distribution",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_msd <- function(x) {
  cols <- gmov_plot_colours()
  ggplot2::ggplot(x$envelope, ggplot2::aes(x = lag)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      fill = cols$simulated,
      alpha = 0.22
    ) +
    ggplot2::geom_line(ggplot2::aes(y = mean), color = cols$simulated, linetype = "dashed") +
    ggplot2::geom_line(
      data = x$observed_curve,
      ggplot2::aes(y = msd),
      color = cols$observed,
      linewidth = 0.9
    ) +
    theme_gmov_diagnostic() +
    ggplot2::labs(
      x = "Lag",
      y = "Mean squared displacement",
      title = "Mean squared displacement",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_sinuosity <- function(x) {
  cols <- gmov_plot_colours()
  ggplot2::ggplot(x$simulated_values, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 15, fill = cols$simulated, color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = x$observed_value, color = cols$observed, linewidth = 1) +
    theme_gmov_diagnostic() +
    ggplot2::labs(
      x = "Straightness index",
      y = "Number of simulations",
      title = "Path sinuosity",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_barrier <- function(x) {
  cols <- gmov_plot_colours()
  ggplot2::ggplot(x$simulated_counts, ggplot2::aes(x = count)) +
    ggplot2::geom_histogram(
      binwidth = 1,
      fill = cols$simulated,
      color = "white",
      boundary = -0.5,
      alpha = 0.75
    ) +
    ggplot2::geom_vline(xintercept = x$observed_count, color = cols$observed, linewidth = 1) +
    theme_gmov_diagnostic() +
    ggplot2::labs(
      x = "Movement segments intersecting barrier",
      y = "Number of simulations",
      title = "Barrier interactions",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

format_p <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  formatC(x, digits = 3, format = "f")
}

format_number <- function(x) {
  formatC(x, digits = 4, format = "fg")
}

print_metric_header <- function(title) {
  cat(title, "\n", sep = "")
}

metric_pillar <- function(metric) {
  unname(c(
    ud = "Emergent space use",
    msd = "Diffusion behavior",
    sinuosity = "Path structure",
    barrier = "Barrier interactions"
  )[metric])
}

generative_print_table <- function(x) {
  summary_tbl <- summary(x)
  tibble::tibble(
    pillar = metric_pillar(summary_tbl$metric),
    metric = summary_tbl$metric,
    diagnostic = summary_tbl$statistic_name,
    observed = summary_tbl$observed_statistic,
    discrepancy = summary_tbl$discrepancy_statistic,
    p_value = summary_tbl$p_value,
    alternative = summary_tbl$alternative
  )
}

gmov_plot_colours <- function() {
  list(
    simulated = "#4C78A8",
    observed = "#D55E00",
    reference = "#4A4A4A",
    grid = "grey90"
  )
}

theme_gmov_diagnostic <- function(base_size = 11) {
  cols <- gmov_plot_colours()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = cols$grid, linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "plain"),
      legend.position = "bottom"
    )
}

plot_dashboard_tracks <- function(observed, simulated, n_simulations = 25, ...) {
  if (!is.numeric(n_simulations) || length(n_simulations) != 1L ||
      !is.finite(n_simulations) || n_simulations < 1) {
    stop("`n_simulations` must be a positive number.", call. = FALSE)
  }

  cols <- gmov_plot_colours()
  obs <- as_gmov_track(observed, ...)
  sims <- as_gmov_simulations(simulated, ...)
  sims <- sims[seq_len(min(as.integer(n_simulations), length(sims)))]

  sims_df <- do.call(
    rbind,
    Map(
      function(track, id) {
        tibble::tibble(sim_id = id, x_ = track$x_, y_ = track$y_)
      },
      sims,
      names(sims)
    )
  )

  x_ <- y_ <- sim_id <- NULL

  ggplot2::ggplot() +
    ggplot2::geom_path(
      data = sims_df,
      ggplot2::aes(x = x_, y = y_, group = sim_id),
      color = cols$simulated,
      alpha = 0.35,
      linewidth = 0.25
    ) +
    ggplot2::geom_path(
      data = obs,
      ggplot2::aes(x = x_, y = y_),
      color = cols$observed,
      linewidth = 0.85
    ) +
    ggplot2::coord_equal() +
    theme_gmov_diagnostic(base_size = 10) +
    ggplot2::labs(
      x = "x-coordinate",
      y = "y-coordinate",
      title = "Observed and simulated trajectories",
      subtitle = paste0("Observed track and ", length(sims), " simulated tracks")
    )
}

draw_gmov_dashboard <- function(plots) {
  if (length(plots) < 1L) {
    stop("No plots are available for this dashboard.", call. = FALSE)
  }

  grid::grid.newpage()

  if ("trajectories" %in% names(plots) && length(plots) > 1L) {
    metric_plots <- plots[names(plots) != "trajectories"]
    n_metric_cols <- if (length(metric_plots) == 1L) 1L else 2L
    n_metric_rows <- ceiling(length(metric_plots) / n_metric_cols)
    layout <- grid::grid.layout(
      nrow = n_metric_rows,
      ncol = n_metric_cols + 1L,
      widths = grid::unit(c(1.35, rep(1, n_metric_cols)), "null")
    )
    grid::pushViewport(grid::viewport(layout = layout))
    print(
      plots$trajectories,
      vp = grid::viewport(layout.pos.row = seq_len(n_metric_rows), layout.pos.col = 1L)
    )
    for (i in seq_along(metric_plots)) {
      row_i <- ceiling(i / n_metric_cols)
      col_i <- ((i - 1L) %% n_metric_cols) + 2L
      print(metric_plots[[i]], vp = grid::viewport(layout.pos.row = row_i, layout.pos.col = col_i))
    }
    grid::popViewport()
    return(invisible(plots))
  }

  n_cols <- if (length(plots) == 1L) 1L else 2L
  n_rows <- ceiling(length(plots) / n_cols)
  layout <- grid::grid.layout(nrow = n_rows, ncol = n_cols)
  grid::pushViewport(grid::viewport(layout = layout))
  for (i in seq_along(plots)) {
    row_i <- ceiling(i / n_cols)
    col_i <- ((i - 1L) %% n_cols) + 1L
    print(plots[[i]], vp = grid::viewport(layout.pos.row = row_i, layout.pos.col = col_i))
  }
  grid::popViewport()
  invisible(plots)
}
