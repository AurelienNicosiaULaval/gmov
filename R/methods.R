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
  ggplot2::ggplot(x$simulated_statistics, ggplot2::aes(x = statistic)) +
    ggplot2::geom_histogram(bins = 15, fill = "grey75", color = "white") +
    ggplot2::geom_vline(xintercept = x$statistic, linewidth = 0.8) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Mean 1-Wasserstein distance",
      y = "Number of simulations",
      title = "Emergent utilization distribution",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_msd <- function(x) {
  ggplot2::ggplot(x$envelope, ggplot2::aes(x = lag)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "grey80") +
    ggplot2::geom_line(ggplot2::aes(y = mean), linetype = "dashed") +
    ggplot2::geom_line(data = x$observed_curve, ggplot2::aes(y = msd), linewidth = 0.8) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Lag",
      y = "Mean squared displacement",
      title = "Mean squared displacement",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_sinuosity <- function(x) {
  ggplot2::ggplot(x$simulated_values, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 15, fill = "grey75", color = "white") +
    ggplot2::geom_vline(xintercept = x$observed_value, linewidth = 0.8) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Straightness index",
      y = "Number of simulations",
      title = "Path sinuosity",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

plot_metric_barrier <- function(x) {
  ggplot2::ggplot(x$simulated_counts, ggplot2::aes(x = count)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "grey75", color = "white", boundary = -0.5) +
    ggplot2::geom_vline(xintercept = x$observed_count, linewidth = 0.8) +
    ggplot2::theme_bw() +
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
