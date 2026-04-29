#' @export
print.gmov_generative <- function(x, ...) {
  cat("gmov generative validation\n")
  cat("Simulated tracks:", x$simulated_summary$n_simulations, "\n")
  cat("Metrics:", paste(x$metrics, collapse = ", "), "\n\n")
  print(summary(x), n = Inf)
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
      ud = "mean observed-simulated W1",
      msd = "MSD integrated squared error",
      sinuosity = "absolute straightness deviation",
      barrier = "crossing count",
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
      x = "Barrier crossings",
      y = "Number of simulations",
      title = "Barrier crossing",
      subtitle = paste0("Monte Carlo p = ", format_p(metric_p_value(x)))
    )
}

format_p <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  formatC(x, digits = 3, format = "f")
}
