test_that("individual metric print methods are available", {
  observed <- data.frame(x = c(0, 1, 1, 2), y = c(0, 0, 1, 1))
  simulated <- list(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0)),
    data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 2)),
    data.frame(x = c(0, 1, 1, 1), y = c(0, 0, 1, 2))
  )
  barrier <- sf::st_sfc(
    sf::st_linestring(matrix(c(0.5, -1, 0.5, 3), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  metrics <- list(
    validate_ud(observed, simulated, grid_size = 4),
    validate_msd(observed, simulated, max_lag = 2),
    validate_sinuosity(observed, simulated),
    validate_barrier_crossing(observed, simulated, barrier)
  )

  for (metric in metrics) {
    expect_no_error(output <- capture.output(print(metric)))
    expect_true(length(output) > 0)
  }
})

test_that("individual metric plot methods return ggplot objects", {
  tmp_plot <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_plot)
  on.exit(grDevices::dev.off(), add = TRUE)
  on.exit(unlink(tmp_plot), add = TRUE)

  observed <- data.frame(x = c(0, 1, 1, 2), y = c(0, 0, 1, 1))
  simulated <- list(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0)),
    data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 2)),
    data.frame(x = c(0, 1, 1, 1), y = c(0, 0, 1, 2))
  )
  barrier <- sf::st_sfc(
    sf::st_linestring(matrix(c(0.5, -1, 0.5, 3), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  metrics <- list(
    validate_ud(observed, simulated, grid_size = 4),
    validate_msd(observed, simulated, max_lag = 2),
    validate_sinuosity(observed, simulated),
    validate_barrier_crossing(observed, simulated, barrier)
  )

  for (metric in metrics) {
    expect_no_error(plot_i <- plot(metric))
    expect_true(inherits(plot_i, "ggplot"))
  }
})
