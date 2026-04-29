test_that("sinuosity uses the straightness index", {
  observed <- data.frame(x = c(0, 3), y = c(0, 4))
  simulated <- list(
    data.frame(x = c(0, 3), y = c(0, 4)),
    data.frame(x = c(0, 0, 3), y = c(0, 4, 4))
  )

  res <- validate_sinuosity(observed, simulated)

  expect_equal(res$observed_value, 1)
  expect_true(all(res$simulated_values$value <= 1))
  expect_true(all(res$simulated_values$value > 0))
})

test_that("sinuosity errors for zero-length paths", {
  observed <- data.frame(x = c(0, 0), y = c(0, 0))
  simulated <- list(
    data.frame(x = c(0, 1), y = c(0, 0)),
    data.frame(x = c(0, 1), y = c(0, 0))
  )

  expect_error(
    validate_sinuosity(observed, simulated),
    "Straightness index is undefined"
  )
})

test_that("MSD is computed over lags", {
  observed <- data.frame(x = 0:4, y = 0)
  simulated <- list(
    data.frame(x = 0:4, y = 0),
    data.frame(x = 0:4, y = 0)
  )

  res <- validate_msd(observed, simulated, max_lag = 3)

  expect_equal(res$observed_curve$lag, 1:3)
  expect_equal(res$observed_curve$msd, c(1, 4, 9))
  expect_equal(res$statistic, 0)
})

test_that("MSD validates max_lag and envelope probabilities", {
  observed <- data.frame(x = 0:4, y = 0)
  simulated <- list(
    data.frame(x = 0:4, y = 0),
    data.frame(x = 0:4, y = 0)
  )

  expect_error(validate_msd(observed, simulated, max_lag = 1.5), "whole number")
  expect_error(
    validate_msd(observed, simulated, envelope_probs = c(0.9, 0.1)),
    "ordered"
  )
})

test_that("UD validation returns Wasserstein statistics", {
  observed <- data.frame(x = c(0, 1, 1), y = c(0, 0, 1))
  simulated <- list(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
    data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
  )

  res <- validate_ud(observed, simulated, grid_size = 4)

  expect_s3_class(res, "gmov_metric_ud")
  expect_true(is.finite(res$statistic))
  expect_equal(nrow(res$simulated_statistics), 2)
  expect_equal(res$method, "empirical_grid_wasserstein")
  expect_equal(sum(res$observed_ud$mass), 1)
})

test_that("UD validation checks grid size and bounds", {
  observed <- data.frame(x = c(0, 1, 1), y = c(0, 0, 1))
  simulated <- list(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
    data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
  )

  expect_error(validate_ud(observed, simulated, grid_size = 4.2), "whole numbers")
  expect_error(
    validate_ud(observed, simulated, bounds = c(xmin = 1, xmax = 0, ymin = 0, ymax = 1)),
    "xmin < xmax"
  )
  expect_error(
    validate_ud(
      observed,
      simulated,
      bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    ),
    "must contain all observed and simulated track coordinates"
  )
})
