test_that("validate_ssf_generative returns expected class and components", {
  observed <- data.frame(x = c(0, 1, 1, 2), y = c(0, 0, 1, 1))
  simulated <- list(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0)),
    data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 2)),
    data.frame(x = c(0, 1, 1, 1), y = c(0, 0, 1, 2))
  )

  res <- validate_ssf_generative(
    observed = observed,
    simulated = simulated,
    metrics = c("ud", "msd", "sinuosity"),
    ud_args = list(grid_size = 4),
    msd_args = list(max_lag = 2)
  )

  expect_s3_class(res, "gmov_generative")
  expect_named(
    res,
    c("call", "observed_summary", "simulated_summary", "metrics", "metric_results", "settings")
  )
  expect_true(all(c("ud", "msd", "sinuosity") %in% names(res$metric_results)))
  expect_equal(res$simulated_summary$n_simulations, 3)

  res_summary <- summary(res)
  expect_named(
    res_summary,
    c("metric", "statistic_name", "observed_statistic", "discrepancy_statistic", "p_value", "alternative")
  )
})

test_that("validate_ssf_generative skips barrier when barrier is NULL", {
  observed <- data.frame(x = c(0, 1, 2), y = c(0, 0, 0))
  simulated <- list(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
    data.frame(x = c(0, 1, 2), y = c(0, 1, 1))
  )

  expect_warning(
    res <- validate_ssf_generative(
      observed = observed,
      simulated = simulated,
      metrics = c("sinuosity", "barrier")
    ),
    "Skipping `barrier`"
  )
  expect_equal(res$metrics, "sinuosity")
})
