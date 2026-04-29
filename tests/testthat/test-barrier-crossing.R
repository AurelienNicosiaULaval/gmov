test_that("barrier crossing counts simple LINESTRING intersections", {
  barrier <- sf::st_sfc(
    sf::st_linestring(matrix(c(0.5, -1, 0.5, 1), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  observed <- data.frame(x = c(0, 0.25, 0.4), y = c(0, 0, 0))
  simulated <- list(
    data.frame(x = c(0, 1), y = c(0, 0)),
    data.frame(x = c(0, 0.75), y = c(0.2, 0.2))
  )

  res <- validate_barrier_crossing(observed, simulated, barrier)

  expect_equal(res$observed_count, 0)
  expect_equal(res$simulated_counts$count, c(1, 1))
  expect_equal(res$method, "segment_intersection_count")
  expect_equal(res$rank_test$alternative, "less")
})

test_that("barrier crossing handles MULTILINESTRING-like sfc input", {
  barrier <- sf::st_sfc(
    sf::st_multilinestring(list(
      matrix(c(0.5, -1, 0.5, 1), ncol = 2, byrow = TRUE),
      matrix(c(1.5, -1, 1.5, 1), ncol = 2, byrow = TRUE)
    )),
    crs = 3857
  )

  observed <- data.frame(x = c(0, 1, 2), y = c(0, 0, 0))
  simulated <- list(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0)),
    data.frame(x = c(0, 0.25), y = c(0, 0))
  )

  res <- validate_barrier_crossing(observed, simulated, barrier)

  expect_equal(res$observed_count, 2)
  expect_equal(res$simulated_counts$count, c(2, 0))
})

test_that("barrier crossing returns zero when segments do not intersect", {
  barrier <- sf::st_sfc(
    sf::st_linestring(matrix(c(0.5, -1, 0.5, 1), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  observed <- data.frame(x = c(0, 0.25), y = c(2, 2))
  simulated <- list(
    data.frame(x = c(0, 0.25), y = c(3, 3)),
    data.frame(x = c(1, 1.25), y = c(3, 3))
  )

  res <- validate_barrier_crossing(observed, simulated, barrier)

  expect_equal(res$observed_count, 0)
  expect_equal(res$simulated_counts$count, c(0, 0))
})

test_that("barrier crossing counts touching and overlapping segments", {
  barrier <- sf::st_sfc(
    sf::st_linestring(matrix(c(0.5, -1, 0.5, 1), ncol = 2, byrow = TRUE)),
    crs = 3857
  )

  observed <- data.frame(x = c(0, 0.5), y = c(0, 0))
  simulated <- list(
    data.frame(x = c(0.5, 0.5), y = c(-0.5, 0.5)),
    data.frame(x = c(0, 1), y = c(0.25, 0.25))
  )

  res <- validate_barrier_crossing(observed, simulated, barrier)

  expect_equal(res$observed_count, 1)
  expect_equal(res$simulated_counts$count, c(1, 1))
})
