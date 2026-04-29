test_that("as_gmov_track accepts x/y and x_/y_ columns", {
  trk_xy <- as_gmov_track(data.frame(x = c(0, 1), y = c(0, 1)))
  trk_amt <- as_gmov_track(data.frame(x_ = c(0, 1), y_ = c(0, 1)))

  expect_named(trk_xy, c("x", "y", "x_", "y_"))
  expect_equal(trk_xy$x_, trk_amt$x_)
  expect_equal(trk_xy$y_, trk_amt$y_)
})

test_that("as_gmov_track converts amt-style steps to a path", {
  steps <- data.frame(
    x1_ = c(0, 1),
    y1_ = c(0, 0),
    x2_ = c(1, 2),
    y2_ = c(0, 0)
  )

  trk <- as_gmov_track(steps)

  expect_equal(trk$x_, c(0, 1, 2))
  expect_equal(trk$y_, c(0, 0, 0))
})

test_that("as_gmov_track accepts sf point tracks", {
  pts <- sf::st_as_sf(
    data.frame(id = 1:3, x = c(0, 1, 2), y = c(0, 0, 1)),
    coords = c("x", "y"),
    crs = 3857
  )

  trk <- as_gmov_track(pts)

  expect_equal(trk$x_, c(0, 1, 2))
  expect_equal(trk$y_, c(0, 0, 1))
  expect_equal(trk$id, 1:3)
})

test_that("as_gmov_simulations splits data frames by simulation id", {
  sims <- data.frame(
    sim_id = rep(c("a", "b"), each = 2),
    x = c(0, 1, 0, 0),
    y = c(0, 0, 0, 1)
  )

  out <- as_gmov_simulations(sims)

  expect_s3_class(out, "gmov_simulations")
  expect_length(out, 2)
})

test_that("as_gmov_simulations honors an explicit simulation id column", {
  sims <- data.frame(
    run = rep(c("a", "b"), each = 2),
    x = c(0, 1, 0, 0),
    y = c(0, 0, 0, 1)
  )

  out <- as_gmov_simulations(sims, id_col = "run")

  expect_s3_class(out, "gmov_simulations")
  expect_named(out, c("a", "b"))
})

test_that("as_gmov_simulations detects common simulation id columns", {
  sims <- data.frame(
    .replicate = rep(c("a", "b"), each = 2),
    x = c(0, 1, 0, 0),
    y = c(0, 0, 0, 1)
  )

  out <- as_gmov_simulations(sims)

  expect_s3_class(out, "gmov_simulations")
  expect_named(out, c("a", "b"))
})

test_that("as_gmov_simulations requires a simulation id for data frames", {
  sims <- data.frame(x = c(0, 1), y = c(0, 0))

  expect_error(
    as_gmov_simulations(sims),
    "simulation identifier"
  )
})
