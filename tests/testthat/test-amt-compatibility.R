skip_if_amt_unavailable <- function() {
  testthat::skip_if_not_installed("amt")
  if (!suppressWarnings(requireNamespace("amt", quietly = TRUE))) {
    testthat::skip("amt is installed but cannot be loaded in this R session.")
  }
}

test_that("as_gmov_track accepts amt track_xy objects", {
  skip_if_amt_unavailable()

  xy <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 0, 1)
  )
  trk_amt <- amt::make_track(xy, x, y)

  trk <- as_gmov_track(trk_amt)

  expect_equal(trk$x_, xy$x)
  expect_equal(trk$y_, xy$y)
})

test_that("as_gmov_track accepts amt step objects", {
  skip_if_amt_unavailable()

  xy <- data.frame(
    x = c(0, 1, 2),
    y = c(0, 0, 1)
  )
  trk_amt <- amt::make_track(xy, x, y)
  steps_amt <- amt::steps(trk_amt)

  trk <- as_gmov_track(steps_amt)

  expect_equal(trk$x_, xy$x)
  expect_equal(trk$y_, xy$y)
})
