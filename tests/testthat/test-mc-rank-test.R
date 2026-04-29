test_that("mc_rank_test computes one-sided p-values with +1 correction", {
  res <- mc_rank_test(5, c(1, 2, 3, 4), alternative = "greater")

  expect_equal(res$p_value, 1 / 5)
  expect_equal(res$rank, 5)
  expect_equal(res$alternative, "greater")
})

test_that("mc_rank_test computes two-sided p-values", {
  res <- mc_rank_test(0, c(-3, -2, -1, 1, 2, 3), alternative = "two.sided")

  expect_equal(res$p_value, 1)
})
