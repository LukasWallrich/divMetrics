testthat::test_that("compute_Blau returns correct index with given bins", {
  # x values that fall into two bins equally: two in [0,2) and two in [2,4)
  result <- compute_Blau(c(1, 1.5, 3, 3.5), bins = c(0, 2, 4), na.rm = TRUE)
  # Proportions: 0.5 each, so Blau = 1 - (0.25+0.25) = 0.5.
  testthat::expect_equal(result, 0.5)
})

testthat::test_that("compute_Blau includes values equal to the upper break", {
  # With right = FALSE, values equal to the upper boundary would otherwise be NA.
  res <- compute_Blau(c(0, 2, 4), bin_width = 2, na.rm = TRUE)
  testthat::expect_equal(res, 2 / 3)
})

testthat::test_that("compute_Blau errors when both bin_width and bins are provided", {
  testthat::expect_error(compute_Blau(c(1, 2, 3), bin_width = 1, bins = c(0, 2, 4)))
})
