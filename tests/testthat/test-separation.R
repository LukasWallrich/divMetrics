testthat::test_that("compute_cv returns correct coefficient and handles zero mean", {
  # For x = c(2, 4): mean = 3, sample sd = sqrt(2)=1.4142136, CV ~ 0.4714045.
  result <- compute_cv(c(2, 4), na.rm = TRUE)
  testthat::expect_equal(result, 1.4142136 / 3, tolerance = 1e-6)
  # For x with zero mean, expect NA.
  testthat::expect_true(is.na(compute_cv(c(0, 0, 0), na.rm = TRUE)))
})

testthat::test_that("compute_sd returns correct standard deviation", {
  # For x = c(1,2,3): sample sd = 1.
  result <- compute_sd(c(1, 2, 3), na.rm = TRUE)
  testthat::expect_equal(result, 1)
})

testthat::test_that("NA handling produces warnings", {
  testthat::expect_warning(compute_cv(c(1, NA, 3), na.rm = TRUE))
  testthat::expect_warning(compute_sd(c(1, NA, 3), na.rm = TRUE))
})
