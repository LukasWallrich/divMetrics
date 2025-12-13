testthat::test_that("compute_cv returns correct coefficient and handles zero mean", {
  # For x = c(2, 4): mean = 3, sample sd = sqrt(2)=1.4142136, CV ~ 0.4714045.
  result <- compute_cv(c(2, 4), na.rm = TRUE)
  testthat::expect_equal(result, 1.4142136 / 3, tolerance = 1e-6)
  # For x with zero mean, expect NA and a warning.
  testthat::expect_warning(
    testthat::expect_true(is.na(compute_cv(c(0, 0, 0), na.rm = TRUE))),
    "Mean is zero"
  )
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

testthat::test_that("compute_GMD returns correct Gini Mean Difference", {
  # For x = c(1, 2, 3): all pairwise absolute differences: |1-2|=1, |1-3|=2, |2-3|=1
  # Mean = (1 + 2 + 1) / 3 = 4/3 ≈ 1.333
  result <- compute_GMD(c(1, 2, 3), na.rm = TRUE)
  expected <- mean(c(1, 2, 1))  # Using dist() logic
  testthat::expect_equal(result, expected, tolerance = 1e-6)
})

testthat::test_that("compute_GMD handles single value (returns 0)", {
  result <- compute_GMD(c(5), na.rm = TRUE)
  testthat::expect_equal(result, 0)
})

testthat::test_that("compute_GMD handles two identical values (returns 0)", {
  result <- compute_GMD(c(5, 5), na.rm = TRUE)
  testthat::expect_equal(result, 0)
})
