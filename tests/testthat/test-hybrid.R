library(divMetrics)

# ---- Tests for compute_Rao ----

testthat::test_that("compute_Rao computes correct value with given bins", {
  # x = c(1, 1, 3, 3) and bins = c(0, 2, 4)
  # Counts: 2 in each bin; proportions: 0.5 each; midpoint distance = 2.
  # Rao = 2 * (0.25 * 2) = 1.
  result <- compute_Rao(c(1, 1, 3, 3), bins = c(0, 2, 4), na.rm = TRUE)
  testthat::expect_equal(result, 1)
})

testthat::test_that("compute_Rao works with bin_width parameter", {
  # Using bin_width instead of bins: for x = c(1, 1, 3, 3) with bin_width = 2
  # Expected: same as using bins = c(0, 2, 4)
  x <- c(1, 1, 3, 3)
  result1 <- compute_Rao(x, bin_width = 2, na.rm = TRUE)
  result2 <- compute_Rao(x, bins = c(0, 2, 4), na.rm = TRUE)
  testthat::expect_equal(result1, result2)
})

testthat::test_that("compute_Rao errors when both bin_width and bins are provided", {
  testthat::expect_error(
    compute_Rao(c(1, 1, 3, 3), bin_width = 2, bins = c(0, 2, 4), na.rm = TRUE)
  )
})

testthat::test_that("compute_Rao handles NA values correctly", {
  x <- c(1, 1, 3, NA, 3)
  testthat::expect_warning(
    result <- compute_Rao(x, bins = c(0, 2, 4), na.rm = TRUE)
  )
  testthat::expect_equal(
    result,
    compute_Rao(c(1, 1, 3, 3), bins = c(0, 2, 4), na.rm = TRUE)
  )
  testthat::expect_error(
    compute_Rao(x, bins = c(0, 2, 4), na.rm = FALSE)
  )
})

# ---- Tests for compute_CEI ----


testthat::test_that("compute_CEI computes correct value and handles grouping", {
  # For x = c(20, 40, 60) with range = c(20,60):
  # C = 40/40 = 1; ideal = c(20,40,60) so abs deviation = 0; E = 1; CEI = 1.
  result <- compute_CEI(c(20, 40, 60), range = c(20, 60), na.rm = TRUE)
  testthat::expect_equal(result, 1)
  # Error when an invalid return parameter is used.
  testthat::expect_error(compute_CEI(c(20, 40, 60), range = c(20, 60), na.rm = TRUE, return = "invalid"))
  # Grouped calculation: two identical groups should both return 1.
  x <- rep(c(20, 40, 60), 2)
  group <- rep(c("A", "B"), each = 3)
  result_group <- compute_CEI(x, group = group, range = c(20, 60), na.rm = TRUE)
  testthat::expect_equal(result_group %>% unname(), c(1, 1))
})

testthat::test_that("compute_CEI returns 0 for a single-value input", {
  # When there is only one value, diversity is 0.
  result <- compute_CEI(42, range = c(0, 100), na.rm = TRUE)
  testthat::expect_equal(result, 0)
})


testthat::test_that("compute_CEI errors with invalid range input", {
  # Invalid if range vector is reversed or too short.
  testthat::expect_error(
    compute_CEI(c(20, 40, 60), range = c(60, 20), na.rm = TRUE)
  )
  testthat::expect_error(
    compute_CEI(c(20, 40, 60), range = c(20), na.rm = TRUE)
  )
})

testthat::test_that("compute_CEI grouping produces a named vector", {
  x <- rep(c(20, 40, 60), 2)
  group <- rep(c("A", "B"), each = 3)
  result <- compute_CEI(x, group = group, range = c(20, 60), na.rm = TRUE, return = "CEI")
  testthat::expect_equal(names(result), c("A", "B"))
})

testthat::test_that("compute_CEI returns correct values for each component", {
  x <- c(20, 40, 60)
  # For a perfectly uniform spread over the full range, we expect all components to equal 1.
  result_CEI <- compute_CEI(x, range = c(20, 60), na.rm = TRUE, return = "CEI")
  result_C   <- compute_CEI(x, range = c(20, 60), na.rm = TRUE, return = "C")
  result_E   <- compute_CEI(x, range = c(20, 60), na.rm = TRUE, return = "E")
  testthat::expect_equal(result_CEI, 1)
  testthat::expect_equal(result_C, 1)
  testthat::expect_equal(result_E, 1)
})

testthat::test_that("compute_CEI handles NA values correctly", {
  x <- c(20, NA, 60)
  testthat::expect_warning(
    result <- compute_CEI(x, range = c(20, 60), na.rm = TRUE, return = "CEI")
  )
  testthat::expect_error(
    compute_CEI(x, range = c(20, 60), na.rm = FALSE, return = "CEI")
  )
})
