library(divMetrics)

# ---- Tests for input validation across all functions ----

testthat::test_that("All functions reject non-numeric x", {
  testthat::expect_error(compute_cv(c("a", "b", "c")), "`x` must be a numeric vector")
  testthat::expect_error(compute_sd(c("a", "b", "c")), "`x` must be a numeric vector")
  testthat::expect_error(compute_Blau(c("a", "b", "c"), bin_width = 1), "`x` must be a numeric vector")
  testthat::expect_error(compute_Rao(c("a", "b", "c"), bin_width = 1), "`x` must be a numeric vector")
  testthat::expect_error(compute_CEI(c("a", "b", "c"), range = c(0, 10)), "`x` must be a numeric vector")
})

testthat::test_that("All functions reject mismatched x and group lengths", {
  testthat::expect_error(compute_cv(c(1, 2, 3), group = c(1, 2)), "`x` and `group` must have the same length")
  testthat::expect_error(compute_sd(c(1, 2, 3), group = c(1, 2)), "`x` and `group` must have the same length")
  testthat::expect_error(compute_Blau(c(1, 2, 3), group = c(1, 2), bin_width = 1), "`x` and `group` must have the same length")
  testthat::expect_error(compute_Rao(c(1, 2, 3), group = c(1, 2), bin_width = 1), "`x` and `group` must have the same length")
  testthat::expect_error(compute_CEI(c(1, 2, 3), group = c(1, 2), range = c(0, 10)), "`x` and `group` must have the same length")
})

testthat::test_that("All functions error on NA without na.rm", {
  testthat::expect_error(compute_cv(c(1, NA, 3), na.rm = FALSE), "contains missing values")
  testthat::expect_error(compute_sd(c(1, NA, 3), na.rm = FALSE), "contains missing values")
  testthat::expect_error(compute_Blau(c(1, NA, 3), bin_width = 1, na.rm = FALSE), "contains missing values")
  testthat::expect_error(compute_Rao(c(1, NA, 3), bin_width = 1, na.rm = FALSE), "contains missing values")
  testthat::expect_error(compute_CEI(c(1, NA, 3), range = c(0, 10), na.rm = FALSE), "contains missing values")
})

testthat::test_that("All functions warn and remove NA with na.rm = TRUE", {
  testthat::expect_warning(compute_cv(c(1, NA, 3), na.rm = TRUE), "missing values were removed")
  testthat::expect_warning(compute_sd(c(1, NA, 3), na.rm = TRUE), "missing values were removed")
  testthat::expect_warning(compute_Blau(c(1, NA, 3), bin_width = 1, na.rm = TRUE), "missing values were removed")
  testthat::expect_warning(compute_Rao(c(1, NA, 3), bin_width = 1, na.rm = TRUE), "missing values were removed")
  testthat::expect_warning(compute_CEI(c(1, NA, 3), range = c(0, 10), na.rm = TRUE), "missing values were removed")
})

# ---- Tests for binning parameter validation ----

testthat::test_that("Binning functions reject both bin_width and bins", {
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bin_width = 1, bins = c(0, 2, 4)),
    "Specify either bin_width or bins, not both"
  )
  testthat::expect_error(
    compute_Rao(c(1, 2, 3), bin_width = 1, bins = c(0, 2, 4)),
    "Specify either bin_width or bins, not both"
  )
})

testthat::test_that("Binning functions require either bin_width or bins", {
  testthat::expect_error(
    compute_Blau(c(1, 2, 3)),
    "Either bin_width or bins must be provided"
  )
  testthat::expect_error(
    compute_Rao(c(1, 2, 3)),
    "Either bin_width or bins must be provided"
  )
})

testthat::test_that("bin_width must be positive numeric", {
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bin_width = -1),
    "must be a single positive numeric value"
  )
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bin_width = 0),
    "must be a single positive numeric value"
  )
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bin_width = c(1, 2)),
    "must be a single positive numeric value"
  )
})

testthat::test_that("bins must be numeric vector with at least 2 values", {
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bins = c(0)),
    "at least two values"
  )
  testthat::expect_error(
    compute_Blau(c(1, 2, 3), bins = "bad"),
    "must be a numeric vector"
  )
})

# ---- Tests for CEI-specific validation ----

testthat::test_that("CEI rejects invalid range parameter", {
  testthat::expect_error(
    compute_CEI(c(1, 2, 3), range = c(10, 5)),
    "`range` must be a numeric vector of length 2 with min < max"
  )
  testthat::expect_error(
    compute_CEI(c(1, 2, 3), range = c(5)),
    "`range` must be a numeric vector of length 2 with min < max"
  )
  testthat::expect_error(
    compute_CEI(c(1, 2, 3), range = c(5, 5)),
    "`range` must be a numeric vector of length 2 with min < max"
  )
})

testthat::test_that("CEI rejects invalid return parameter", {
  testthat::expect_error(
    compute_CEI(c(1, 2, 3), range = c(0, 10), return = "bad"),
    "Invalid return value"
  )
})

# ---- Tests for compute_cv specific validation ----

testthat::test_that("compute_cv warns when mean is zero", {
  testthat::expect_warning(
    result <- compute_cv(c(0, 0, 0), warn_zero_mean = TRUE),
    "Mean is zero; CV is undefined"
  )
  testthat::expect_true(is.na(result))
})

testthat::test_that("compute_cv can suppress zero mean warning", {
  testthat::expect_silent(
    result <- compute_cv(c(0, 0, 0), warn_zero_mean = FALSE)
  )
  testthat::expect_true(is.na(result))
})
