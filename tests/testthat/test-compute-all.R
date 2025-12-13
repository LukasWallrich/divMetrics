library(divMetrics)

# ---- Tests for compute_all_metrics ----

testthat::test_that("compute_all_metrics works with continuous method", {
  # Create test data
  x <- c(20, 25, 30, 35, 40, 45, 50, 55)
  group <- c(rep("A", 4), rep("B", 4))

  # Test continuous method with binning for Blau
  result <- compute_all_metrics(
    x,
    group = group,
    method = "continuous",
    bin_width = 10,
    return_df = FALSE
  )

  testthat::expect_type(result, "list")
  testthat::expect_length(result, 5)  # 5 metrics for continuous with Blau
  testthat::expect_named(result, c("CV", "SD", "GMD", "Blau", "CEI"))

  # Check that each metric is a named vector
  for(metric in result) {
    testthat::expect_type(metric, "double")
    testthat::expect_length(metric, 2)
    testthat::expect_named(metric, c("A", "B"))
  }
})

testthat::test_that("compute_all_metrics handles different return formats", {
  x <- c(20, 25, 30, 35)
  group <- c("A", "A", "B", "B")

  # Test return_df = FALSE (returns list)
  result_list <- compute_all_metrics(
    x, group = group, method = "continuous", return_df = FALSE
  )
  testthat::expect_type(result_list, "list")

  # Test return_df = TRUE (returns dataframe)
  result_df <- compute_all_metrics(
    x, group = group, method = "continuous", return_df = TRUE
  )
  testthat::expect_s3_class(result_df, "data.frame")
  testthat::expect_true(all(c("group", "CV", "SD") %in% names(result_df)))
})

testthat::test_that("compute_all_metrics handles binning parameters for Blau", {
  x <- c(1, 2, 3, 4, 5, 6)
  group <- rep("A", 6)

  # With bin_width
  result_bw <- compute_all_metrics(
    x,
    group = group,
    method = "continuous",
    bin_width = 2
  )
  testthat::expect_type(result_bw$Blau, "double")

  # With explicit bins
  result_bins <- compute_all_metrics(
    x,
    group = group,
    method = "continuous",
    bins = c(0, 2, 4, 6, 8)
  )
  testthat::expect_type(result_bins$Blau, "double")
})

testthat::test_that("compute_all_metrics handles range parameter for CEI", {
  x <- c(20, 25, 30, 35)
  group <- rep("A", 4)

  # With specified range
  result <- compute_all_metrics(
    x,
    group = group,
    method = "continuous",
    range = c(0, 100)
  )
  testthat::expect_type(result$CEI, "double")
  testthat::expect_true(result$CEI >= 0 && result$CEI <= 1)
})

testthat::test_that("compute_all_metrics handles categorical method error", {
  x <- c("A", "B", "C", "D")
  group <- rep("Team", 4)

  # Categorical method is not yet implemented
  testthat::expect_error(
    compute_all_metrics(x, group = group, method = "categorical"),
    "Categorical method not yet implemented"
  )
})

testthat::test_that("compute_all_metrics handles categorical method with distance matrix", {
  # Skip this test for now as categorical method is not fully implemented
  # in compute_all_metrics (based on the code review)
  testthat::skip("Categorical method in compute_all_metrics not fully implemented")
})

testthat::test_that("compute_all_metrics handles single group", {
  x <- c(20, 25, 30, 35)

  # Single group (no group parameter)
  result <- compute_all_metrics(x, method = "continuous", return_df = FALSE)

  testthat::expect_type(result, "list")
  testthat::expect_length(result, 4)  # CV, SD, GMD, CEI (no Blau without binning)

  # Each metric should return a single value
  for(metric in result) {
    testthat::expect_type(metric, "double")
    testthat::expect_length(metric, 1)
  }
})

testthat::test_that("compute_all_metrics handles edge cases", {
  # Single value - need to provide range for CEI since single value has no spread
  result_single <- compute_all_metrics(5, method = "continuous", range = c(0, 10))
  testthat::expect_type(result_single, "list")

  # With NA values - suppressWarnings because NA handling generates multiple warnings
  x_na <- c(20, 25, NA, 35)
  result_na <- suppressWarnings(
    compute_all_metrics(x_na, na.rm = TRUE, method = "continuous")
  )
  testthat::expect_type(result_na, "list")
})

testthat::test_that("compute_all_metrics validates inputs", {
  x <- c(20, 25, 30)

  # Invalid method
  testthat::expect_error(
    compute_all_metrics(x, method = "invalid"),
    "'arg' should be one of"
  )
})