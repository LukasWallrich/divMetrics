library(divMetrics)

# ---- Tests for plot_metric_comparison ----

testthat::test_that("plot_metric_comparison creates valid plot", {
  testthat::skip_if_not_installed("ggplot2")

  metrics_df <- data.frame(
    team = rep(c("A", "B", "C"), each = 2),
    Blau = rep(c(0.5, 0.7, 0.3), each = 2),
    CV = rep(c(0.3, 0.5, 0.2), each = 2)
  )

  p <- plot_metric_comparison(metrics_df,
                               metrics = c("Blau", "CV"),
                               group_var = "team")
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_metric_comparison actually normalizes data", {
  testthat::skip_if_not_installed("ggplot2")

  # Create data with known values
  metrics_df <- data.frame(
    team = rep(c("A", "B"), each = 2),
    Metric1 = rep(c(10, 20), each = 2),
    Metric2 = rep(c(100, 200), each = 2)
  )

  # With normalize = TRUE, each metric should be divided by its own SD
  p_norm <- plot_metric_comparison(metrics_df,
                                    metrics = c("Metric1", "Metric2"),
                                    group_var = "team",
                                    normalize = TRUE)

  # Extract plotted data
  built_norm <- ggplot2::ggplot_build(p_norm)
  y_values_norm <- built_norm$data[[1]]$y

  # After normalization, largest value should be smaller than before
  # (since we're dividing by SD which is > 1 for this data)
  testthat::expect_true(max(y_values_norm) < 100)

  # With normalize = FALSE, should preserve original scales
  p_raw <- plot_metric_comparison(metrics_df,
                                  metrics = c("Metric1", "Metric2"),
                                  group_var = "team",
                                  normalize = FALSE)

  built_raw <- ggplot2::ggplot_build(p_raw)
  y_values_raw <- built_raw$data[[1]]$y

  # Raw data should contain large values from Metric2
  testthat::expect_true(max(y_values_raw) >= 100)
})

testthat::test_that("plot_metric_comparison plots correct groups", {
  testthat::skip_if_not_installed("ggplot2")

  metrics_df <- data.frame(
    team = rep(c("A", "B", "C"), each = 2),
    M1 = rep(c(0.5, 0.7, 0.3), each = 2),
    M2 = rep(c(0.3, 0.5, 0.2), each = 2),
    M3 = rep(c(0.4, 0.6, 0.25), each = 2)
  )

  p <- plot_metric_comparison(metrics_df,
                              metrics = c("M1", "M2", "M3"),
                              group_var = "team")

  built <- ggplot2::ggplot_build(p)
  plot_data <- built$data[[1]]  # geom_line data

  # Should have 3 unique groups (colors/lines)
  testthat::expect_equal(length(unique(plot_data$group)), 3)

  # Should have all 3 metrics represented on x-axis
  x_labels <- built$layout$panel_params[[1]]$x$get_labels()
  testthat::expect_equal(length(x_labels), 3)
  testthat::expect_true(all(c("M1", "M2", "M3") %in% x_labels))
})

testthat::test_that("plot_metric_comparison requires ggplot2", {
  # Should give informative error when ggplot2 not available
  # (This test only works if ggplot2 is actually missing, so we just
  # verify it works when ggplot2 IS available)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    test_df <- data.frame(
      team = c("A", "B"),
      Blau = c(0.5, 0.7)
    )
    testthat::expect_s3_class(
      plot_metric_comparison(test_df, metrics = "Blau", group_var = "team"),
      "ggplot"
    )
  }
})
