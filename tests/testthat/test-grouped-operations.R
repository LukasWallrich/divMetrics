library(divMetrics)

# ---- Tests for grouped calculations ----

testthat::test_that("compute_cv works with groups", {
  x <- c(1, 2, 3, 10, 20, 30)
  group <- c("A", "A", "A", "B", "B", "B")

  result <- compute_cv(x, group = group)

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))
  testthat::expect_true(all(!is.na(result)))

  # Verify values match ungrouped calculation
  testthat::expect_equal(unname(result["A"]), compute_cv(c(1, 2, 3)), tolerance = 1e-6)
  testthat::expect_equal(unname(result["B"]), compute_cv(c(10, 20, 30)), tolerance = 1e-6)
})

testthat::test_that("compute_sd works with groups", {
  x <- c(1, 2, 3, 10, 20, 30)
  group <- c("A", "A", "A", "B", "B", "B")

  result <- compute_sd(x, group = group)

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))

  # Verify values
  testthat::expect_equal(unname(result["A"]), stats::sd(c(1, 2, 3)))
  testthat::expect_equal(unname(result["B"]), stats::sd(c(10, 20, 30)))
})

testthat::test_that("compute_Blau works with groups", {
  x <- c(1, 1.5, 3, 3.5, 5, 5.5, 7, 7.5)
  group <- c("A", "A", "A", "A", "B", "B", "B", "B")

  result <- compute_Blau(x, group = group, bins = c(0, 2, 4, 6, 8))

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))

  # Each group has values in two bins with equal proportions
  testthat::expect_equal(unname(result["A"]), 0.5)
  testthat::expect_equal(unname(result["B"]), 0.5)
})

testthat::test_that("compute_Rao works with groups", {
  x <- c(1, 1, 3, 3, 5, 5, 7, 7)
  group <- c("A", "A", "A", "A", "B", "B", "B", "B")

  result <- compute_Rao(x, group = group, bins = c(0, 2, 4, 6, 8))

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))
  testthat::expect_true(all(!is.na(result)))
})

testthat::test_that("compute_CEI works with groups", {
  x <- c(20, 40, 60, 25, 45, 65)
  group <- c("A", "A", "A", "B", "B", "B")

  result <- compute_CEI(x, group = group, range = c(20, 70))

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))
  testthat::expect_true(all(!is.na(result)))
})

# ---- Tests for group ordering ----

testthat::test_that("Group ordering is preserved in results", {
  x <- c(1, 2, 3, 4, 5, 6)
  # Non-alphabetic order
  group <- c("Z", "Z", "A", "A", "M", "M")

  result <- compute_sd(x, group = group)

  # Names should be in order of first appearance, not alphabetic
  testthat::expect_equal(names(result), c("Z", "A", "M"))
})

testthat::test_that("Factor levels are respected for group ordering", {
  x <- c(1, 2, 3, 4, 5, 6)
  group <- factor(c("Z", "Z", "A", "A", "M", "M"), levels = c("A", "M", "Z"))

  result <- compute_sd(x, group = group)

  # Should follow factor level order
  testthat::expect_equal(names(result), c("A", "M", "Z"))
})

# ---- Tests for NA handling with groups ----

testthat::test_that("NA removal works correctly with groups", {
  x <- c(1, 2, NA, 10, 20, NA)
  group <- c("A", "A", "A", "B", "B", "B")

  testthat::expect_warning(
    result <- compute_cv(x, group = group, na.rm = TRUE),
    "2 missing values were removed"
  )

  testthat::expect_length(result, 2)
  testthat::expect_named(result, c("A", "B"))
})

# ---- Tests for return_df parameter (where available) ----

testthat::test_that("compute_Blau return_df works with groups", {
  x <- c(1, 1.5, 3, 3.5)
  group <- c("A", "A", "B", "B")

  result <- compute_Blau(x, group = group, bins = c(0, 2, 4), return_df = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(ncol(result), 3)
  testthat::expect_named(result, c("group", "group_members", "index_value"))
  testthat::expect_equal(nrow(result), 2)
})

testthat::test_that("compute_CEI return_df works with groups", {
  x <- c(20, 40, 60, 25, 45, 65)
  group <- c("A", "A", "A", "B", "B", "B")

  result <- compute_CEI(x, group = group, range = c(20, 70), return_df = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(ncol(result), 3)
  testthat::expect_named(result, c("group", "group_members", "index_value"))
  testthat::expect_equal(nrow(result), 2)
})

# ---- Tests for single group edge case ----

testthat::test_that("Single group works (equivalent to ungrouped)", {
  x <- c(1, 2, 3, 4, 5)
  group <- rep("A", 5)

  result_grouped <- compute_cv(x, group = group)
  result_ungrouped <- compute_cv(x)

  testthat::expect_equal(as.numeric(result_grouped), result_ungrouped)
})
