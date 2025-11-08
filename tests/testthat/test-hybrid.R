library(divMetrics)

## ---- Tests for compute_Rao (distance matrix form) ----

testthat::test_that("compute_Rao matches p' D p for simple case", {
  # Categories A,B with distance 2 between them
  D <- matrix(c(0,2,2,0), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("A","B")))
  x <- c("A","A","B","B")
  # p = (0.5, 0.5); Q = 0.5*0.5*2 + 0.5*0.5*2 = 1
  result <- compute_Rao(x, D = D, na.rm = TRUE)
  testthat::expect_equal(result, 1)
})

testthat::test_that("compute_Rao works with groups and dist objects", {
  D <- matrix(c(0,1,1,0), nrow = 2, byrow = TRUE,
              dimnames = list(c("X","Y"), c("X","Y")))
  Dd <- as.dist(D)
  x <- c("X","X","Y","Y","Y","X")
  g <- c("A","A","A","B","B","B")
  res <- compute_Rao(x, group = g, D = Dd, na.rm = TRUE)
  testthat::expect_equal(names(res), c("A","B"))
  # Basic sanity: values are numeric and non-negative
  testthat::expect_true(all(res >= 0))
})

testthat::test_that("compute_Rao validates categories and symmetry", {
  D <- matrix(c(0,1,2,0), nrow = 2) # no dimnames and not symmetric
  testthat::expect_error(compute_Rao(c("A","B"), D = D), "must have row and column names|must be symmetric")

  D2 <- matrix(c(0,1,1,0), nrow = 2, byrow = TRUE,
               dimnames = list(c("A","B"), c("A","B")))
  testthat::expect_error(
    compute_Rao(c("A","C"), D = D2),
    "not present in `D`"
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
