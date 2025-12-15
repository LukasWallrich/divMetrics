# Internal helper to create bin breaks and midpoints.
# Either 'bin_width' or 'bins' must be provided (not both).
# Bins are defined as left-inclusive, right-exclusive intervals.
# Returns a list with elements: breaks and midpoints.
create_bins <- function(x, bin_width = NULL, bins = NULL, return_midpoints = FALSE) {
  if (!is.null(bin_width) && !is.null(bins))
    stop("Specify either bin_width or bins, not both.")
  if (!is.null(bin_width)) {
    if (!is.numeric(bin_width) || length(bin_width) != 1 || bin_width <= 0)
      stop("`bin_width` must be a single positive numeric value.")
    global_min <- floor(min(x, na.rm = TRUE) / bin_width) * bin_width
    global_max <- ceiling(max(x, na.rm = TRUE) / bin_width) * bin_width
    breaks <- seq(from = global_min, to = global_max, by = bin_width)
  } else if (!is.null(bins)) {
    if (!is.numeric(bins) || length(bins) < 2)
      stop("`bins` must be a numeric vector with at least two values defining the bin boundaries.")
    breaks <- bins
  } else {
    stop("Either bin_width or bins must be provided.")
  }
  if (return_midpoints) {
    midpoints <- utils::head(breaks, -1) + diff(breaks) / 2
    list(breaks = breaks, midpoints = midpoints)
  } else {
    breaks
  }
}

# Internal helper for consistent NA removal.
remove_na <- function(x, group = NULL, na.rm = FALSE) {
  if (!is.null(group) && length(x) != length(group)) {
    stop("`x` and `group` must have the same length.")
  }
  if (!na.rm && any(is.na(x)))
    stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
  if (na.rm && any(is.na(x))) {
    keep <- !is.na(x)
    warning(sum(!keep), " missing values were removed.")
    x <- x[keep]
    if (!is.null(group)) group <- group[keep]
  }
  list(x = x, group = group)
}

#' Report Teams
#'
#' Combines attribute values by team, maintaining the order of first appearance.
#'
#' @param attribute A vector of attribute values.
#' @param team A vector indicating the team for each attribute.
#'
#' @return A named character vector where each element is a comma-separated string of attribute values for a team.
#' The names correspond to the teams.
#'
#' @examples
#' report_teams(c("A", "B", "C", "A", "A", "A"), c(1,1,1,2,2,2))
#' report_teams(c(1,2,3,4), c(2,2,1,1))
#' @keywords internal
#' @noRd

report_teams <- function(attribute, team) {
  if (length(attribute) != length(team)) {
    stop("`attribute` and `team` must have the same length.")
  }

  if (!is.factor(team))  {
   ord <- unique(team)
  } else {
    ord <- levels(team)
  }
  stats::setNames(sapply(ord, function(t) paste(attribute[team == t], collapse = ", ")), ord)
}

#' Parse Line to Vector
#'
#' Converts a comma-separated string into a numeric vector.
#' This is a simplified replacement for timesaveR::line_to_vector().
#'
#' @param x A character string with comma-separated values.
#' @param return A character string indicating what to return. Only
#'   "vector" is supported.
#'
#' @return A numeric vector of the parsed values.
#'
#' @source https://lukaswallrich.github.io/timesaveR/
#'
#' @keywords internal
line_to_vector <- function(x, return = "vector") {
  if (return != "vector") {
    stop("Only return = 'vector' is currently supported.")
  }
  # Split by comma and convert to numeric
  as.numeric(trimws(strsplit(x, ",")[[1]]))
}

#' Correlation Matrix
#'
#' Creates a correlation matrix from numeric data. This is a simplified
#' replacement for timesaveR::cor_matrix().
#'
#' @param x A data frame with numeric columns.
#'
#' @return A correlation matrix object with additional attributes for
#'   formatting.
#'
#' @source https://lukaswallrich.github.io/timesaveR/
#' 
#' @keywords internal
cor_matrix <- function(x) {
  # Calculate correlation matrix
  cor_mat <- stats::cor(x, use = "complete.obs")

  # Store the original data for later use in report_cor_table
  attr(cor_mat, "data") <- x
  class(cor_mat) <- c("cor_matrix", "matrix")

  cor_mat
}

## Report Correlation Table (gt)
#'
#' Report Correlation Table
#'
#' Creates a compact, APA-style correlation table with descriptive statistics
#' (M, SD), lower-triangular correlations, 95% CIs, and significance stars,
#' returned as a formatted `gt` table. Simplified replacement for
#' `timesaveR::report_cor_table()`.
#'
#' @param x A correlation matrix produced by `cor_matrix()` (must carry the
#'   original data in attribute `"data"`).
#'
#' @return A `gt_tbl` object. If the `gt` package is not installed, an error is
#'   raised with an instruction to install it.
#'
#' @source https://lukaswallrich.github.io/timesaveR/
#'
#' @keywords internal
report_cor_table <- function(x) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required to format the correlation table. Please install it with install.packages('gt').")
  }

  data <- attr(x, "data")
  if (is.null(data)) {
    stop("Correlation matrix must have 'data' attribute. Use cor_matrix() to create it.")
  }

  # Descriptives and meta
  means <- colMeans(data, na.rm = TRUE)
  sds <- apply(data, 2, stats::sd, na.rm = TRUE)
  n <- nrow(data)
  var_names <- colnames(x)
  n_vars <- length(var_names)

  # Prepare table shell
  output <- data.frame(
    Variable = paste0(seq_len(n_vars), ". ", var_names),
    M = as.numeric(means),
    SD = as.numeric(sds),
    stringsAsFactors = FALSE
  )

  # Fill lower-triangular correlations with CI and sig
  for (j in seq_len(n_vars)) {
    col_vals <- character(n_vars)
    for (i in seq_len(n_vars)) {
      if (i == j) {
        col_vals[i] <- "-"
      } else if (i < j) {
        # upper triangle -> leave blank
        col_vals[i] <- ""
      } else {
        r <- x[i, j]
        r_clamped <- max(min(r, 0.999999), -0.999999)
        z <- 0.5 * log((1 + r_clamped) / (1 - r_clamped))
        se_z <- 1 / sqrt(max(n - 3, 1))
        ci_lower <- (exp(2 * (z - 1.96 * se_z)) - 1) / (exp(2 * (z - 1.96 * se_z)) + 1)
        ci_upper <- (exp(2 * (z + 1.96 * se_z)) - 1) / (exp(2 * (z + 1.96 * se_z)) + 1)

        # p-value (guard against divide-by-zero)
        denom <- sqrt(max(1 - r_clamped^2, .Machine$double.eps))
        t_stat <- r_clamped * sqrt(max(n - 2, 1)) / denom
        p_value <- 2 * (1 - stats::pt(abs(t_stat), df = max(n - 2, 1)))
        sig <- ifelse(p_value < 0.001, "***",
                 ifelse(p_value < 0.01,  "**",
                 ifelse(p_value < 0.05,  "*",  "")))

        col_vals[i] <- paste0(
          sprintf("%.2f", r), " [",
          sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]",
          sig
        )
      }
    }
    output[[as.character(j)]] <- col_vals
  }

  # Build gt table
  center_cols <- setdiff(colnames(output), "Variable")
  gt_tbl <- gt::gt(output) |>
    gt::fmt_number(columns = c("M", "SD"), decimals = 2) |>
    gt::cols_align(align = "left", columns = "Variable") |>
    gt::cols_align(align = "center", columns = center_cols) |>
    gt::tab_options(
      table.font.size = gt::px(13)
    ) |>
    gt::tab_source_note(
      source_note = gt::md("Note. Values are Pearson correlations with 95% CIs in brackets. *** p < .001, ** p < .01, * p < .05.")
    )

  gt_tbl
}
