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
    midpoints <- head(breaks, -1) + diff(breaks) / 2
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

report_teams <- function(attribute, team) {
  if (length(attribute) != length(team)) {
    stop("`attribute` and `team` must have the same length.")
  }

  if (!is.factor(team))  {
   ord <- unique(team)
  } else {
    ord <- levels(team)
  }
  setNames(sapply(ord, function(t) paste(attribute[team == t], collapse = ", ")), ord)
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
  cor_mat <- cor(x, use = "complete.obs")

  # Store the original data for later use in report_cor_table
  attr(cor_mat, "data") <- x
  class(cor_mat) <- c("cor_matrix", "matrix")

  cor_mat
}

#' Report Correlation Table
#'
#' Creates an APA-formatted correlation table with descriptive statistics,
#' correlations, confidence intervals, and significance tests. This is a
#' simplified replacement for timesaveR::report_cor_table().
#'
#' @param x A correlation matrix (result from cor_matrix()).
#'
#' @return Prints a formatted correlation table and returns invisibly.
#'
#' @source https://lukaswallrich.github.io/timesaveR/
#'
#' @keywords internal
report_cor_table <- function(x) {
  # Get the original data if available
  data <- attr(x, "data")

  if (is.null(data)) {
    stop(
      "Correlation matrix must have 'data' attribute. ",
      "Use cor_matrix() to create it."
    )
  }

  # Compute descriptive statistics
  means <- colMeans(data, na.rm = TRUE)
  sds <- apply(data, 2, sd, na.rm = TRUE)
  n <- nrow(data)

  # Get variable names
  var_names <- colnames(x)
  n_vars <- length(var_names)

  # Initialize output table
  output <- data.frame(
    Variable = var_names,
    M = sprintf("%.2f", means),
    SD = sprintf("%.2f", sds),
    stringsAsFactors = FALSE
  )

  # Add correlation columns with CIs and significance
  for (i in 1:n_vars) {
    col_name <- as.character(i)
    col_data <- character(n_vars)

    for (j in 1:n_vars) {
      r <- x[i, j]

      # Handle diagonal (r = 1)
      if (i == j) {
        col_data[j] <- "-"
      } else if (i > j) {
        # Lower triangle (already calculated above)
        col_data[j] <- ""
      } else {
        # Calculate confidence intervals using Fisher's z transformation
        z <- 0.5 * log((1 + r) / (1 - r))
        se_z <- 1 / sqrt(n - 3)
        ci_lower <- (exp(2 * (z - 1.96 * se_z)) - 1) /
          (exp(2 * (z - 1.96 * se_z)) + 1)
        ci_upper <- (exp(2 * (z + 1.96 * se_z)) - 1) /
          (exp(2 * (z + 1.96 * se_z)) + 1)

        # Calculate p-value
        t_stat <- r * sqrt(n - 2) / sqrt(1 - r^2)
        p_value <- 2 * (1 - pt(abs(t_stat), n - 2))

        # Add significance stars
        sig <- ifelse(p_value < 0.001, "***",
          ifelse(p_value < 0.01, "**",
            ifelse(p_value < 0.05, "*", "")))

        # Store formatted value
        col_data[j] <- paste0(
          sprintf("%.2f", r), " [",
          sprintf("%.2f", ci_lower), ", ",
          sprintf("%.2f", ci_upper), "]",
          sig
        )
      }
    }

    output[[col_name]] <- col_data
  }

  # Rename numeric columns to variable numbers for the table
  names(output)[-(1:3)] <- as.character(1:n_vars)

  # Print header information
  cat("Correlation Table with Confidence Intervals (95%)\n")
  cat("Note: *** p < .001, ** p < .01, * p < .05\n\n")

  # Print the table
  print(output, quote = FALSE, right = TRUE)

  # Return invisibly for chaining
  invisible(output)
}
