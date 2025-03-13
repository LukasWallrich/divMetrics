#' Compute Blau's Index
#'
#' Computes Blau's Index for a numeric vector, optionally within groups. Continuous values
#' can be binned either by specifying a bin width or explicit bin boundaries.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param bin_width Positive numeric value specifying bin width.
#' @param bins Optional numeric vector of bin boundaries. Values are assigned to bins using left-inclusive, right-exclusive intervals (i.e., first interval [a, b) does not include b).#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param verbose Logical. If TRUE, prints the bin breaks.
#'
#' @return A single Blau's Index value if group is NULL, or a named numeric vector with one value per group if group is provided.
#' @export
compute_Blau <- function(x, group = NULL, bin_width = NULL, bins = NULL, na.rm = FALSE, verbose = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  breaks <- create_bins(x, bin_width = bin_width, bins = bins, return_midpoints = FALSE)

  if (verbose) {
    message("Bin breaks: ", paste(breaks, collapse = ", "))
  }

  binned_data <- tibble::tibble(x = x, group = group) %>%
    dplyr::mutate(bin = cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = FALSE))

  if (any(is.na(binned_data$bin)))
    stop("Error in categorizing data – missing values introduced. Check input data.")

  compute_group_blau <- function(bin_counts) {
    prop <- bin_counts / sum(bin_counts)
    1 - sum(prop^2)
  }

  if (is.null(group)) {
    bin_counts <- binned_data %>% dplyr::count(bin) %>% dplyr::pull(n)
    compute_group_blau(bin_counts)
  } else {
    binned_data %>%
      dplyr::group_by(group) %>%
      dplyr::count(bin) %>%
      dplyr::summarise(Blau_Index = compute_group_blau(n), .groups = "drop") %>%
      tibble::deframe()
  }
}
