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
#' @param return_df Logical. If TRUE, returns a dataframe with group, group_members and index_value.
#'
#' @return A named numeric vector with one value per group if group is provided (or a single value), unless return_df is `TRUE`.
#' @export
compute_Blau <- function(x, group = NULL, bin_width = NULL, bins = NULL, na.rm = FALSE, verbose = FALSE, return_df = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

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
    res <- compute_group_blau(bin_counts)
  } else {
    res <- binned_data %>%
      dplyr::group_by(group) %>%
      dplyr::count(bin) %>%
      dplyr::summarise(Blau_Index = compute_group_blau(n), .groups = "drop") %>%
      tibble::deframe()
  }
  if (return_df) {
    group_members <- report_teams(x, group)
    tibble::tibble(group = names(res), group_members = group_members, index_value = res)
  } else {
    res
  }

}
