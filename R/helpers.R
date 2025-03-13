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
