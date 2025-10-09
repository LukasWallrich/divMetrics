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
