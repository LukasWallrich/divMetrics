#' Compute Rao's Quadratic Entropy (Rao-Simpson Index)
#'
#' Computes Rao's Quadratic Entropy for a numeric vector, optionally within groups.
#' Continuous data are discretized into bins (using bin_width or bins), and the index is calculated
#' based on the pairwise differences between bin midpoints weighted by the bin proportions.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param bin_width Positive numeric value specifying bin width.
#' @param bins Optional numeric vector of bin boundaries. Values are assigned to bins using left-inclusive, right-exclusive intervals (i.e., [a, b)).
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param verbose Logical. If TRUE, prints additional messages.
#'
#' @return A single Rao's Index value if group is NULL, or a named numeric vector with one value per group if group is provided.
#' @export
compute_Rao <- function(x, group = NULL, bin_width = NULL, bins = NULL, na.rm = FALSE, verbose = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  bin_details <- create_bins(x, bin_width = bin_width, bins = bins, return_midpoints = TRUE)
  breaks <- bin_details$breaks
  bin_midpoints <- bin_details$midpoints

  binned_data <- tibble::tibble(x = x, group = group) %>%
    dplyr::mutate(bin = cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = FALSE))

  compute_group_rao <- function(bin_counts, observed_bins) {
    total <- sum(bin_counts)
    if (total == 0) return(0)
    prop <- bin_counts / total
    obs_midpoints <- bin_midpoints[observed_bins]
    obs_dist_matrix <- as.matrix(stats::dist(obs_midpoints))
    sum(outer(prop, prop) * obs_dist_matrix)
  }

  if (is.null(group)) {
    bin_counts <- binned_data %>%
      dplyr::count(bin) %>%
      tidyr::complete(bin = seq_along(bin_midpoints), fill = list(n = 0)) %>%
      dplyr::pull(n)
    observed_bins <- which(bin_counts > 0)
    compute_group_rao(bin_counts[observed_bins], observed_bins)
  } else {
    binned_data %>%
      dplyr::group_by(group) %>%
      dplyr::count(bin) %>%
      tidyr::complete(bin = seq_along(bin_midpoints), fill = list(n = 0)) %>%
      dplyr::summarise(Rao_Index = {
        observed_bins <- which(n > 0)
        compute_group_rao(n[observed_bins], observed_bins)
      }, .groups = "drop") %>%
      tibble::deframe()
  }
}



#' Compute the Coverage and Evenness Index (CEI) or its Components
#'
#' Computes the Coverage and Evenness Index (CEI), or its components: Coverage (Coverage) and
#' Evenness Factor (E), for a numeric vector, optionally within groups. C is the proportion
#' of the theoretical range covered by the data, and
#' E is based on the uniformity of the distribution of values.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param range Numeric vector of length 2 specifying the theoretical min and max; defaults to the observed range.
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param return Character: "CEI" (default), "S", or "E".
#'
#' @return A single value if group is NULL, or a named numeric vector with one value per group if group is provided.
#' @export

compute_CEI <- function(x, group = NULL, range = NULL, na.rm = FALSE, return = "CEI") {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  if (!na.rm && any(is.na(x))) stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
  if (na.rm && any(is.na(x))) {
    keep <- !is.na(x)
    warning(sum(!keep), " missing values were removed.")
    x <- x[keep]
    if (!is.null(group)) group <- group[keep]
  }

  if (is.null(range)) {
    range <- range(x, na.rm = TRUE)
    message("Using observed range: c(", paste(range, collapse = ", "), ")")
  }
  if (length(range) != 2 || range[1] >= range[2])
    stop("`range` must be a numeric vector of length 2 with min < max.")

  range_min <- range[1]
  range_max <- range[2]
  total_range <- range_max - range_min

  compute_group_cei <- function(values) {
    n <- length(values)
    if (n < 2) return(list(CEI = 0, S = 0, E = 0))
    values <- sort(values)
    obs_range <- max(values) - min(values)
    C <- ifelse(total_range > 0, obs_range / total_range, 0)
    if (obs_range == 0) return(list(CEI = 0, S = C, E = 0))
    ideal <- seq(min(values), max(values), length.out = n)
    abs_dev <- sum(abs(values - ideal))
    max_dev <- (obs_range * (n - 1)) / 2
    E <- 1 - (abs_dev / max_dev)
    list(CEI = C * E, C = C, E = E)
  }

  if (is.null(group)) {
    res <- compute_group_cei(x)
  } else {
    res <- tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(res = list(compute_group_cei(x)), .groups = "drop") %>%
      tidyr::unnest_wider(res)
  }

  # Return requested component
  unlist(switch(return,
                "CEI" = res$CEI %>% setNames(res$group),
                "C" = res$C %>% setNames(res$group),
                "E" = res$E %>% setNames(res$group),
                stop("Invalid return value. Use 'CEI', 'C', or 'E'.")))
}


#' #' Compute the Even Spread Index (ESI)
#' #'
#' #' Computes the Even Spread Index (ESI) for a numeric vector. When a grouping variable is provided,
#' #' the index is computed within each group. The `range` parameter can be "observed" (to use the observed
#' #' min and max, but only when multiple groups are present) or a numeric vector of length 2 specifying
#' #' the theoretical minimum and maximum. When no grouping variable is provided, `range` must be numeric.
#' #'
#' #' @param x Numeric vector.
#' #' @param group Optional grouping variable of the same length as x. Required if range = "observed".
#' #' @param range Either "observed" or a numeric vector of length 2 with min < max.
#' #' @param na.rm Logical. If TRUE, NA values are removed.
#' #'
#' #' @return A single ESI value if group is NULL, or a numeric vector (one per group) if group is provided.
#' #' @export
#' compute_ESI <- function(x, group = NULL, range = "observed", na.rm = FALSE) {
#'   if (!is.numeric(x)) stop("`x` must be a numeric vector.")
#'
#'   if (!na.rm && any(is.na(x))) stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
#'   if (na.rm && any(is.na(x))) {
#'     keep <- !is.na(x)
#'     warning(sum(!keep), " missing values were removed.")
#'     x <- x[keep]
#'     if (!is.null(group)) group <- group[keep]
#'   }
#'
#'   if (is.character(range)) {
#'     if (range != "observed") stop("If `range` is a character, it must be 'observed'.")
#'     if (is.null(group)) {
#'       stop("When group is NULL, `range` must be a numeric vector; 'observed' range requires multiple groups.")
#'     }
#'     range_min <- min(x, na.rm = TRUE)
#'     range_max <- max(x, na.rm = TRUE)
#'   } else if (is.numeric(range)) {
#'     if (length(range) != 2 || range[1] >= range[2])
#'       stop("`range` must be a numeric vector of length 2 with min < max.")
#'     range_min <- range[1]
#'     range_max <- range[2]
#'   } else {
#'     stop("`range` must be either 'observed' or a numeric vector of length 2.")
#'   }
#'
#'   compute_group_esi <- function(values, range_min, range_max) {
#'     if (length(values) < 2) return(0)
#'     values <- sort(values)
#'     if (range_min == range_max) return(0)
#'     ideal_spread <- seq(range_min, range_max, length.out = length(values))
#'     asd <- mean(abs(values - ideal_spread))
#'     max_asd <- mean(abs(range_min - ideal_spread))
#'     1 - (asd + 1) / (max_asd + 1)
#'   }
#'
#'   if (is.null(group)) {
#'     compute_group_esi(x, range_min, range_max)
#'   } else {
#'     tibble::tibble(x = x, group = group) %>%
#'       dplyr::group_by(group) %>%
#'       dplyr::summarise(ESI = compute_group_esi(x, range_min, range_max), .groups = "drop") %>%
#'       dplyr::pull(ESI)
#'   }
#' }
