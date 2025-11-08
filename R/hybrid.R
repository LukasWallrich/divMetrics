#' Compute Rao's Quadratic Entropy (Rao-Simpson Index)
#'
#' Computes Rao's Quadratic Entropy for a numeric vector, optionally within groups.
#' Continuous data are discretized into bins (using bin_width or bins), and the index is calculated
#' based on the pairwise differences between bin midpoints weighted by the bin proportions.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x. Will be converted to a factor if not already.
#' @param bin_width Positive numeric value specifying bin width.
#' @param bins Optional numeric vector of bin boundaries. Values are assigned to bins using left-inclusive, right-exclusive intervals (i.e., [a, b)).
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param verbose Logical. If TRUE, prints additional messages.
#' @param return_df Logical. If TRUE, returns a dataframe with group, group_members and index_value.
#'
#' @return A single Rao's Index value if group is NULL, or a named numeric vector with one value per group if group is provided (or a data frame if return_df is TRUE).
#' @export
compute_Rao <- function(x, group = NULL, bin_width = NULL, bins = NULL, na.rm = FALSE, verbose = FALSE, return_df = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

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
    res <- compute_group_rao(bin_counts[observed_bins], observed_bins)
  } else {
    res <- binned_data %>%
      dplyr::group_by(group) %>%
      dplyr::count(bin) %>%
      tidyr::complete(bin = seq_along(bin_midpoints), fill = list(n = 0)) %>%
      dplyr::summarise(Rao_Index = {
        observed_bins <- which(n > 0)
        compute_group_rao(n[observed_bins], observed_bins)
      }, .groups = "drop") %>%
      tibble::deframe()
  }

  if (return_df) {
    if (is.null(group)) {
      tibble::tibble(
        group = NA_character_,
        group_members = paste(x, collapse = ", "),
        index_value = res
      )
    } else {
      group_members <- report_teams(x, group)
      tibble::tibble(
        group = names(res),
        group_members = unname(group_members[names(res)]),
        index_value = unname(res)
      )
    }
  } else {
    res
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
#' @param return Character: "CEI" (default), "C", or "E". The deprecated alias "S" maps to "C".
#' @param verbose Logical. If TRUE, prints the range used.
#' @param return_df Logical. If TRUE, returns a dataframe with group, group_members and index_value.
#'
#' @return A named numeric vector with one value per group if group is provided (or a single value), unless return_df is `TRUE`.
#' @export

compute_CEI <- function(x, group = NULL, range = NULL, na.rm = FALSE, return = "CEI", verbose = FALSE, return_df = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (!is.null(group) && length(x) != length(group)) {
    stop("`x` and `group` must have the same length.")
  }

  if (!na.rm && any(is.na(x))) stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
  if (na.rm && any(is.na(x))) {
    keep <- !is.na(x)
    warning(sum(!keep), " missing values were removed.")
    x <- x[keep]
    if (!is.null(group)) group <- group[keep]
  }

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

  if (is.null(range)) {
    range <- range(x, na.rm = TRUE)
    if (verbose) {
      message("Using observed range: c(", paste(range, collapse = ", "), ")")
    }
  } else if (verbose) {
    message("Using specified range: c(", paste(range, collapse = ", "), ")")
  }
  if (length(range) != 2 || range[1] >= range[2])
    stop("`range` must be a numeric vector of length 2 with min < max.")

  range_min <- range[1]
  range_max <- range[2]
  total_range <- range_max - range_min

  compute_group_cei <- function(values) {
    n <- length(values)
    if (n < 2) return(list(CEI = 0, C = 0, E = 0))
    values <- sort(values)
    obs_range <- max(values) - min(values)
    C <- ifelse(total_range > 0, obs_range / total_range, 0)
    if (obs_range == 0) return(list(CEI = 0, C = C, E = 0))
    ideal <- seq(min(values), max(values), length.out = n)
    abs_dev <- sum(abs(values - ideal))
    max_dev <- (obs_range * (n - 1)) / 2
    E <- 1 - (abs_dev / max_dev)
    list(CEI = C * E, C = C, E = E)
  }

  return <- toupper(return)
  if (return == "S") {
    warning("`return = \"S\"` is deprecated; use \"C\" instead.", call. = FALSE)
    return <- "C"
  }
  if (!return %in% c("CEI", "C", "E")) {
    stop("Invalid return value. Use 'CEI', 'C', or 'E'.")
  }

  if (is.null(group)) {
    metrics <- compute_group_cei(x)
    component_value <- metrics[[return]]
    if (return_df) {
      tibble::tibble(
        group = NA_character_,
        group_members = paste(x, collapse = ", "),
        index_value = component_value
      )
    } else {
      component_value
    }
  } else {
    res <- tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(metrics = list(compute_group_cei(x)), .groups = "drop") %>%
      tidyr::unnest_wider(metrics)

    component_values <- res[[return]]
    names(component_values) <- res$group

    if (return_df) {
      group_members <- report_teams(x, group)
      tibble::tibble(
        group = res$group,
        group_members = unname(group_members[res$group]),
        index_value = component_values
      )
    } else {
      component_values
    }
  }

}
