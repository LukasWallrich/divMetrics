## Rao (distance-matrix only)
#' Compute Rao's Quadratic Entropy (distance-matrix form)
#'
#' Computes Rao's Quadratic Entropy for a categorical vector using a provided
#' dissimilarity matrix D. For each group, the category proportions p are
#' computed and Q = p' D p is returned.
#'
#' This variant does not accept binning for continuous data. For continuous
#' attributes, use `compute_GMD()` instead.
#'
#' @param x Vector of categories (character or factor). Values must be present
#'   in the row/column names of `D`.
#' @param group Optional grouping variable of the same length as x. Will be
#'   converted to a factor if not already.
#' @param D A symmetric distance/dissimilarity matrix (or a `dist` object)
#'   whose row/column names define the category universe.
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param return_df Logical. If TRUE, returns a dataframe with group,
#'   group_members and index_value.
#'
#' @return A single Rao's Index value if group is NULL, or a named numeric
#'   vector with one value per group if group is provided (or a data frame if
#'   return_df is TRUE).
#' @export
compute_Rao <- function(x, group = NULL, D, na.rm = FALSE, return_df = FALSE) {
  if (missing(D)) stop("`D` (distance matrix) must be provided.")

  # Accept dist or matrix
  if (inherits(D, "dist")) {
    D <- as.matrix(D)
  }
  if (!is.matrix(D)) stop("`D` must be a matrix or a dist object.")
  if (is.null(rownames(D)) || is.null(colnames(D)))
    stop("`D` must have row and column names.")
  if (!all(rownames(D) == colnames(D)))
    stop("Row and column names of `D` must match and be in the same order.")
  if (!isSymmetric(D, tol = 1e-10))
    stop("`D` must be symmetric.")

  # x can be any atomic vector interpreted as categories
  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

  cats <- rownames(D)
  x_chr <- as.character(x)
  if (!all(unique(x_chr) %in% cats)) {
    missing_cats <- setdiff(unique(x_chr), cats)
    stop("The following categories in `x` are not present in `D`: ", paste(missing_cats, collapse = ", "))
  }

  compute_group_rao <- function(values_chr) {
    n <- length(values_chr)
    if (n == 0) return(0)
    counts <- table(factor(values_chr, levels = cats))
    p <- as.numeric(counts) / sum(counts)
    as.numeric(t(p) %*% D %*% p)
  }

  if (is.null(group)) {
    res <- compute_group_rao(x_chr)
  } else {
    res <- tibble::tibble(x = x_chr, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(Rao_Index = compute_group_rao(x), .groups = "drop") %>%
      tibble::deframe()
  }

  if (return_df) {
    if (is.null(group)) {
      tibble::tibble(
        group = NA_character_,
        group_members = paste(x_chr, collapse = ", "),
        index_value = res
      )
    } else {
      group_members <- report_teams(x_chr, group)
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
#' @param return Character: "CEI" (default), "C", or "E".
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

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

  if (is.null(range)) {
    range <- base::range(x)
    if (diff(range) == 0) {
      warning("Observed range is zero; CEI components will be zero.")
      if (is.null(group)) {
        res <- 0
      } else {
        res <- stats::setNames(rep(0, length(levels(group))), levels(group))
      }
      if (return_df) {
        if (is.null(group)) {
          return(tibble::tibble(
            group = NA_character_,
            group_members = paste(x, collapse = ", "),
            index_value = res
          ))
        }
        group_members <- report_teams(x, group)
        return(tibble::tibble(
          group = names(res),
          group_members = unname(group_members[names(res)]),
          index_value = unname(res)
        ))
      }
      return(res)
    }
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
    max_dev <- (obs_range * (n)) / 2
    E <- 1 - (abs_dev / max_dev)
    list(CEI = C * E, C = C, E = E)
  }

  component <- toupper(return)
  if (!component %in% c("CEI", "C", "E")) {
    stop("Invalid return value. Use 'CEI', 'C', or 'E'.")
  }

  if (is.null(group)) {
    metrics <- compute_group_cei(x)
    component_value <- metrics[[component]]
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

    component_values <- res[[component]]
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
