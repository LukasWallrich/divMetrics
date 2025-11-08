

#' Compute Coefficient of Variation (CV)
#'
#' Computes the coefficient of variation (CV) for a numeric vector, optionally within groups.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param warn_zero_mean Logical. If TRUE (default), warns when mean is zero (resulting in NA).
#' @param return_df Logical. If TRUE, returns a dataframe with group, group_members and index_value.
#'
#' @return A single CV value if group is NULL, or a named numeric vector with one value per group if group is provided (or a data frame if return_df is TRUE). Returns NA when mean is zero.
#' @export
compute_cv <- function(x, group = NULL, na.rm = FALSE, warn_zero_mean = TRUE, return_df = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

  compute_group_cv <- function(values) {
    m <- mean(values)
    if (m == 0) {
      if (warn_zero_mean) warning("Mean is zero; CV is undefined. Returning NA.")
      return(NA)
    }
    stats::sd(values) / m
  }

  if (is.null(group)) {
    res <- compute_group_cv(x)
  } else {
    res <- tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(CV = compute_group_cv(x), .groups = "drop") %>%
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

#' Compute Standard Deviation (SD)
#'
#' Computes the standard deviation for a numeric vector, optionally within groups.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param na.rm Logical. If TRUE, NA values are removed.
#' @param return_df Logical. If TRUE, returns a dataframe with group, group_members and index_value.
#'
#' @return A single SD value if group is NULL, or a named numeric vector with one value per group if group is provided (or a data frame if return_df is TRUE).
#' @export
compute_sd <- function(x, group = NULL, na.rm = FALSE, return_df = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  na_removed <- remove_na(x, group = group, na.rm = na.rm)
  x <- na_removed$x
  group <- na_removed$group

  if (!is.factor(group) && !is.null(group)) group <- factor(group, levels = unique(group))

  if (is.null(group)) {
    res <- stats::sd(x)
  } else {
    res <- tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(SD = stats::sd(x), .groups = "drop") %>%
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
