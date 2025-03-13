

#' Compute Coefficient of Variation (CV)
#'
#' Computes the coefficient of variation (CV) for a numeric vector, optionally within groups.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param na.rm Logical. If TRUE, NA values are removed.
#'
#' @return A single CV value if group is NULL, or a named numeric vector with one value per group if group is provided.
#' @export
compute_cv <- function(x, group = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  if (!na.rm && any(is.na(x))) stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
  if (na.rm && any(is.na(x))) {
    keep <- !is.na(x)
    warning(sum(!keep), " missing values were removed.")
    x <- x[keep]
    if (!is.null(group)) group <- group[keep]
  }

  compute_group_cv <- function(values) {
    if (mean(values) == 0) return(NA)
    stats::sd(values) / mean(values)
  }

  if (is.null(group)) {
    compute_group_cv(x)
  } else {
    tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(CV = compute_group_cv(x), .groups = "drop") %>%
      tibble::deframe()
  }
}

#' Compute Standard Deviation (SD)
#'
#' Computes the standard deviation for a numeric vector, optionally within groups.
#'
#' @param x Numeric vector.
#' @param group Optional grouping variable of the same length as x.
#' @param na.rm Logical. If TRUE, NA values are removed.
#'
#' @return A single SD value if group is NULL, or a named numeric vector with one value per group if group is provided.
#' @export
compute_sd <- function(x, group = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")

  if (!na.rm && any(is.na(x))) stop("`x` contains missing values. Set na.rm = TRUE to ignore them.")
  if (na.rm && any(is.na(x))) {
    keep <- !is.na(x)
    warning(sum(!keep), " missing values were removed.")
    x <- x[keep]
    if (!is.null(group)) group <- group[keep]
  }

  compute_group_sd <- function(values) {
    stats::sd(values)
  }

  if (is.null(group)) {
    compute_group_sd(x)
  } else {
    tibble::tibble(x = x, group = group) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(SD = compute_group_sd(x), .groups = "drop") %>%
      tibble::deframe()
  }
}
