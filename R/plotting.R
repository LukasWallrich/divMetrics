#' Compare Multiple Diversity Metrics
#'
#' Creates a line plot comparing normalized diversity scores across multiple metrics.
#' Useful for visualizing how different teams rank differently depending on the metric chosen.
#'
#' @param data A data frame containing diversity scores
#' @param metrics Character vector of column names containing diversity metrics to compare
#' @param group_var Character string naming the column that identifies groups/teams
#' @param group_members_var Optional character string naming the column with group member details
#' @param normalize Logical. If TRUE (default), normalizes scores by dividing by standard deviation
#' @param title Character string for plot title
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate multiple metrics
#' ages <- c(25, 28, 32, 45, 52, 30, 31, 32, 33, 34)
#' teams <- c(rep("A", 5), rep("B", 5))
#'
#' results <- data.frame(
#'   team = c("A", "B"),
#'   CV = compute_cv(ages, teams),
#'   SD = compute_sd(ages, teams),
#'   Blau = compute_Blau(ages, teams, bin_width = 10)
#' )
#'
#' plot_metric_comparison(results,
#'                       metrics = c("CV", "SD", "Blau"),
#'                       group_var = "team")
#' }
plot_metric_comparison <- function(data, metrics, group_var,
                                   group_members_var = NULL,
                                   normalize = TRUE,
                                   title = "Comparison of Diversity Metrics") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.")
  }

  # Prepare data for plotting
  plot_data <- data

  # Normalize if requested
  if (normalize) {
    for (metric in metrics) {
      if (metric %in% names(plot_data)) {
        plot_data[[metric]] <- plot_data[[metric]] / stats::sd(plot_data[[metric]], na.rm = TRUE)
      }
    }
  }

  # Reshape to long format
  plot_data <- plot_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(metrics),
                       names_to = "Metric",
                       values_to = "Score")

  # Create base plot
  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = .data$Metric,
                                  y = .data$Score,
                                  group = .data[[group_var]],
                                  color = .data[[group_var]])) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = title,
                 x = "Diversity Metric",
                 y = if (normalize) "Normalized Score" else "Score",
                 color = group_var) +
    ggplot2::theme_minimal()

  # Add labels if group_members provided
  if (!is.null(group_members_var) && group_members_var %in% names(data)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      label_data <- plot_data %>%
        dplyr::filter(.data$Metric == metrics[1])
      label_data$label <- paste0(data[[group_var]], ": ", data[[group_members_var]])

      p <- p + ggrepel::geom_text_repel(data = label_data,
                                        ggplot2::aes(label = .data$label),
                                        nudge_x = -0.5)
    }
  }

  p
}


#' Compute All Diversity Metrics
#'
#' Convenience function to compute multiple diversity metrics at once.
#'
#' @param x Numeric vector of values (continuous attributes) or character/factor for categorical
#' @param group Optional grouping variable
#' @param method Character. One of "continuous" (default) or "categorical".
#'   When "continuous", computes CV, SD, GMD, CEI (and Blau if binning is provided).
#'   When "categorical", not yet implemented.
#' @param bin_width Bin width for Blau (if NULL, skips Blau)
#' @param bins Optional bin boundaries for Blau
#' @param range Optional range for CEI (if NULL, uses observed range)
#' @param na.rm Logical. If TRUE, removes NA values
#' @param return_df Logical. If TRUE, returns a data frame; if FALSE returns a list
#'
#' @return A data frame (if return_df = TRUE) or named list of diversity scores
#' @export
#'
#' @examples
#' ages <- c(25, 28, 32, 45, 52)
#' compute_all_metrics(ages, bin_width = 10)
#'
#' # With groups
#' ages <- c(25, 28, 32, 45, 52, 30, 31, 32, 33, 34)
#' teams <- c(rep("A", 5), rep("B", 5))
#' compute_all_metrics(ages, group = teams, bin_width = 10, return_df = TRUE)
compute_all_metrics <- function(x, group = NULL,
                               method = c("continuous", "categorical"),
                               bin_width = NULL, bins = NULL,
                               range = NULL, na.rm = FALSE, return_df = FALSE) {

  method <- match.arg(method)
  results <- list()

  if (method == "categorical") {
    stop("Categorical method not yet implemented.")
  }

  # For continuous attributes
  results$CV <- compute_cv(x, group = group, na.rm = na.rm)
  results$SD <- compute_sd(x, group = group, na.rm = na.rm)
  results$GMD <- compute_GMD(x, group = group, na.rm = na.rm)

  # Variety metrics (require binning)
  if (!is.null(bin_width) || !is.null(bins)) {
    results$Blau <- compute_Blau(x, group = group, bin_width = bin_width,
                                 bins = bins, na.rm = na.rm)
  }

  # Hybrid metric
  if (is.null(range)) {
    message("Using observed range for CEI. Specify 'range' for theoretical range.")
    range <- range(x, na.rm = TRUE)
  }
  results$CEI <- compute_CEI(x, group = group, range = range, na.rm = na.rm)

  # Convert to data frame if requested
  if (return_df) {
    if (is.null(group)) {
      tibble::tibble(!!!results)
    } else {
      # Combine all results into a data frame
      group_names <- names(results[[1]])
      df <- tibble::tibble(group = group_names)
      for (metric_name in names(results)) {
        df[[metric_name]] <- results[[metric_name]]
      }
      df
    }
  } else {
    results
  }
}
