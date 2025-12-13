# Compare Multiple Diversity Metrics

Creates a line plot comparing normalized diversity scores across
multiple metrics. Useful for visualizing how different teams rank
differently depending on the metric chosen.

## Usage

``` r
plot_metric_comparison(
  data,
  metrics,
  group_var,
  group_members_var = NULL,
  normalize = TRUE,
  title = "Comparison of Diversity Metrics"
)
```

## Arguments

- data:

  A data frame containing diversity scores

- metrics:

  Character vector of column names containing diversity metrics to
  compare

- group_var:

  Character string naming the column that identifies groups/teams

- group_members_var:

  Optional character string naming the column with group member details

- normalize:

  Logical. If TRUE (default), normalizes scores by dividing by standard
  deviation

- title:

  Character string for plot title

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate multiple metrics
ages <- c(25, 28, 32, 45, 52, 30, 31, 32, 33, 34)
teams <- c(rep("A", 5), rep("B", 5))

results <- data.frame(
  team = c("A", "B"),
  CV = compute_cv(ages, teams),
  SD = compute_sd(ages, teams),
  Blau = compute_Blau(ages, teams, bin_width = 10)
)

plot_metric_comparison(results,
                      metrics = c("CV", "SD", "Blau"),
                      group_var = "team")
} # }
```
