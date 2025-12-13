# divMetrics

`divMetrics` provides functions for calculating a range of diversity
measures for continuous attributes and visualizing them. The package
enables researchers to explore divergences between diversity measures to
select one that matches their research questions and theoretical
framework. For continuous attributes, it offers directly interpretable
spread metrics (e.g., the Gini Mean Difference). Rao’s quadratic entropy
is available in its general distance‑matrix form for categorical
attributes.

## Installation

You can install the development version of divMetrics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lukaswallrich/divMetrics")
```

## Overview

Diversity can be conceptualized in different ways (Harrison & Klein,
2007):

- **Variety**: Even spread across the categories represented
- **Separation**: Differences in position along a continuum (e.g.,
  dispersion)
- **Disparity**: Inequality

For continuous attributes (e.g., age, tenure), different metrics capture
different aspects of diversity. `divMetrics` implements several key
measures:

| Metric                                                                                   | Type                 | Description                                                                       |
|------------------------------------------------------------------------------------------|----------------------|-----------------------------------------------------------------------------------|
| [`compute_Blau()`](https://lukaswallrich.github.io/divMetrics/reference/compute_Blau.md) | Variety              | Blau’s index with binning for continuous data                                     |
| [`compute_cv()`](https://lukaswallrich.github.io/divMetrics/reference/compute_cv.md)     | Separation/Disparity | Coefficient of variation                                                          |
| [`compute_sd()`](https://lukaswallrich.github.io/divMetrics/reference/compute_sd.md)     | Separation           | Standard deviation                                                                |
| [`compute_GMD()`](https://lukaswallrich.github.io/divMetrics/reference/compute_GMD.md)   | Separation           | Gini Mean Difference (mean pairwise absolute difference; continuous Rao analogue) |
| [`compute_CEI()`](https://lukaswallrich.github.io/divMetrics/reference/compute_CEI.md)   | Variety              | Coverage and Evenness Index                                                       |

CEI captures coverage of a theoretical range together with within‑range
evenness.

## Basic Usage

### Single Group

Calculate diversity for a single group:

``` r
library(divMetrics)

# Example: Age diversity in a team
ages <- c(25, 28, 32, 45, 52)

# Standard deviation (separation)
compute_sd(ages)
#> [1] 11.58879

# Coefficient of variation (disparity)
compute_cv(ages)
#> [1] 0.3183733

# Blau's index (requires binning)
compute_Blau(ages, bin_width = 10)
#> [1] 0.72

# Coverage and Evenness Index
compute_CEI(ages, range = c(20, 65))
#> [1] 0.4833333

# Gini Mean Difference (continuous)
compute_GMD(ages)
#> [1] 14.2
```

### Multiple Groups

Compare diversity across multiple teams:

``` r
# Two teams with different age distributions
ages <- c(25, 28, 32, 45, 52, 30, 31, 32, 33, 34)
teams <- c(rep("Team A", 5), rep("Team B", 5))

# Calculate CV for each team
compute_cv(ages, group = teams)
#>     Team A     Team B 
#> 0.31837329 0.04941059

# Calculate Blau's index for each team
compute_Blau(ages, group = teams, bin_width = 10)
#> Team A Team B 
#>   0.72   0.00

# Get results as a data frame
compute_Blau(ages, group = teams, bin_width = 10, return_df = TRUE)
#> # A tibble: 2 × 3
#>   group  group_members      index_value
#>   <chr>  <chr>                    <dbl>
#> 1 Team A 25, 28, 32, 45, 52        0.72
#> 2 Team B 30, 31, 32, 33, 34        0
```

### Rao with a distance matrix (categorical)

``` r
# Suppose categories A/B with a distance of 2 between them
D <- matrix(c(0, 2,
              2, 0), nrow = 2, byrow = TRUE,
            dimnames = list(c("A","B"), c("A","B")))
cats <- c("A","A","B","B","A")
compute_Rao(cats, D = D)
#> [1] 0.96
```

## Compute multiple metrics at once

``` r
# For continuous attributes, include CV, SD, GMD, CEI,
# and (optionally) Blau if you pass bins/bin_width
compute_all_metrics(ages, method = "continuous", bin_width = 10, return_df = TRUE)
#> Using observed range for CEI. Specify 'range' for theoretical range.
#> # A tibble: 1 × 5
#>      CV    SD   GMD  Blau   CEI
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 0.238  8.13  8.62  0.58 0.646
```

## Choosing a Metric

Different metrics emphasize different aspects of diversity:

- **SD**: Prioritises separation between subgroups, maximised when
  values cluster at the extremes
- **CV**: Focuses on disparity, maximised when values are skewed towards
  the lower end of the distribution
- **GMD**: Mean absolute difference in the attribute (interpretable in
  original units)
- **Blau’s Index**: Captures categorical variety (depends on binning
  choice)
- **CEI**: Combines range coverage with evenness of distribution
- **Rao’s Index (categorical)**: Expected dissimilarity p’ D p using a
  supplied distance matrix

See `vignette("metrics_comparison")` for detailed comparisons and
guidance.

## Key Features

- Consistent interface across all metrics
- Support for grouped calculations
- Flexible handling of missing values
- Optional data frame output format
- Implementation of the novel CEI metric (Coverage & Evenness Index)
- Automatic binning options for Blau (variety)

## References

Harrison, D. A., & Klein, K. J. (2007). What’s the difference? Diversity
constructs as separation, variety, or disparity in organizations.
*Academy of Management Review, 32*(4), 1199-1228.

## License

MIT License
