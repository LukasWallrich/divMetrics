# Compute All Diversity Metrics

Convenience function to compute multiple diversity metrics at once.

## Usage

``` r
compute_all_metrics(
  x,
  group = NULL,
  method = c("continuous", "categorical"),
  bin_width = NULL,
  bins = NULL,
  range = NULL,
  na.rm = FALSE,
  return_df = FALSE
)
```

## Arguments

- x:

  Numeric vector of values (continuous attributes) or character/factor
  for categorical

- group:

  Optional grouping variable

- method:

  Character. One of "continuous" (default) or "categorical". When
  "continuous", computes CV, SD, GMD, CEI (and Blau if binning is
  provided). When "categorical", not yet implemented.

- bin_width:

  Bin width for Blau (if NULL, skips Blau)

- bins:

  Optional bin boundaries for Blau

- range:

  Optional range for CEI (if NULL, uses observed range)

- na.rm:

  Logical. If TRUE, removes NA values

- return_df:

  Logical. If TRUE, returns a data frame; if FALSE returns a list

## Value

A data frame (if return_df = TRUE) or named list of diversity scores

## Examples

``` r
ages <- c(25, 28, 32, 45, 52)
compute_all_metrics(ages, bin_width = 10)
#> Using observed range for CEI. Specify 'range' for theoretical range.
#> $CV
#> [1] 0.3183733
#> 
#> $SD
#> [1] 11.58879
#> 
#> $GMD
#> [1] 14.2
#> 
#> $Blau
#> [1] 0.72
#> 
#> $CEI
#> [1] 0.8444444
#> 

# With groups
ages <- c(25, 28, 32, 45, 52, 30, 31, 32, 33, 34)
teams <- c(rep("A", 5), rep("B", 5))
compute_all_metrics(ages, group = teams, bin_width = 10, return_df = TRUE)
#> Using observed range for CEI. Specify 'range' for theoretical range.
#> # A tibble: 2 × 6
#>   group     CV    SD   GMD  Blau   CEI
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 A     0.318  11.6   14.2  0.72 0.844
#> 2 B     0.0494  1.58   2    0    0.148
```
