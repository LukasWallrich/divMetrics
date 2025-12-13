# Compute Coefficient of Variation (CV)

Computes the coefficient of variation (CV) for a numeric vector,
optionally within groups.

## Usage

``` r
compute_cv(
  x,
  group = NULL,
  na.rm = FALSE,
  warn_zero_mean = TRUE,
  return_df = FALSE
)
```

## Arguments

- x:

  Numeric vector.

- group:

  Optional grouping variable of the same length as x.

- na.rm:

  Logical. If TRUE, NA values are removed.

- warn_zero_mean:

  Logical. If TRUE (default), warns when mean is zero (resulting in NA).

- return_df:

  Logical. If TRUE, returns a dataframe with group, group_members and
  index_value.

## Value

A single CV value if group is NULL, or a named numeric vector with one
value per group if group is provided (or a data frame if return_df is
TRUE). Returns NA when mean is zero.
