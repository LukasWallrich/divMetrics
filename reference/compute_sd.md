# Compute Standard Deviation (SD)

Computes the standard deviation for a numeric vector, optionally within
groups.

## Usage

``` r
compute_sd(x, group = NULL, na.rm = FALSE, return_df = FALSE)
```

## Arguments

- x:

  Numeric vector.

- group:

  Optional grouping variable of the same length as x.

- na.rm:

  Logical. If TRUE, NA values are removed.

- return_df:

  Logical. If TRUE, returns a dataframe with group, group_members and
  index_value.

## Value

A single SD value if group is NULL, or a named numeric vector with one
value per group if group is provided (or a data frame if return_df is
TRUE).
