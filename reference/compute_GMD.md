# Compute Gini Mean Difference (GMD)

Computes the Gini Mean Difference (mean pairwise absolute difference)
for a numeric vector, optionally within groups. Returns values in the
same units as the input (e.g., years for age). This is the continuous
analogue of Rao's Q with equal weights and absolute-distance
dissimilarity, differing only by the factor (n-1)/n.

## Usage

``` r
compute_GMD(x, group = NULL, na.rm = FALSE, return_df = FALSE)
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

A single GMD value if group is NULL, or a named numeric vector with one
value per group if group is provided (or a data frame if return_df is
TRUE).
