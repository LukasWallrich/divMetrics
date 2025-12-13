# Compute Rao's Quadratic Entropy (distance-matrix form)

Computes Rao's Quadratic Entropy for a categorical vector using a
provided dissimilarity matrix D. For each group, the category
proportions p are computed and Q = p' D p is returned.

## Usage

``` r
compute_Rao(x, group = NULL, D, na.rm = FALSE, return_df = FALSE)
```

## Arguments

- x:

  Vector of categories (character or factor). Values must be present in
  the row/column names of `D`.

- group:

  Optional grouping variable of the same length as x. Will be converted
  to a factor if not already.

- D:

  A symmetric distance/dissimilarity matrix (or a `dist` object) whose
  row/column names define the category universe.

- na.rm:

  Logical. If TRUE, NA values are removed.

- return_df:

  Logical. If TRUE, returns a dataframe with group, group_members and
  index_value.

## Value

A single Rao's Index value if group is NULL, or a named numeric vector
with one value per group if group is provided (or a data frame if
return_df is TRUE).

## Details

This variant does not accept binning for continuous data. For continuous
attributes, use
[`compute_GMD()`](https://lukaswallrich.github.io/divMetrics/reference/compute_GMD.md)
instead.
