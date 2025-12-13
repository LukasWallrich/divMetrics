# Compute Blau's Index

Computes Blau's Index for a numeric vector, optionally within groups.
Continuous values can be binned either by specifying a bin width or
explicit bin boundaries.

## Usage

``` r
compute_Blau(
  x,
  group = NULL,
  bin_width = NULL,
  bins = NULL,
  na.rm = FALSE,
  verbose = FALSE,
  return_df = FALSE
)
```

## Arguments

- x:

  Numeric vector.

- group:

  Optional grouping variable of the same length as x.

- bin_width:

  Positive numeric value specifying bin width.

- bins:

  Optional numeric vector of bin boundaries. Values are assigned to bins
  using left-inclusive, right-exclusive intervals (i.e., first interval
  \[a, b) does not include b).#' @param na.rm Logical. If TRUE, NA
  values are removed.

- verbose:

  Logical. If TRUE, prints the bin breaks.

- return_df:

  Logical. If TRUE, returns a dataframe with group, group_members and
  index_value.

## Value

A named numeric vector with one value per group if group is provided (or
a single value), unless return_df is `TRUE`.
