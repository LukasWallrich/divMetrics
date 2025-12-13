# Compute the Coverage and Evenness Index (CEI) or its Components

Computes the Coverage and Evenness Index (CEI), or its components:
Coverage (Coverage) and Evenness Factor (E), for a numeric vector,
optionally within groups. C is the proportion of the theoretical range
covered by the data, and E is based on the uniformity of the
distribution of values.

## Usage

``` r
compute_CEI(
  x,
  group = NULL,
  range = NULL,
  na.rm = FALSE,
  return = "CEI",
  verbose = FALSE,
  return_df = FALSE
)
```

## Arguments

- x:

  Numeric vector.

- group:

  Optional grouping variable of the same length as x.

- range:

  Numeric vector of length 2 specifying the theoretical min and max;
  defaults to the observed range.

- na.rm:

  Logical. If TRUE, NA values are removed.

- return:

  Character: "CEI" (default), "C", or "E".

- verbose:

  Logical. If TRUE, prints the range used.

- return_df:

  Logical. If TRUE, returns a dataframe with group, group_members and
  index_value.

## Value

A named numeric vector with one value per group if group is provided (or
a single value), unless return_df is `TRUE`.
