# Report Correlation Table

Creates a compact, APA-style correlation table with descriptive
statistics (M, SD), lower-triangular correlations, 95% CIs, and
significance stars, returned as a formatted `gt` table. Simplified
replacement for `timesaveR::report_cor_table()`.

## Usage

``` r
report_cor_table(x)
```

## Source

https://lukaswallrich.github.io/timesaveR/

## Arguments

- x:

  A correlation matrix produced by
  [`cor_matrix()`](https://lukaswallrich.github.io/divMetrics/reference/cor_matrix.md)
  (must carry the original data in attribute `"data"`).

## Value

A `gt_tbl` object. If the `gt` package is not installed, an error is
raised with an instruction to install it.
