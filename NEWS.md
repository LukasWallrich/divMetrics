# divMetrics (development version)

## New Features

* Added `compute_all_metrics()` for convenient calculation of all diversity metrics at once
* Added `plot_metric_comparison()` for visualizing differences between metrics
* Added `return_df` parameter to all compute functions for consistent data frame output
* Added `verbose` parameter to `compute_CEI()` and `compute_Rao()` for optional progress messages
* Added `warn_zero_mean` parameter to `compute_cv()` to control warnings about undefined CV

## Improvements

* All functions now validate that `x` and `group` have the same length
* `compute_cv()` and `compute_sd()` now use the shared `remove_na()` helper for consistency
* Improved error messages and input validation across all functions
* Added comprehensive test suite (95 tests, up from 25)

## Documentation

* Added README with usage examples and metric comparison guidance
* Added pkgdown website configuration
* Improved function documentation with better parameter descriptions
* All vignette dependencies now declared in DESCRIPTION

## Bug Fixes

* Fixed potential issues with length mismatches between `x` and `group`
* Removed redundant wrapper function in `compute_sd()`
* Standardized NA handling across all functions
