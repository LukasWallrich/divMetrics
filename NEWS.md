# divMetrics NEWS

## Version 0.2.0 (2025-01-13)

### Major Changes
- Initial beta release of the divMetrics package
- Uniform functions to calculate a variety of team diversity metrics
  - Variety based on categorisation: Blau's index
  - Separation based on continuous attributes: Standard deviation, Coefficient of variation, Gini Mean Difference
  - Hybrid metrics: Rao's quadratic entropy (categorical, with distance matrix), Coverage & Evenness Index (CEI; continuous, combining range coverage with evenness)
- Convenience function `compute_all_metrics()` to calculate all continuous metrics at once

### Features
- Consistent interface across all diversity metrics
- Support for grouped calculations (compare multiple teams/groups)
- Flexible NA handling with informative warnings
- Optional data frame output format with team member details
- Visualization tools to compare normalized diversity scores across metrics
- Automatic binning options for variety metrics
- Comprehensive test coverage for all core functions

### Documentation
- Detailed function documentation with examples
- Two comprehensive articles to inform choice of diversity metrics:
  - "Comparing metrics for continuous attributes"
  - "Assessing statistical power depending on measure"

### Limitations
- The package is currently in beta; users are encouraged to report bugs and suggest features
- The package is currently *focused* on diversity along continuous attributes; the support for categorical attributes is limited to a few metrics (Blau's index and Rao's quadratic entropy) and documented less comprehensively. This will be expanded in future releases