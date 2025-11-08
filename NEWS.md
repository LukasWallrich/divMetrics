# divMetrics (development version)

Initial unreleased version.

- Core metrics: `compute_sd()`, `compute_cv()`, `compute_GMD()` (continuous), `compute_Blau()` (with bins), `compute_CEI()`.
- Rao's quadratic entropy is provided as `compute_Rao(x, D=...)` for categorical data with a user‑supplied distance matrix.
- Helper utilities: `compute_all_metrics(method = "continuous")`, `plot_metric_comparison()`.
