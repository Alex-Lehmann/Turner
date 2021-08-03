# General estimator function ===================================================
point_estimate <- function(df, spec) {
  estimate_fn <- switch(spec$stat,
                   "Mean" = estimate_mean,
                   "Median" = estimate_median,
                   "Correlation" = estimate_correlation,
                   "Linear Regression" = estimate_lm,
                   "Smoothing Spline" = estimate_spline,
                   "LOESS" = estimate_loess
                 )
  estimate <- estimate_fn(df, spec)
  
  if (!is.list(estimate)) return(list(replication = estimate))
  else return(estimate)
}
