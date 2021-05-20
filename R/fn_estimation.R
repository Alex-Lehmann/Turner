# General estimator function ===================================================
point_estimate <- function(df, spec) {
  estimate_fn <- switch(spec$stat,
                   "Mean" = estimate_mean,
                   "Median" = estimate_median,
                   "Correlation" = estimate_correlation
                 )
  estimate <- estimate_fn(df, spec)
  return(estimate)
}

# Statistic evaluation methods #################################################
# Mean =========================================================================
estimate_mean <- function(df, spec) { mean(pull(df, as.name(spec$var))) }

# Median =======================================================================
estimate_median <- function(df, spec) { median(pull(df, as.name(spec$var))) }

# Correlation ==================================================================
estimate_correlation <- function(df, spec) {
  cor_mat <- df %>%
    select(spec$var1, spec$var2) %>%
    cor()
  return(cor_mat[1,2])
}
