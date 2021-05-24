# General estimator function ===================================================
point_estimate <- function(df, spec) {
  estimate_fn <- switch(spec$stat,
                   "Mean" = estimate_mean,
                   "Median" = estimate_median,
                   "Correlation" = estimate_correlation,
                   "Linear Regression" = estimate_lm
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

# Linear regressio =============================================================
estimate_lm <- function(df, spec) {
  # Construct model formula and fit
  model <- as.formula(paste0(
                        spec$response,
                        " ~ ",
                        str_c(spec$predictors, collapse = " + ")
                      )
           )
  #fit <- lm(model, df)
  fit <- switch(spec$fit,
           "Ordinary Least Squares" = lm(model, df),
           "Least Median of Squares" = lmsreg(model, df)
         )
  
  # Extract coefficients from fit
  coefs <- fit$coefficients %>%
    as.list() %>%
    set_names(
      names(.) %>%
        str_replace_all("[^[[:alnum:]]]", "") %>%
        paste0("replication_", .)
    )
  
  return(coefs)
}
