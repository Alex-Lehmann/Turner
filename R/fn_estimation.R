# General estimator function ===================================================
point_estimate <- function(df, spec) {
  estimate_fn <- switch(spec$stat,
                   "Mean" = estimate_mean,
                   "Median" = estimate_median,
                   "Correlation" = estimate_correlation,
                   "Linear Model" = estimate_lm
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

# Linear model =================================================================
estimate_lm <- function(df, spec) {
  # Construct model formula
  model <- as.formula(paste0(
                        spec$response,
                        " ~ ",
                        str_c(spec$predictors, collapse = " + ")
                      )
           )
  fit <- lm(model, data = df)
  
  # Convert results to list
  coefs <- fit$coefficients %>%
    as.list() %>%
    set_names(
      names(.) %>%
        str_replace_all("[^[[:alnum:]]]", "") %>%
        paste0("replication_", .)
    )
  
  return(coefs)
}
