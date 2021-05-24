# Resampling and estimation operations #########################################
do_bootstrap <- function(df, B, spec, coefs = NULL) {
  # Select bootstrap procedure and generate bootstrap samples
  if (spec$stat %in% "Median") boot_proc <- resample_smooth
  else boot_proc <- resample_cases
  boot_samples <- boot_proc(df, B, spec, coefs)
  
  # Select evaluation method and compute bootstrap replications
  eval_stat <- switch(spec$stat,
                 "Mean" = eval_mean,
                 "Median" = eval_median,
                 "Correlation" = eval_correlation,
                 "Linear Model" = eval_lm
  )
  boot_reps <- eval_stat(boot_samples, spec)
  
  return(boot_reps)
}

estimate_distribution <- function(df, fn) {
  df %>%
    summarize(across(starts_with("replication"), fn)) %>%
    flatten()
}

# Bootstrap procedures #########################################################
# Case resampling ==============================================================
resample_cases <- function(df, B, spec, coefs = NULL) {
  bootstraps(df, times = B, apparent = TRUE) %>%
    mutate(sample = map(splits, ~analysis(.)))
}

# Smoothed bootstrap ===========================================================
resample_smooth <- function(df, B, spec, coefs = NULL) {
  bootstraps(df, times = B, apparent = TRUE) %>%
    mutate(
      sample = map(splits,
                 ~select(analysis(.), spec$var) + matrix(
                                                    rnorm(
                                                      nrow(.) * ncol(.),
                                                      sd = 1 / sqrt(nrow(.))
                                                    ),
                                                    ncol = ncol(.)
                                                  )
               )
    )
}

# Residual resampling for models y = XB + e ====================================
resample_residuals <- function(df, B, spec, coefs) {
  # Calculate residuals --------------------------------------------------------
  # Construct design matrix
  X_values <- rep(1, nrow(df))
  for (covariate in spec$predictors) {
    X_values <- c(X_values, pull(df, as.name(covariate)))
  }
  X <- matrix(
         X_values, nrow = nrow(df),
         dimnames = list(rownames(df), c("Intercept", spec$predictors)))
  
  # Compute predicted responses and compute residuals
  predictions <- as.vector(X %*% unlist(coefs))
  residuals <- pull(df, as.name(spec$response)) - predicted_responses
  
  # Resample residuals to obtain bootstrap data --------------------------------
  boot_samples <- bootstraps(tibble(boot_residual = residuals)) %>%
    mutate(
      sample = map(splits,
                 ~analysis(.) %>%
                   mutate(!!spec$response := boot_residual + predictions) %>%
                   cbind( # Add covariates
                     X %>%
                       as_tibble() %>%
                       select(-Intercept)
                   )
               )
    )
  
  return(boot_samples)
}

# Statistic evaluation methods #################################################
# Mean =========================================================================
eval_mean <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_mean, spec = spec))
}

# Median =======================================================================
eval_median <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_median, spec = spec))
}

# Correlation ==================================================================
eval_correlation <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_correlation, spec = spec))
}

# Linear model =================================================================
eval_lm <- function(df, spec) {
  df %>%
    mutate(replication = map(sample, estimate_lm, spec = spec)) %>%
    unnest_wider(replication)
}

# Helper functions #############################################################
# Generates procedure specification ============================================
make_spec <- function(stat, var1, vars = NULL) {
  switch(stat,
    "Mean" = list(stat = "Mean", var = var1),
    "Median" = list(stat = "Median", var = var1),
    "Correlation" = list(stat = "Correlation", var1 = var1, var2 = vars),
    "Linear Model" = list(
                       stat = "Linear Model",
                       response = var1,
                       predictors = vars
                     )
  )
}
