# Bootstrapping manager ========================================================
do_bootstrap <- function(df, B, spec, coefs = NULL) {
  # Select bootstrap procedure and generate bootstrap samples
  if (spec$stat %in% "Median") boot_proc <- resample_smooth
  else if (spec$stat %in% "Linear Regression") boot_proc <- resample_residuals
  else boot_proc <- resample_cases
  boot_samples <- boot_proc(df, B, spec, coefs)
  
  # Select evaluation method and compute bootstrap replications
  eval_stat <- switch(spec$stat,
                 "Mean" = eval_mean,
                 "Median" = eval_median,
                 "Correlation" = eval_correlation,
                 "Linear Regression" = eval_lm
  )
  boot_reps <- eval_stat(boot_samples, spec)
  
  return(boot_reps)
}

# Summary statistic estimation =================================================
estimate_summary <- function(df, fn) {
  df %>%
    summarize(across(starts_with("replication"), fn)) %>%
    flatten() %>%
    as.list()
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
                 ~dplyr::select(analysis(.), spec$var) + matrix(
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
         dimnames = list(df$row_id, c("Intercept", spec$predictors))
       )
  
  # Compute predicted responses and compute residuals
  predictions <- as.vector(X %*% unlist(coefs))
  residuals <- pull(df, as.name(spec$response)) - predictions
  
  # Resample residuals ---------------------------------------------------------
  # Create bootstrap response data
  boot_samples <- bootstraps(
                    tibble(
                      boot_residual = residuals,
                      row_id = rownames(X)
                    ),
                    times = B,
                    apparent = TRUE
                  ) %>%
    mutate(sample = map(splits,
                      ~analysis(.) %>%
                         mutate(
                           !!spec$response := boot_residual + predictions,
                           row_id = as.integer(row_id)
                         )
           )
    )
  
  # Add covariates
  covariates <- dplyr::select(df, row_id, spec$predictors)
  boot_samples <- mutate(boot_samples,
                    sample = map(sample,
                               inner_join, y = covariates, by = "row_id"
                             )
                  )
  
  return(boot_samples)
}

# Procedure decision functions #################################################
choose_lm_proc <- function(df, spec) {
  # Regenerate model -----------------------------------------------------------
  # Construct model formula and fit
  model <- as.formula(paste0(
                        spec$response,
                        " ~ ",
                        str_c(spec$predictors, collapse = " + ")
                      )
  )
  fit <- switch(spec$fit,
                "Ordinary Least Squares" = lm(model, df),
                "Least Median of Squares" = lmsreg(model, df),
                "Iteratively Re-Weighted Least Squares" = rlm(model, df)
  )
  
  # White's test for heteroskedasticity ----------------------------------------
  test <- white_lm(fit)
  if (test$p.value > 0.05) return(resample_residuals) # Homoskedastic residuals
  else return(resample_cases) # Heteroskedastic residuals
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

# Linear regression ============================================================
eval_lm <- function(df, spec) {
  replications <- df %>%
    mutate(replication = map(sample, estimate_lm, spec = spec)) %>%
    unnest_wider(replication) %>%
    dplyr::select(starts_with("replication_"))
  
  augmented_df <- df
  for (col_name in colnames(replications)) {
    augmented_df <- mutate(augmented_df, !!col_name := replications[[col_name]])
  }
  
  return(augmented_df)
}

# Helper functions #############################################################
# Generates procedure specification ============================================
make_spec <- function(stat, var1, vars = NULL, fit = NULL) {
  switch(stat,
    "Mean" = list(stat = stat, var = var1),
    "Median" = list(stat = stat, var = var1),
    "Correlation" = list(stat = stat, var1 = var1, var2 = vars),
    "Linear Regression" = list(
                       stat = stat,
                       response = var1,
                       predictors = vars,
                       fit = fit
                     )
  )
}
