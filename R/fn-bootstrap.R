# Bootstrapping manager ========================================================
do_bootstrap <- function(df, B, spec, coefs = NULL, strata = NULL) {
  # Select bootstrap procedure and generate bootstrap samples
  if (spec$stat == "Median") boot_proc <- resample_smooth
  else if (spec$stat == "Linear Regression") boot_proc <- choose_lm_proc(
                                                            df,
                                                            spec
                                                          )
  else boot_proc <- resample_cases
  boot_samples <- boot_proc(df, B, spec, coefs, strata)
  
  # Select evaluation method and compute bootstrap replications
  eval_stat <- switch(spec$stat,
                 "Mean" = eval_mean,
                 "Median" = eval_median,
                 "Correlation" = eval_correlation,
                 "Linear Regression" = eval_lm,
                 "Smoothing Spline" = eval_spline,
                 "LOESS" = eval_loess
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
resample_cases <- function(df, B, spec, coefs = NULL, strata = NULL) {
  bootstraps(df, times = B, strata = all_of(strata), apparent = TRUE) %>%
    mutate(sample = map(splits, ~analysis(.)))
}

# Smoothed bootstrap ===========================================================
resample_smooth <- function(df, B, spec, coefs = NULL, strata = NULL) {
  bootstraps(df, times = B, strata = all_of(strata), apparent = TRUE) %>%
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
