# Resampling operation =========================================================
do_bootstrap <- function(df, B, spec) {
  # Select bootstrap procedure and generate bootstrap samples
  if (spec$stat %in% "Median") boot_proc <- resample_smooth
  else boot_proc <- resample_cases
  boot_samples <- boot_proc(df, B, spec)
  
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

# Bootstrap procedures #########################################################
# Case resampling ==============================================================
resample_cases <- function(df, B, spec) {
  bootstraps(df, times = B, apparent = TRUE) %>%
    mutate(sample = map(splits, ~analysis(.x)))
}

# Smoothed bootstrap ===========================================================
resample_smooth <- function(df, B, spec) {
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
