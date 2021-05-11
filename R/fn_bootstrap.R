# Performs nonparametric bootstrap resampling ----------------------------------
do_bootstrap = function(df, B, fn, variable, seed) {
  # Random seed
  if (is.na(seed)) set.seed(NULL)
  else set.seed(seed)
  
  # Generate bootstrap samples; smooth if statistic demands it
  boot_samples = bootstraps(df, times = B, apparent = TRUE) %>%
    mutate(splits,
      sample = map(splits,
                 ~.x %>%
                   analysis() %>%
                   select(all_of(variable))
               )
    )
  if (fn %in% c("Median")) {
    boot_samples = mutate(boot_samples,
                     sample = map(sample,
                                ~.x + matrix(
                                        rnorm(nrow(.x)*ncol(.x)),
                                        ncol = ncol(.x)
                                      )
                              )
                   )
  }
  
  # Compute statistic on bootstrap samples
  statistic = select_fn(fn)
  reps = boot_samples %>%
    mutate(
      estimate = map(sample,
                   function(sample, fn, variable) {
                     sample %>%
                       pull(as.name(variable)) %>%
                       fn()
                   },
                   variable = variable,
                   fn = statistic
                 )
    ) %>%
    unnest(estimate)
  boot_stat = mean(reps$estimate)
  
  return(list(reps = reps, stat = boot_stat))
}

# Builds a pivot confidence interval at the specified confidence level ---------
make_pivot_ci = function(df, variable, alpha) {
  v = df[[variable]]
  
  # Compute upper and lower quantiles
  upper_quantile = quantile(v, probs = 1 - (alpha / 2), na.rm = TRUE)
  lower_quantile = quantile(v, probs = alpha / 2, na.rm = TRUE)
  
  # Compute CI
  center = mean(v)
  ci = c(center, (2 * center) - c(upper_quantile, lower_quantile))
  return(ci)
}

# Helpers ======================================================================
# Selects functino to apply ----------------------------------------------------
select_fn = function(fn) {
  switch(fn,
         "Mean" = mean,
         "Median" = median
  )
}
