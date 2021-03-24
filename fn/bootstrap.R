library(tidymodels)

# Performs nonparametric bootstrap resampling
do_bootstrap = function(df, B, statistic, variable) {
  # Generate bootstrap samples
  boot_samples = bootstraps(df, times = B, apparent = TRUE)
  
  # Compute statistic on bootstrap samples
  reps = values$boot_reps %>%
    mutate(
      estimate = map(
                   splits,
                   compute_statistic,
                   variable = variable,
                   statistic = statistic
                 )
    ) %>%
    unnest(model)
  boot_stat = mean(boot_reps$estimate)
  
  return(list(reps = reps, stat = boot_stat))
}

# Helpers ======================================================================
# Computes statistic on bootstrap samples
compute_statistic = function(split, variable, statistic) {
  split %>%
    analysis() %>%
    pull(as.name(variable)) %>%
    fn()
}