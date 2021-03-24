# Performs nonparametric bootstrap resampling
do_bootstrap = function(df, B, fn, variable) {
  # Generate bootstrap samples
  boot_samples = bootstraps(df, times = B, apparent = TRUE)
  
  # Compute statistic on bootstrap samples
  statistic = select_fn(fn)
  reps = boot_samples %>%
    mutate(
      estimate = map(
                   splits,
                   function(split, fn, variable) {
                     split %>%
                       analysis() %>%
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

# Helpers ======================================================================
# Selects functino to apply ----------------------------------------------------
select_fn = function(fn) {
  switch(fn,
         "Mean" = mean
  )
}
