# Extracts row IDs and prepares bootstrap results for jackknife ----------------
prep_boots = function(boots) {
  # Extract row IDs in each bootstrap sample
  boots = boots %>%
    mutate(samples = map(splits, extract_row_ids))
}

# Computes absolute and relative influence values for each observation ---------
eval_influence = function(boots) {
  # Get all IDs in original sample
  sample_ids = boots %>%
    filter(id == "Apparent") %>%
    pull(splits) %>%
    extract2(1) %>%
    assessment() %>%
    pull(row_id) %>%
    unique()
  n = length(sample_ids)
  
  # Jackknife bootstrap samples
  jk_stats = rep(NA, n)
  for (i in sample_ids) {
    jk_stats[i] = boots %>%
      filter(map_lgl(samples, function(x) { i %in% x })) %>%
      pull(estimate) %>%
      mean()
  }
  jk_stats = tibble(
               row_id = sample_ids,
               deleted_point_values = jk_stats
             )
  
  # Compute influence function
  jk_mean = mean(jk_stats$deleted_point_values)
  jk_stats = jk_stats %>%
    mutate(
      abs_influence = (n - 1)*(jk_mean - deleted_point_values),
      rel_influence = abs_influence/sqrt(sum(abs_influence^2/(n - 1)))
    ) %>%
    select(row_id, abs_influence, rel_influence)
  
  return(jk_stats)
}

# Returns the passed quantiles for bootstrap samples after leaving out one -----
# observation at a time --------------------------------------------------------
find_percentiles = function(boots, probs = c(0.05, 0.50, 0.95)) {
  # Get all IDs in original sample
  sample_ids = boots %>%
    filter(id == "Apparent") %>%
    pull(splits) %>%
    extract2(1) %>%
    assessment() %>%
    pull(row_id) %>%
    unique()
  n = length(sample_ids)
  
  # Make matrix
  matrix_cols = length(probs) + 1
  percentiles = matrix(rep(NA, n * matrix_cols), ncol = matrix_cols)
  
  # Find percentiles when leaving out each point
  for (i in 1:n) {
    cur_id = sample_ids[i]
    cur_percentiles = boots %>%
      filter(map_lgl(samples, function(x) { cur_id %in% x })) %>%
      pull(estimate) %>%
      quantile(probs = probs)
    percentiles[i, ] = c(cur_id, cur_percentiles)
  }
  
  # Convert percentile matrix to tibble
  percentiles = as_tibble(percentiles, .name_repair = "minimal")
  colnames(percentiles) = c("row_id", paste0("percentile_", probs))
  
  return(percentiles)
}

# Helpers ======================================================================
# Extract row IDs from rsplit object -------------------------------------------
extract_row_ids = function(split, mode = "analysis") {
  # Set to extract row IDs from
  set_fn = switch(mode,
                  "analysis" = analysis,
                  "assessment" = assessment
  )
  
  # Extract row IDs
  row_ids = split %>%
    set_fn() %>%
    pull(row_id) %>%
    unique() %>%
    as.list()
  return(row_ids)
}
