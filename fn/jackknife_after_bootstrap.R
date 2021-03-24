# Performs jackknife-after-bootstrap (JAB) and computes estimates, influence
# values, and quantiles for outlier detection use
jackknife_after_bootstrap = function(boots, probs = c(0.05, 0.50, 0.95)) {
  # Extract row IDs in each bootstrap sample
  boots = boots %>%
    mutate(samples = map(splits, extract_row_ids))
  
  # Jackknife samples in a tibble
  sample_ids = get_sample_ids(boots)
  n = length(sample_ids)
  jack_sample = list()
  for (i in 1:n) {
    cur_id = sample_ids[i]
    jack_sample[[i]] = filter(boots,
                        map_lgl(samples, function(x) { !cur_id %in% x })
                      )
  }
  jab_values = tibble(deleted_case = sample_ids, jack_sample = jack_sample)
  
  # Compute jackknife statistic, influence values, and quantiles
  jab_values = mutate(jab_values,
                 estimate=map_dbl(jack_sample, function(x) { mean(x$estimate)}),
                 abs_influence = (n - 1) * (mean(estimate) - estimate),
                 rel_influence = abs_influence / sqrt(
                                                   sum(abs_influence^2/(n - 1))
                                                 )
               )
  for (p in probs) {
    jab_values = mutate(jab_values,
                   !!paste0("quantile_", p) := map_dbl(jack_sample,
                                                 function(x) {
                                                   quantile(x$estimate, p)
                                                 }
                                               )
                 )
  }
  
  return(jab_values)
}

# Computes uncertainty bands using IQR and normal theory for each quantile
get_uncertainty_bands = function(boots, jack, probs = c(0.05, 0.50, 0.95)) {
  # Find full-data bootstrap quantiles
  uncertainty_bands = tibble(
                        quantiles = probs,
                        full_quantile = quantile(boots$estimate, probs = probs)
                      )
  
  # Find band widths and construct upper/lower bounds
  uncertainty_bands$band_width = jack %>%
    summarize(across(starts_with("quantile"), function(x) { IQR(x)*1.96 })) %>%
    unlist()
  uncertainty_bands = mutate(uncertainty_bands,
                        lower = full_quantile - band_width,
                        upper = full_quantile + band_width
                      )
  return(uncertainty_bands)
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

# Get all IDs in original sample -----------------------------------------------
get_sample_ids = function(boots) {
  sample_ids = boots %>%
    filter(id == "Apparent") %>%
    pull(splits) %>%
    extract2(1) %>%
    assessment() %>%
    pull(row_id) %>%
    unique()
  sample_ids
}
