# Generates jackknife-after-bootstrap samples ==================================
jackknife_after_bootstrap <- function(boots) {
  # Extract row IDs in each bootstrap sample
  boots <- boots %>%
    mutate(samples = map(splits, extract_row_ids))
  
  # Jackknife samples in a tibble
  sample_ids <- get_sample_ids(boots)
  n = length(sample_ids)
  jab_sample = list()
  for (i in 1:n) {
    cur_id = sample_ids[i]
    jab_sample[[i]] = filter(boots,
                        map_lgl(samples, function(x) { !cur_id %in% x })
                      )
  }
  
  return(tibble(deleted_case = sample_ids, jab_sample = jab_sample))
}

# Helpers ======================================================================
# Extract row IDs from rsplit object -------------------------------------------
extract_row_ids <- function(split, mode = "analysis") {
  # Set to extract row IDs from
  set_fn <- switch(mode,
                  "analysis" = analysis,
                  "assessment" = assessment
           )
  
  # Extract row IDs
  row_ids <- split %>%
    set_fn() %>%
    pull(row_id) %>%
    unique() %>%
    as.list()
  return(row_ids)
}

# Get all IDs in original sample -----------------------------------------------
get_sample_ids <- function(boots) {
  sample_ids <- boots %>%
    filter(id == "Apparent") %>%
    pull(splits) %>%
    extract2(1) %>%
    assessment() %>%
    pull(row_id) %>%
    unique()
  return(sample_ids)
}
