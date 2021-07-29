# Generates jackknife-after-bootstrap samples ==================================
jackknife_after_bootstrap <- function(boots) {
  # Extract row IDs in each bootstrap sample
  boots <- boots %>%
    mutate(samples = map(splits, extract_row_ids))
  
  # Jackknife samples in a tibble
  sample_ids <- get_sample_ids(boots)
  n <- length(sample_ids)
  jab_sample <- list()
  for (i in 1:n) {
    cur_id <- sample_ids[i]
    jab_sample[[i]] <- filter(boots,
                         map_lgl(samples, function(x) { !cur_id %in% x })
                       )
  }
  
  return(tibble(deleted_case = sample_ids, jab_sample = jab_sample))
}

# Computes influence values for each observation ===============================
jackknife_influence <- function(df) {
  n <- nrow(df)
  
  # Compute jackknife estimates ------------------------------------------------
  jab_values <- df %>%
    mutate(
      jab_estimate = map(jab_sample,
                       ~summarize(., across(starts_with("replication"), mean))
                     )
    ) %>%
    unnest_wider(jab_estimate)
    
  # Compute influence values -------------------------------------------------
  jab_influence <- jab_values %>%
    mutate(
      across(starts_with("replication"), ~(mean(.) - .)^2),
      abs_influence = (n - 1) * sqrt(rowSums(across(starts_with("replication")))),
      rel_influence = abs_influence / sqrt(sum(abs_influence^2) / (n - 1))
    ) %>%
    dplyr::select(abs_influence, rel_influence)
  
  return(cbind(jab_values, jab_influence))
}

# Finds quantiles for case-deletion estimates ==================================
bootstrap_disruption <- function(q, boots, jab_samples) {
  # Full-data quantiles --------------------------------------------------------
  bootstrap_quantiles <- boots %>%
    summarize(across(starts_with("replication"), ~quantile(., q))) %>%
    unlist()
  
  # Jackknife-after-bootstrap quantiles ----------------------------------------
  jab_quantiles <- jab_samples %>%
    transmute(quantiles = map(jab_sample,
                            ~summarize(.,
                              across(starts_with("replication"), ~quantile(.,q))
                            ) %>%
                              unlist()
                          )
    )
  
  # Calculate bootstrap disruption distance ------------------------------------
  disruption_dists <- jab_quantiles %>%
    transmute(disruption = map_dbl(quantiles, ~sum((.-bootstrap_quantiles)^2))) %>%
    pull(disruption)
  
  # Augment jackknife-after-bootstrap results and terminate --------------------
  jab_samples %>%
    mutate(disruption = disruption_dists) %>%
    return()
}

# Computes boundaries for uncertainty bands ====================================
get_uncertainty_bound <- function(q, boots, jab_samples) {
  scalar <- qchisq(
              q,
              length(str_which(colnames(jab_samples), "replication"))
            )
  return(IQR(jab_samples$disruption) * scalar)
}

# Helpers ######################################################################
# Extract row IDs from rsplit object ===========================================
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

# Get all IDs in original sample ===============================================
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

# Computes L2 norm for bootstrap estimates =====================================
l2 <- function(df) {
  reps <- colnames(df)[str_which(colnames(df), "replication")]
  y <- df
  for (col_name in reps) {
    new_col <- col_name %>%
      str_extract("(?<=replication_).*") %>%
      paste0("sq_", .)
    y <- mutate(y, !!new_col := (!!as.name(col_name))^2)
  }
  
  y <- mutate(y,
         l2_value = rowSums(across(starts_with("sq"))),
         .keep = "unused"
       )
  
  return(y)
}
