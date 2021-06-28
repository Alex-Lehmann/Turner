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
  df %>%
    mutate(
      jab_estimate = map(jab_sample,
                       ~summarize(., across(starts_with("replication"), mean))
                     )
    ) %>%
    unnest_wider(jab_estimate) %>%
    
    # Compute influence values -------------------------------------------------
    mutate(
      across(starts_with("replication"), ~(. - mean(.))^2),
      abs_influence = (n - 1)*sqrt(rowSums(across(starts_with("replication")))),
      rel_influence = abs_influence / sqrt(sum(abs_influence^2) / (n - 1))
    ) %>%
    return()
}

# Finds quantiles for case-deletion estimates ==================================
find_quantiles <- function(df) {
  mutate(df,
    # Compute L2 distance for bootstrap estimates ------------------------------
    jab_sample = map(jab_sample, l2),
    
    # Find 5%, 50%, and 95% L2 error quantiles ---------------------------------
    quantile_0.05 = map_dbl(jab_sample, ~quantile(.$l2_value, 0.05)),
    quantile_0.50 = map_dbl(jab_sample, ~quantile(.$l2_value, 0.5)),
    quantile_0.95 = map_dbl(jab_sample, ~quantile(.$l2_value, 0.95))
  )
}

# Computes boundaries for uncertainty bands ====================================
get_uncertainty_bands <- function(df_boots, df_jab_samples) {
  # Find full-data bootstrap quantiles -----------------------------------------
  boots <- mutate(df_boots,
             across(starts_with("replication"), ~.^2),
             l2_value = rowSums(across(starts_with("replication")))
           )
  uncertainty_bands <- tibble(
                         quantile = c(0.05, 0.5, 0.95),
                         center = quantile(
                                    boots$l2_value,
                                    probs = c(0.05, 0.5, 0.95)
                                  )
                       )
  
  # Find band widths and construct upper/lower bounds --------------------------
  p <- length(str_which(colnames(df_jab_samples), "replication"))
  scalar <- qchi(0.95, p)
  uncertainty_bands$width <- df_jab_samples %>%
    summarize(across(starts_with("quantile"), ~IQR(.) * scalar)) %>%
    unlist()
  uncertainty_bands <- mutate(uncertainty_bands,
                         lower = center - width,
                         upper = center + width
                       )
  
  return(uncertainty_bands)
}

# Checks for outlier points and flags them =====================================
check_outliers <- function(df, uncertainty) {
  jab_samples <- mutate(df, outlier = 0)
  for (col_name in c("quantile_0.05", "quantile_0.50", "quantile_0.95")) {
    prob <- as.numeric(str_extract(col_name, "(?<=quantile_).*"))
    band <- filter(uncertainty, quantile == prob)
    center <- band$center
    width <- band$width
    
    jab_samples <- mutate(jab_samples,
                     outlier = outlier +
                               (abs(!!as.name(col_name) - center) > width)
                   )
  }
  
  jab_samples <- mutate(jab_samples, outlier = outlier >= 1)
  
  return(jab_samples)
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
