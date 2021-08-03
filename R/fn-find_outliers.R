# Detects potential outliers ===================================================
find_outliers <- function(df, spec) {
  # Compute decision boundary --------------------------------------------------
  p <- sum(str_detect(colnames(df), "replication"))
  boundary <- 2 * qchi(spec$threshold, p) * mad(df$rel_influence)
  
  # Flag potential outliers ----------------------------------------------------
  return(mutate(df, is_outlier = rel_influence > boundary))
}
