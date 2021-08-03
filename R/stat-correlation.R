# Estimation ###################################################################
estimate_correlation <- function(df, spec) {
  cor_mat <- df %>%
    dplyr::select(spec$var1, spec$var2) %>%
    cor()
  return(cor_mat[1,2])
}

# Bootstrap evaluation #########################################################
eval_correlation <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_correlation, spec = spec))
}
