# Estimation ###################################################################
estimate_loess <- function(df, spec) {
  model <- as.formula(paste0(
    spec$response,
    " ~ ",
    str_c(spec$predictors, collapse = " + ")
  )
  )
  fit <- loess(model, df)
  
  return(predict(fit, matrix(spec$target, nrow = 1)))
}

# Bootstrap evaluation #########################################################
eval_loess <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_loess, spec = spec))
}
