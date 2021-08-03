# Estimation ###################################################################
estimate_spline <- function(df, spec) {
  model <- list(x = df[[spec$predictor]], y = df[[spec$response]])
  fit <- smooth.spline(model, all.knots = TRUE)
  
  return(fit$lambda)
}

# Bootstrap evaluation #########################################################
eval_spline <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_spline, spec = spec))
}
