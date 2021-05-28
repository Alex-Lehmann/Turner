# Estimation ###################################################################
estimate_mean <- function(df, spec) { mean(pull(df, as.name(spec$var))) }

# Bootstrap evaluation #########################################################
eval_mean <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_mean, spec = spec))
}