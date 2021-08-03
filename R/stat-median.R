# Estimation ###################################################################
estimate_median <- function(df, spec) { median(pull(df, as.name(spec$var))) }

# Bootstrap evaluation #########################################################
eval_median <- function(df, spec) {
  mutate(df, replication = map_dbl(sample, estimate_median, spec = spec))
}
