# Estimation ###################################################################
estimate_lm <- function(df, spec) {
  # Construct model formula and fit
  model <- as.formula(paste0(
    spec$response,
    " ~ ",
    str_c(spec$predictors, collapse = " + ")
  )
  )
  
  fit <- switch(spec$fit,
                "Ordinary Least Squares" = lm(model, df),
                "Iteratively Re-Weighted Least Squares" = rlm(model, df)
  )
  
  # Extract coefficients from fit
  coefs <- fit$coefficients %>%
    as.list() %>%
    set_names(
      names(.) %>%
        str_replace_all("[^[[:alnum:]]]", "") %>%
        paste0("replication_", .)
    )
  
  return(coefs)
}

# Bootstrap evaluation #########################################################
eval_lm <- function(df, spec) {
  replications <- df %>%
    mutate(replication = map(sample, estimate_lm, spec = spec)) %>%
    unnest_wider(replication) %>%
    dplyr::select(starts_with("replication_"))
  
  augmented_df <- df
  for (col_name in colnames(replications)) {
    augmented_df <- mutate(augmented_df, !!col_name := replications[[col_name]])
  }
  
  return(augmented_df)
}

# Procedure decision ###########################################################
choose_lm_proc <- function(df, spec) {
  # Regenerate model -----------------------------------------------------------
  # Construct model formula and fit
  model <- as.formula(paste0(
                        spec$response,
                        " ~ ",
                        str_c(spec$predictors, collapse = " + ")
                     )
           )
  fit <- switch(spec$fit,
                "Ordinary Least Squares" = lm(model, df),
                "Iteratively Re-Weighted Least Squares" = rlm(model, df)
         )
  
  # White's test for heteroskedasticity ----------------------------------------
  test <- white_lm(fit)
  if (test$p.value > 0.05) return(resample_residuals) # Homoskedastic residuals
  else return(resample_cases) # Heteroskedastic residuals
}
