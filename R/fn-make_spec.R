# Generates procedure specification ============================================
make_spec <- function(stat, var1, vars = NULL, fit = NULL, target = NULL) {
  switch(stat,
         "Mean" = list(stat = stat, var = var1),
         "Median" = list(stat = stat, var = var1),
         "Correlation" = list(stat = stat, var1 = var1, var2 = vars),
         "Linear Regression" = list(
           stat = stat,
           response = var1,
           predictors = vars,
           fit = fit
         ),
         "Smoothing Spline" = list(
           stat = stat,
           response = var1,
           predictor = vars
         ),
         "LOESS" = list(
           stat = stat,
           response = var1,
           predictors = vars,
           target = target[which(!is.na(target))]
         )
  )
}
