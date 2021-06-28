# Generates procedure specification ============================================
make_spec <- function(stat,
                      var1,
                      vars = NULL,
                      threshold,
                      fit = NULL,
                      target = NULL) {
  spec <- switch(stat,
            "Mean" = list(stat = stat, var = var1, threshold = threshold),
            "Median" = list(stat = stat, var = var1, threshold = threshold),
            "Correlation" = list(
                              stat = stat,
                              var1 = var1,
                              var2 = vars,
                              threshold = threshold
                            ),
            "Linear Regression" = list(
                                    stat = stat,
                                    response = var1,
                                    predictors = vars,
                                    threshold = threshold,
                                    fit = fit
                                  ),
            "Smoothing Spline" = list(
                                   stat = stat,
                                   response = var1,
                                   predictor = vars,
                                   threshold = threshold
                                 ),
            "LOESS" = list(
                        stat = stat,
                        response = var1,
                        predictors = vars,
                        threshold = threshold,
                        target = target[which(!is.na(target))]
                      )
          )
  
  return(spec)
}
