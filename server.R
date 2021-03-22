library(shiny)

shinyServer(function(input, output, session) {
  values = reactiveValues()
  
  # Data ingest ================================================================
  observeEvent(input$user_file, {
    # Check that file is a CSV; reject if not
    path = input$user_file$datapath
    if (tools::file_ext(path) != "csv") {
      message(paste0("File ", path, " is not a .csv file."))
      return()
    } else {
      # Add row IDs
      df = readr::read_csv(path, col_types=cols())
      values$user_file = mutate(df, row_id = 1:nrow(df))
    }
  })
  
  # UI objects =================================================================
  # Statistic selection --------------------------------------------------------
  output$select_variable = renderUI({
    # Get variables from loaded data, if present
    if (!is.null(values$user_file)) choices = colnames(values$user_file)
    else choices = NULL
    
    # Generate input
    selectInput("param_variable", "Variable:", choices)
  })
  
  # Data preview ---------------------------------------------------------------
  output$data_preview = DT::renderDataTable({
    # Don't display table until something is loaded
    if (is.null(values$user_file)) return(NULL) else return(values$user_file)
  }, options=list(scrollX=TRUE))
  
  # Bootstrap procedure ========================================================
  observeEvent(input$go, {
    # Store settings for reporting
    values$param_B = input$param_B
    values$param_statistic = input$param_statistic
    values$param_variable = input$param_variable
    
    # Generate bootstrap samples
    values$boot_samples = bootstraps(
                            values$user_file,
                            times = values$param_B,
                            apparent=TRUE
                          )
    
    # Helper function to compute statistic
    compute_statistic = function(split, variable, fn) {
      analysis(split) %>%
        pull(as.name(variable)) %>%
        fn(na.rm = TRUE)
    }
    
    # Compute statistic on boostrap samples
    statistic = switch(values$param_statistic,
                  "Mean" = mean,
                  "Median" = median
                )
    values$boot_reps = values$boot_samples %>%
      mutate(
        model = map(
          splits,
          compute_statistic,
          variable = values$param_variable,
          fn = statistic
        )
      ) %>%
      unnest(model) %>%
      rename(estimate = model)
    
    # Compute mean estimate
    values$boot_stat = mean(values$boot_reps$estimate)
    
    # Jackknife-after-bootstrap for outlier detection
    values$jab_values = jackknife_after_bootstrap(values$boot_reps)
  })
  
  # Results ====================================================================
  # Histogram ------------------------------------------------------------------
  output$boots_histogram = renderPlotly({
    # Compute pivot confidence interval bounds
    upper_quantile = quantile(
                       values$boot_reps$estimate,
                       probs = 1 - (input$ci_alpha / 2),
                       na.rm = TRUE
                     )
    lower_quantile = quantile(
                       values$boot_reps$estimate,
                       probs = input$ci_alpha / 2,
                       na.rm = TRUE
                     )
    values$ci = tibble(
                  estimate = values$boot_stat,
                  upper = 2 * values$boot_stat - lower_quantile,
                  lower = 2 * values$boot_stat - upper_quantile
                )
    
    # Generate plot
    fig = ggplot() +
      geom_histogram(values$boot_reps, mapping = aes(estimate)) +
      geom_vline(
        values$ci,
        mapping = aes(xintercept = estimate, color = "Mean")) +
      geom_vline(values$ci, mapping = aes(xintercept = upper, color = "CI")) +
      geom_vline(values$ci, mapping = aes(xintercept = lower, color = "CI")) +
      ggtitle("Histogram of Bootstrap Estimates, with Pivot CI") +
      xlab(paste0("Bootstrap Estimate of ", input$param_statistic)) +
      ylab("Count")
    ggplotly(fig)
  })
  
  # Numeric results ------------------------------------------------------------
  output$boots_results = renderUI({
    fluidRow(
      column(width=12, align="center",
        h2("Bootstrap Results"),
        
        # Estimates ------------------------------------------------------------
        h3("Estimate"),
        h4(values$boot_stat),
        h3(paste0((1 - input$ci_alpha)*100, "% Pivot CI")),
        h4(paste0("(", values$ci$lower, ", ", values$ci$upper, ")")),
        
        # Settings -------------------------------------------------------------
        HTML("<br>"),
        h3("Settings"),
        h4(paste0(values$param_B, " samples")),
        h4(paste0("Statistic: ", values$param_statistic)),
        h4(paste0("Variable: ", values$param_variable))
      )
    )
  })
  
  # Outlier detection ==========================================================
  # Jackknife-after-bootstrap plot ---------------------------------------------
  output$jab_plot = renderPlot({
    fig = ggplot(values$jab_values, aes(x = rel_influence)) +
      
      # Quantiles
      geom_point(aes(y = percentile_0.05)) +
      geom_line(aes(y = percentile_0.05)) +
      geom_point(aes(y = percentile_0.5)) +
      geom_line(aes(y = percentile_0.5)) +
      geom_point(aes(y = percentile_0.95)) +
      geom_line(aes(y = percentile_0.95))
    fig
  })
})
