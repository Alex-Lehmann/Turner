library(shiny)

shinyServer(function(input, output, session) {
  values = reactiveValues()
  
  # Wizard navigation buttons ==================================================
  # Welcome page ---------------------------------------------------------------
  observeEvent(input$welcome_next, {
    updateTabsetPanel(session, "wizard", selected = "data_upload")
  })
  # Data upload page -----------------------------------------------------------
  observeEvent(input$data_upload_next, {
    updateTabsetPanel(session, "wizard", selected = "settings")
  })
  # Settings page --------------------------------------------------------------
  observeEvent(input$settings_next, {
    # Show busy dialog
    show_modal_spinner(spin = "swapping-squares", text = "Resampling...")
    
    # Store settings for reporting
    values$param_B = input$param_B
    values$param_statistic = input$param_statistic
    values$param_variable = input$param_variable
    
    # Resample
    boot_results = do_bootstrap(
      values$user_file,
      B = input$param_B,
      fn = input$param_statistic,
      variable = input$param_variable)
    values$boot_reps = boot_results$reps
    values$boot_stat = boot_results$stat
    
    # Navigate to results
    updateTabsetPanel(session, "wizard", selected = "boot_results")
    remove_modal_spinner()
  })
  
  observeEvent(input$settings_previous, {
    updateTabsetPanel(session, "wizard", selected = "data_upload")
  })
  # Bootstrap results page -----------------------------------------------------
  observeEvent(input$boot_results_next, {
    # Show busy dialog
    show_modal_spinner(
      spin = "swapping-squares",
      text = "Searching for outliers..."
    )
    # Check for outliers
    values$jab_values = jackknife_after_bootstrap(values$boot_reps)
    values$jab_uncertainty = get_uncertainty_bands(
                               values$boot_reps,
                               values$jab_values
                             )
    # Navigate to outlier page
    updateTabsetPanel(session, "wizard", selected = "outlier_detection")
    remove_modal_spinner()
  })
  
  observeEvent(input$boot_results_previous, {
    updateTabsetPanel(session, "wizard", selected = "settings")
  })
  
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
  # Data preview ---------------------------------------------------------------
  output$data_preview = DT::renderDataTable({
    # Don't display table until something is loaded
    if (is.null(values$user_file)) return(NULL) else return(values$user_file)
  }, options=list(scrollX=TRUE))
  
  # Data page conditional action button ----------------------------------------
  output$data_upload_next = renderUI({
    ui = NULL
    if (!is.null(values$user_file)) {
      ui = fluidRow(
        column(width = 12,
          hr(),
          actionButton("data_upload_next", "Next", width = "100%")
        )
      )
    }
    return(ui)
  })
  
  # Statistic selection --------------------------------------------------------
  output$select_variable = renderUI({
    # Get variables from loaded data, if present
    if (!is.null(values$user_file)) choices = colnames(values$user_file)
    else choices = NULL
    
    # Generate input
    selectInput("param_variable", "Variable:", choices)
  })
  
  # Results ====================================================================
  # Histogram ------------------------------------------------------------------
  output$results_hist = renderPlotly({
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
  output$results_summary = renderUI({
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
  output$jab_plot = renderPlotly({
    # Base Plotly object
    fig = plot_ly(type = "scatter")
    
    for (q in str_subset(colnames(values$jab_values), "^quantile_")) {
      # Extract quantile value from name
      quantile_prob = q %>%
        str_extract("(?<=_).*") %>%
        as.numeric()
      
      # Sorted tibble to trace
      jack_data = values$jab_values %>%
        select(rel_influence, as.name(q)) %>%
        arrange(rel_influence)
      colnames(jack_data) = c("x", "y")
      
      # Get uncertainty for this quantile and make Plotly-friendly
      uncertainty_interval = filter(
                               values$jab_uncertainty,
                               quantile == quantile_prob
                             )
      x = values$jab_values$rel_influence
      x_bounds = c(min(x), max(x))
      uncertainty_data = tibble(
                           x = x_bounds,
                           quantile = rep(uncertainty_interval$full_quantile,2),
                           upper = rep(uncertainty_interval$upper, 2),
                           lower = rep(uncertainty_interval$lower, 2)
                         )
      
      # Generate plot
      fig = fig %>%
        # Uncertainty bands
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~quantile,
          name = paste0("Full-data ", quantile_prob * 100, "% quantile"),
          mode = "lines",
          line = list(color = "#000000", dash = "dash"),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~upper,
          mode = "lines",
          line = list(color = "transparent"),
          fillcolor = "rgba(0, 0, 0, 0.2)",
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~lower,
          mode = "lines",
          line = list(color = "transparent"),
          fill = "tonexty", fillcolor = "rgba(0, 0, 0, 0.2)",
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        # Jackknife quantiles
        add_trace(
          data = jack_data,
          x = ~x,
          y = ~y,
          name = paste0(quantile_prob * 100, "% quantile"),
          mode = "lines+markers",
          color = "orange",
          hovertemplate = "Influence: %{x}<br>Value: %{y}"
        ) %>%
        # Axes
        layout(
          title = "Jackknife-After-Bootstrap Plot",
          xaxis = list(title = "Relative Influence"),
          yaxis = list(title = "Value")
        )
    }
    
    fig
  })
})
