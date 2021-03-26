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
    values$boot_reps0 = boot_results$reps
    values$boot_stat0 = boot_results$stat
    
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
    values$jab_values = jackknife_after_bootstrap(values$boot_reps0)
    values$jab_uncertainty = get_uncertainty_bands(
                               values$boot_reps0,
                               values$jab_values
                             )
    values$outliers = check_outliers(values$jab_values, values$jab_uncertainty)
    # Navigate to outlier page
    updateTabsetPanel(session, "wizard", selected = "outlier_detection")
    remove_modal_spinner()
  })
  
  observeEvent(input$boot_results_previous, {
    updateTabsetPanel(session, "wizard", selected = "settings")
  })
  
  # Outlier detection page -----------------------------------------------------
  observeEvent(input$outliers_next, {
    
  })
  
  observeEvent(input$outliers_previous, {
    updateTabsetPanel(session, "wizard", selected = "boot_results")
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
    # Compute density
    density = density(values$boot_reps0$estimate)
    
    # Compute pivot confidence interval bounds
    ci_values = make_pivot_ci(values$boot_reps0, "estimate", input$ci_alpha)
    values$ci = tibble(
                  estimate = ci_values[1],
                  lower = ci_values[2],
                  upper = ci_values[3]
                )
    
    # Generate lines for confidence interval
    ci_vector = unlist(values$ci)
    vlines = make_vlines(ci_vector[1], dash = "dash") %>% 
      append(make_vlines(ci_vector[2:3], color = "blue", dash = "dash"))
    
    # Generate plot
    fig = plot_ly() %>%
      config(displayModeBar = FALSE) %>%
      
      # Density
      add_trace(
        name = "Density",
        type = "scatter",
        mode = "lines",
        x = density$x, y = density$y,
        yaxis = "y2",
        fill = "tozeroy",
        hoverinfo = "skip"
      ) %>%
      
      # Histogram
      add_trace(
        name = "Histogram",
        type = "histogram",
        data = values$boot_reps0,
        x = ~estimate,
        hovertemplate = "Bin range: %{x}<br>Estimates: %{y}<extra></extra>"
      ) %>%
      
      # Layout
      layout(
        title = "Bootstrap Estimates with Pivot Confidence Interval",
        xaxis = list(title = "Bootstrap Estimate", fixedrange = TRUE),
        yaxis = list(title = "Count", fixedrange = TRUE),
        yaxis2 = list(
                   overlaying = "y",
                   side = "right",
                   title = "Density",
                   fixedrange = TRUE
                 ),
        shapes = vlines
      )
    
    fig
  })
  
  # Numeric results ------------------------------------------------------------
  output$results_summary = renderUI({
    # Maximum decimals in passed data for rounding purposes
    decimals = values$user_file %>%
      pull(as.name(values$param_variable)) %>%
      count_decimals() %>%
      max()
    
    fluidRow(
      # Summary statistics -----------------------------------------------------
      column(width = 6, align = "center",
        h3("Mean Estimate"),
        h4(round(values$boot_stat0, decimals + 1)),
        h3("Standard Error"),
        h4(round(sd(values$boot_reps0$estimate), decimals + 1))
      ),
      # Confidence interval ----------------------------------------------------
      column(width = 6, align = "center",
        h3(paste0((1 - input$ci_alpha) * 100), "% Pivot Confidence Interval"),
        h4(paste0(
             "(", round(values$ci$lower, decimals + 1)
             , ", ", 
             round(values$ci$upper, decimals + 1), ")")
        ),
        
        h3("Skewness"),
        h4(round(skewness(values$boot_reps0$estimate), decimals + 3))
      )
    )
  })
  
  # Outlier detection ==========================================================
  # Jackknife-after-bootstrap plot ---------------------------------------------
  output$jab_plot = renderPlotly({
    # Base Plotly object
    fig = plot_ly(type = "scatter", showlegend = FALSE) %>%
      layout(
        title = "Jackknife-After-Bootstrap Plot",
        xaxis = list(title = "Relative Influence", zeroline = FALSE),
        yaxis = list(title = "Value")
      ) %>%
      config(displayModeBar = FALSE)
    
    for (q in str_subset(colnames(values$jab_values), "^quantile_")) {
      # Extract quantile value from name
      quantile_prob = q %>%
        str_extract("(?<=_).*") %>%
        as.numeric()
      
      # Sorted tibble to trace
      jack_data = values$jab_values %>%
        mutate(outlier = ifelse(
                           deleted_case %in% values$outliers,
                           TRUE, FALSE
                         )
               ) %>%
        select(deleted_case, rel_influence, as.name(q), outlier) %>%
        arrange(rel_influence)
      colnames(jack_data) = c("deleted_case", "x", "y", "outlier")
      
      # Get uncertainty for this quantile and make Plotly-friendly
      uncertainty_interval = filter(
                               values$jab_uncertainty,
                               quantile == quantile_prob
                             )
      x = values$jab_values$rel_influence
      x_bounds = c(min(x), max(x)) * 1.05
      uncertainty_data = tibble(
                           x = x_bounds,
                           quantile = rep(uncertainty_interval$full_quantile,2),
                           upper = rep(uncertainty_interval$upper, 2),
                           lower = rep(uncertainty_interval$lower, 2)
                         )
      
      # Add traces
      fig = fig %>%
        # Uncertainty bands
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~quantile,
          name = paste0("Full-data ", quantile_prob * 100, "% quantile"),
          mode = "lines",line = list(color = "#000000", dash = "dash"),
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~upper,
          mode = "lines", line = list(color = "transparent"),
          fillcolor = "rgba(0, 0, 0, 0.2)",
          hoverinfo = "skip"
        ) %>%
        add_trace(
          data = uncertainty_data,
          x = ~x, y = ~lower,
          mode = "lines", line = list(color = "transparent"),
          fill = "tonexty", fillcolor = "rgba(0, 0, 0, 0.2)",
          hoverinfo = "skip"
        ) %>%
        
        # Jackknife quantiles
        add_trace(
          data = jack_data,
          name = paste0(quantile_prob * 100, "% quantile"),
          x = ~x,
          y = ~y,
          mode = "markers", marker = list(size = 10),
          symbol = ~outlier, symbols = c("o", "x-thin"),
          color = "orange",
          customdata = ~deleted_case,
          hovertemplate = "ID: %{customdata}<br>Influence: %{x}<br>Value: %{y}"
        ) %>%
        add_trace(
          data = jack_data,
          x = ~x,
          y = ~y,
          mode = "lines",
          color = "orange",
          hoverinfo = "skip"
        )
    }
    
    values$jab_plot = fig
    fig
  })
  
  # Outlier display ------------------------------------------------------------
  output$outliers = DT::renderDataTable({
    if (length(values$outliers > 0)){
      values$outlier_table = values$user_file %>%
        filter(row_id %in% values$outliers)
    } else NULL
  }, selection = list(selected = 1:length(values$outliers)),
     options = list(scrollX = TRUE, ordering = FALSE))
  
  # EHs for outlier point selection --------------------------------------------}
  # From plot
  observeEvent(event_data("plotly_click"), {
    clicked_marker = event_data("plotly_click")
    
    # Check if clicked marker is an outlier
    click_id = clicked_marker$customdata
    if (is.null(click_id)) return()
    
    table_ids = values$outlier_table$row_id
    if (click_id %in% table_ids) {
      selection_ids = table_ids[input$outliers_rows_selected]
      if (click_id %in% selection_ids) {
        # If case is selected, deselect
        selection_index = which(selection_ids == click_id)
        dataTableProxy("outliers") %>%
          selectRows(input$outliers_rows_selected[-selection_index])
      } else {
        # If case is not selected, select
        click_index = which(table_ids == click_id)
        dataTableProxy("outliers") %>%
          selectRows(c(input$outliers_rows_selected, click_index))
      }
    }
  })
  
  # From table
  observeEvent(input$outliers_rows_selected, ignoreNULL = FALSE, {
    if (!is.null(input$outliers_rows_selected)) {
      # Make new vertical lines
      vlines = values$jab_values %>%
        filter(deleted_case %in% values$outliers) %>%
        slice(input$outliers_rows_selected) %>%
        pull(rel_influence) %>%
        make_vlines(dash = "dot")
    } else vlines = NULL
    
    # Update layout without any vertical lines
    plotlyProxy("jab_plot", session) %>%
      plotlyProxyInvoke("relayout",
        list(
          title = "Jackknife-After-Bootstrap Plot",
          xaxis = list(title = "Relative Influence", zeroline = FALSE),
          yaxis = list(title = "Value"),
          shapes = vlines
        )
      )
  })
})
