boot_results_ui = function(id) {
  ns = NS(id)
  
  tagList(
    plotlyOutput(ns("results_hist")),
    sliderInput(ns("ci_alpha"),
      "Confidence level:",
      min = 0.01, max = 0.15,
      value = 0.05,
      ticks = FALSE,
      width = "100%"
    ),
    uiOutput(ns("results_summary"))
  )
}

boot_results_server = function(id, values) {
  moduleServer(id, function(input, output, session) {
    # Histogram ----------------------------------------------------------------
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
    
    # Numeric results ----------------------------------------------------------
    output$results_summary = renderUI({
      # Maximum decimals in passed data for rounding purposes
      decimals = values$user_file %>%
        pull(as.name(values$param_variable)) %>%
        count_decimals() %>%
        max()
      
      fluidRow(
        # Summary statistics ---------------------------------------------------
        column(width = 6, align = "center",
          h3("Mean Estimate"),
          h4(round(values$boot_stat0, decimals + 1)),
          
          h3("Standard Error"),
          h4(round(sd(values$boot_reps0$estimate), decimals + 1))
        ),
        # Confidence interval --------------------------------------------------
        column(width = 6, align = "center",
          h3(paste0((1 - input$ci_alpha) * 100), "% Pivot Confidence Interval"),
          h4(paste0(
               "(", round(values$ci$lower, decimals + 1)
               , ", ", 
               round(values$ci$upper, decimals + 1), ")"
             )
          ),
          
          h3("Skewness"),
          h4(round(skewness(values$boot_reps0$estimate), decimals + 3))
        )
      )
    })
  })
}