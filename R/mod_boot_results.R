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

boot_results_server = function(id, values, i) {
  moduleServer(id, function(input, output, session) {
    # Histogram ----------------------------------------------------------------
    output$results_hist = renderPlotly({
      boot_reps = values[[paste0("boot_reps", i)]]
      
      # Compute density
      density = density(boot_reps$estimate)
      
      # Compute pivot confidence interval bounds
      ci_values = make_pivot_ci(boot_reps, "estimate", input$ci_alpha)
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
          data = boot_reps,
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
      boot_reps = values[[paste0("boot_reps", i)]]
      user_file = values[[paste0("user_file", i)]]
      boot_stat = values[[paste0("boot_stat", i)]]
      # Maximum decimals in passed data for rounding purposes
      decimals = user_file %>%
        pull(as.name(values$param_variable)) %>%
        count_decimals() %>%
        max()
      
      fluidRow(
        # Summary statistics ---------------------------------------------------
        column(width = 6, align = "center",
          h3("Mean Estimate"),
          h4(round(boot_stat, decimals + 1)),
          
          h3("Standard Error"),
          h4(round(sd(boot_reps$estimate), decimals + 1))
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
          h4(round(skewness(boot_reps$estimate), decimals + 3))
        )
      )
    })
  })
}