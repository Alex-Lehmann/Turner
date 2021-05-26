# UI ###########################################################################
results_ui <- function(id) {
  ns = NS(id)
  
  tagList(
    wellPanel(
      fluidRow(column(width = 12, align = "center", uiOutput(ns("title")))),
      plotlyOutput(ns("histogram")),
      sliderInput(ns("alpha"),
        "Confidence Level:",
        min = 0.01, max = 0.15,
        value = 0.05,
        ticks = FALSE,
        width = "100%"
      ),
      uiOutput(ns("estimates"))
    )
  )
}

# Server #######################################################################
results_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns_values <- reactiveValues()
    
    # Title ====================================================================
    output$title <- renderUI({
      if (values$spec$stat %in% c("Mean", "Median")) {
        title <- paste0(values$spec$stat, " of ", values$spec$var)
      } else if (values$spec$stat %in% "Correlation") {
        title <- paste0(
                   values$spec$stat,
                   " Between ",
                   values$spec$var1, " and ", values$spec$var2
                 )
      } else if (values$spec$stat %in% "Linear Regression") {
        if (id == "Intercept") title <- "Intercept Term"
        else title <- paste0("Regression Coefficient for ", id)
      } else "oops"
      
      return(titlePanel(title))
    })
    
    # Histogram ================================================================
    # Histogram definition
    output$histogram <- renderPlotly({
      # Discard unneeded columns -----------------------------------------------
      if (ncol(dplyr::select(values$boots, starts_with("replication"))) > 1) {
        target_col_name <- paste0("replication_", id)
      } else target_col_name <- "replication"
      df <- values$boots %>%
        dplyr::select(splits, id, as.name(target_col_name)) %>%
        rename(replication = as.name(target_col_name))
      
      # Compute plot features --------------------------------------------------
      # Density function
      density <- density(df$replication)
      
      # Percentile confidence interval
      ns_values$interval <- df %>%
        mutate(tidy_replication = map(replication,
                                    ~tibble(
                                       term = values$spec$stat,
                                       estimate = .
                                     )
                                  )
        ) %>%
        int_pctl(tidy_replication, alpha = input$alpha) %>%
        as.list()
      
      # Generate plot ----------------------------------------------------------
      plot_ly() %>%
        config(displayModeBar = FALSE) %>%
        
        # Histogram
        add_trace(
          name = "Histogram",
          type = "histogram",
          data = df,
          x = ~replication,
          hovertemplate = "Bin range: %{x}<br>Estimates: %{y}<extra></extra>"
        ) %>%
        
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
        
        # Layout
        layout(
          title = "Distribution of Bootstrap Estimates with Confidence Interval",
          xaxis = list(title = "Bootstrap Estimate", fixedrange = TRUE),
          yaxis = list(title = "Count", fixedrange = TRUE),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "Density",
            fixedrange = TRUE
          ),
          
          # Confidence interval bounds
          shapes = c(
                     make_vlines(ns_values$interval$.estimate, dash = "dash"),
                     make_vlines(
                       ns_values$interval$.lower,
                       color = "blue",
                       dash = "dash"
                     ),
                     make_vlines(
                       ns_values$interval$.upper,
                       color = "blue",
                       dash = "dash"
                     )
                   )
        )
    })
    
    # Numerical results ========================================================
    output$estimates <- renderUI({
      # Preliminaries ----------------------------------------------------------
      
      # Get list names for statistic
      if (length(values$estimate) > 1) target_name <- paste0("replication_", id)
      else target_name <- "replication"
      
      fluidRow(
        # Estimate value -------------------------------------------------------
        column(width = 4, align = "center",
          wellPanel(
            h2("Estimates"),
            hr(),
            
            h3("Point Estimate"),
            h4(values$estimate[[target_name]]),
            
            h3(paste0((1 - input$alpha) * 100), "% Interval Estimate"),
            h4(paste0(
                 "(", ns_values$interval$.lower,
                 ", ",
                 ns_values$interval$.upper, ")"
               )
            )
          )
        ),
        
        # Summary statistics ---------------------------------------------------
        column(width = 8, align = "center",
          wellPanel(
            h2("Distribution Summary Statistics"),
            hr(),
            
            fluidRow(
              column(width = 6, align = "center",
                h3("Standard Error"),
                h4(values$se[[target_name]]),
                
                h3("Bias"),
                h4(values$bias[[target_name]])
              ),
              column(width = 6, align = "center",
                h3("Skewness"),
                h4(values$skewness[[target_name]]),
                
                h3("Kurtosis"),
                h4(values$kurtosis[[target_name]])
              )
            )
          )
        )
      )
    })
  })
}

# Helpers ######################################################################
# Function to make a vertical line on a Plotly scatterplot =====================
make_vlines = function(x, color = "#000000", dash = NULL) {
  l = list()
  for (x_intercept in x) {
    l = append(l, list(list(
      type = "line",
      x0 = x_intercept, x1 = x_intercept,
      y0 = 0, y1 = 1, yref = "paper",
      line = list(color = color, dash = dash)
                       )
                  )
        )
  }
  return(l)
}
