library(shiny)

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  values$upload_flag <- FALSE
  
  # UI element definitions #####################################################
  # Procedure setup ============================================================
  # Data selector/upload -------------------------------------------------------
  output$data_selector <- renderUI({
    fluidRow(
      column(width = 12,
        selectInput("data_select",
          "Data:",
          c("User Upload", "mtcars", "iris", "cell_survival", "galaxies")
        ),
        conditionalPanel("input.data_select == 'User Upload'",
          fileInput("user_file", "Upload CSV data", accept = ".csv")
        )
      )
    )
  })
  
  # Data preview ---------------------------------------------------------------
  output$data_preview <- renderDataTable({
    # Don't display table until something is loaded
    if (is.null(values$data)) return(NULL)
    else return(dplyr::select(values$data, -row_id))
  }, options = list(scrollX=TRUE))
  
  # Variable selectors ---------------------------------------------------------
  output$var_selector <- renderUI({
    # Define selectors
    var1_selector <- selectInput("param_var1", "var1_selector", values$col_names)
    var2_selector <- selectInput("param_var2", "var2_selector", values$col_names)
    
    vars_selector <- selectInput("param_vars",
                       "vars_selector",
                       values$col_names,
                       multiple = TRUE
                     )
    
    # Generate control
    if (input$param_stat %in% c("Correlation", "Smoothing Spline")) {
      control <- fluidRow(column(width = 12, var1_selector, var2_selector))
    } else if (input$param_stat %in% c("Linear Regression", "LOESS")) {
      control <- fluidRow(column(width = 12, var1_selector, vars_selector))
    } else control <- var1_selector
    
    return(control)
  })
  
  output$strata_selector <- renderUI({
    selectInput("param_strata", "Strata", c(" ", values$col_names))
  })
  
  # Jackknife-after-bootstrap ==================================================
  # Plot -----------------------------------------------------------------------
  output$jab_plot <- renderPlotly({
    # Calculate disruption for given quantile
    values$jab_samples <- bootstrap_disruption(
                            input$jab_quantile,
                            values$boots,
                            values$jab_samples
                          )
    
    # Calculate uncertainty for given quantile and detect outliers
    values$uncertainty <- get_uncertainty_bound(
                            input$outlier_threshold,
                            values$boots,
                            values$jab_samples
                          )
    values$jab_samples <- mutate(values$jab_samples,
                            outlier = disruption > values$uncertainty
                          )
    
    # Base Plotly object
    fig <- plot_ly(type = "scatter", showlegend = FALSE) %>%
      layout(
        title = "Jackknife-After-Bootstrap Plot",
        xaxis = list(title = "Relative Influence", zeroline = FALSE),
        yaxis = list(title = "Disruption")
      ) %>%
      config(displayModeBar = FALSE)
    
    # Sorted tibble to trace
    jab_data <- values$jab_samples %>%
      dplyr::select(deleted_case, rel_influence, disruption, outlier) %>%
      arrange(rel_influence) %>%
      set_colnames(c("deleted_case", "x", "y", "outlier"))
    
    # Uncertainty bound tibble
    uncertainty_data <- tibble(
                          x = c(0, max(jab_data$x)) * 1.05,
                          y = values$uncertainty
                        )
    
    # Add traces
    fig <- fig %>%
      # Uncertainty bound
      add_trace(
        data = uncertainty_data,
        x = ~x, y = ~y,
        name = "Uncertainty Bound",
        mode = "lines", line = list(color = "#000000", dash = "dash"),
        fill = "tozeroy", fillcolor = "rgba(0, 0, 0, 0.2)",
        hoverinfo = "skip"
      ) %>%
      
      # Jackknife quantiles
      add_trace(
        data = jab_data,
        x = ~x, y = ~y,
        mode = "markers", marker = list(size = 10),
        symbol = ~outlier, symbols = c("o", "x-thin"),
        color = "orange",
        customdata = ~deleted_case,
        hovertemplate = "ID: %{customdata}<br>Influence: %{x}<br>Disruption: %{y}"
      ) %>%
      add_trace(
        data = jab_data,
        x = ~x, y = ~y,
        mode = "lines",
        color = "orange",
        hoverinfo = "skip"
      )
    
    fig
  })
  
  # Outlier list ---------------------------------------------------------------
  output$outlier_list <- renderDataTable({
    outliers <- filter(values$jab_samples, outlier)
    
    jab_data <- outliers %>%
      dplyr::select(
        `Case No.` = deleted_case,
        `Relative Influence` = rel_influence,
        Disruption = disruption
      )
    replication_data <- outliers %>%
      dplyr::select(starts_with("replication")) %>%
      set_colnames(
        str_to_title(str_replace_all(colnames(.), "replication_", ""))
      )
    
    cbind(replication_data, jab_data) %>%
      relocate(`Case No.`) %>%
      return()
  })
  
  # Outlier boundary -----------------------------------------------------------
  output$boundary_display <- renderUI({
    HTML(paste0("<b>Outlier Boundary: ", values$uncertainty, "</b>"))
  })
  
  # UI event handlers ##########################################################
  # Procedure setup page =======================================================
  # File upload checking -------------------------------------------------------
  observeEvent(input$user_file, {
    # Check that file is a CSV; reject if not
    path <- input$user_file$datapath
    if (tools::file_ext(path) != "csv") {
      message(paste0("File ", path, " is not a .csv file."))
    } else {
      # Add row IDs and store
      df <- read_csv(path, col_types = cols())
      values$data <- mutate(df, row_id = 1:nrow(df))
      values$col_names <- colnames(df)
      values$upload_flag <- TRUE
    }
  })
  
  # Toy data input -------------------------------------------------------------
  observeEvent(input$data_select, {
    if (input$data_select != "User Upload") {
      path <- paste0("toy_data/", input$data_select, ".csv")
      df <- read_csv(path, col_types = cols())
      values$data <- mutate(df, row_id = 1:nrow(df))
      values$col_names <- colnames(df)
      values$upload_flag <- TRUE
    }
  })
  
  # Navigation button event handlers ###########################################
  # Welcome page ===============================================================
  observeEvent(input$welcome_next, {
    updateTabsetPanel(session, "wizard", "setup")
  })
  
  # Procedure setup page =======================================================
  observeEvent(input$settings_next, {
    # Set up procedure ---------------------------------------------------------
    if (!is.na(input$param_B) & input$param_B > 0) {
      values$param_B <- round(input$param_B)
    } else values$param_B <- 1000
    if (!is.na(input$param_seed) & input$param_seed > 0) {
      values$param_seed <- round(input$param_seed)
    } else values$param_seed <- NA
    if (input$param_strata == " ") values$strata <- NULL
    else values$strata <- input$param_strata
    
    # Generate procedure specification
    if (input$param_stat == "Correlation") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_var2
                     )
    } else if (input$param_stat == "Linear Regression") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_vars,
                       fit = input$param_fit
                     )
    } else if (input$param_stat == "Smoothing Spline") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_var2,
                       input$param_threshold
                     )
    } else if (input$param_stat == "LOESS") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_vars,
                       target = c(
                                  input$param_target1,
                                  input$param_target2,
                                  input$param_target3
                                )
                     )
    }
    else values$spec <- make_spec(
                          input$param_stat,
                          input$param_var1
                        )
    
    # Random seed --------------------------------------------------------------
    if (!is.na(values$param_seed)) set.seed(values$param_seed)
    else set.seed(NULL)
    
    # Execute resampling -------------------------------------------------------
    values$estimate <- point_estimate(values$data, values$spec)
    values$boots <- do_bootstrap(
                      df = values$data,
                      B = values$param_B,
                      spec = values$spec,
                      coefs = values$estimate,
                      strata = values$strata
                    )
    
    # Run diagnostics ----------------------------------------------------------
    values$jab_samples <- values$boots %>%
      jackknife_after_bootstrap() %>%
      jackknife_influence()
    
    # Bootstrap estimates ------------------------------------------------------
    values$se <- estimate_summary(values$boots, sd)
    values$bias <- map2(
                     estimate_summary(values$boots, mean),
                     values$estimate,
                     `-`
                   )
    values$skewness <- estimate_summary(values$boots, skewness)
    values$kurtosis <- estimate_summary(values$boots, kurtosis)
    
    # Display results ----------------------------------------------------------
    # Generate results modules for each estimated parameter
    for (parameter in names(values$estimate)) {
      id <- str_replace(parameter, "replication_", "")
      insertUI("#results_next", where = "beforeBegin", results_ui(id))
      results_server(id, values)
    }
    
    # Show results
    updateTabsetPanel(session, "wizard", "results")
  })
  
  # Results page ===============================================================
  observeEvent(input$results_next, {
    updateTabsetPanel(session, "wizard", "outliers")
  })
})
