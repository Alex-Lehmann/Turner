library(shiny)

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  values$upload_flag <- FALSE
  
  # UI element definitions #####################################################
  # Procedure setup ============================================================
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
      df <- read_csv(path, col_types=cols())
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
    
    # Generate procedure specification
    if (input$param_stat == "Correlation") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_var2,
                       input$param_threshold
                     )
    } else if (input$param_stat == "Linear Regression") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_vars,
                       input$param_threshold,
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
                       input$param_threshold,
                       target = c(
                                  input$param_target1,
                                  input$param_target2,
                                  input$param_target3
                                )
                     )
    }
    else values$spec <- make_spec(
                          input$param_stat,
                          input$param_var1,
                          threshold = input$param_threshold
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
                      coefs = values$estimate
                    )
    
    # Run diagnostics ----------------------------------------------------------
    values$jab_samples <- values$boots %>%
      jackknife_after_bootstrap() %>%
      jackknife_influence() %>%
      find_quantiles()
    values$uncertainty <- get_uncertainty_bands(
      values$boots,
      values$jab_samples
    )
    values$jab_samples <- check_outliers(values$jab_samples, values$uncertainty)
    
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
})
