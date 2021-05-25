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
    var1_selector <- selectInput("param_var1", "Variable 1:", values$col_names)
    var2_selector <- selectInput("param_var2", "Variable 2:", values$col_names)
    
    vars_selector <- selectInput("param_vars",
                       "Predictors:",
                       values$col_names,
                       multiple = TRUE
                     )
    
    # Generate control
    if (input$param_stat %in% "Correlation") {
      control <- fluidRow(column(width = 12, var1_selector, var2_selector))
    } else if (input$param_stat %in% "Linear Regression") {
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
    if (input$param_stat %in% "Correlation") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_var2
                     )
    } else if (input$param_stat %in% "Linear Regression") {
      values$spec <- make_spec(
                       input$param_stat,
                       input$param_var1,
                       input$param_vars,
                       input$param_fit
                     )
    } else values$spec <- make_spec(input$param_stat, input$param_var1)
    
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
    values$jab_samples <- jackknife_after_bootstrap(values$boots)
    
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
      print(id)
      
      insertUI(selector = "#add", where = "afterEnd", results_ui(id))
      results_server(id, values)
    }
    
    # Show results
    updateTabsetPanel(session, "wizard", "results")
  })
})
