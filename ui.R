library(shiny)

shinyUI(fluidPage(
  navbarPage(title = "Turner (WIP)",
    tabPanel("Bootstrap",
      tabsetPanel(type = "hidden", id = "wizard", selected = "welcome",
        # Welcome page #########################################################
        tabPanelBody("welcome",
          welcome_ui("default"),
          actionButton("welcome_next", "Start", width = "100%")
        ),
        
        # Procedure setup ######################################################
        tabPanelBody("setup",
          titlePanel("Procedure Setup"),
          
          # Data upload ========================================================
          h3("Data"),
          uiOutput("data_selector"),
          dataTableOutput("data_preview"),
          hr(),
          
          # Procedure setup ====================================================
          h3("Bootstrap Settings"),
          fluidRow(
            # Bootstrap parameters ---------------------------------------------
            column(width = 4,
              h4("Bootstrap Parameters"),
              numericInput("param_B",
                "Bootstrap Samples:",
                min = 1,
                value = 1000,
                step = 1
              ),
              numericInput("param_seed",
                "Random Seed:",
                min = 1,
                value = NULL,
                step = 1
              )
            ),
            
            # Model selection --------------------------------------------------
            column(width = 4,
              h4("Model/Statistic Selection"),
              selectInput("param_stat",
                "Model/Statistic:",
                c(
                  "Mean", "Median", "Correlation", # Summary statistics
                  "Linear Regression", "Smoothing Spline", "LOESS" # Regressions
                )
              ),
              
              # Linear regression-specific
              conditionalPanel("input.param_stat == 'Linear Regression'",
                selectInput("param_fit",
                  "Fit Method:",
                  c(
                    "Ordinary Least Squares",
                    "Iteratively Re-Weighted Least Squares"
                  )
                )
              ),
              
              # LOESS-specific
              conditionalPanel("input.param_stat == 'LOESS'",
                numericInput("param_target1", "target1_value", NULL),
                conditionalPanel("input.param_vars.length > 1",
                  numericInput("param_target2", "target2_value", NULL)
                ),
                conditionalPanel("input.param_vars.length > 2",
                  numericInput("param_target3", "target3_value", NULL)
                )
              )
            ),
            
            # Variable selection -----------------------------------------------
            column(width = 4,
              h4("Variable Selection"),
              uiOutput("var_selector"),
              uiOutput("strata_selector")
            )
          ),
          
          # Go button ----------------------------------------------------------
          actionButton("settings_next", "Compute", width = "100%")
        ),
        
        # Results ##############################################################
        tabPanelBody("results",
          titlePanel("Results"),
          actionButton("results_next", "Next", width = "100%")
        ),
        
        # Jackknife-after-bootstrap ############################################
        tabPanelBody("outliers",
          titlePanel("Case Influence and Outliers"),
          
          # Plot ===============================================================
          plotlyOutput("jab_plot"),
          fluidRow(
            column(width = 6,
              sliderInput("jab_quantile",
                "Sample Quantile:",
                min = 0, max = 1,
                value = 0.5,
                ticks = FALSE,
                width = "100%"
              )
            ),
            column(width = 6,
              sliderInput("outlier_threshold",
                "Outlier Threshold:",
                min = 0, max = 1,
                value = 0.98,
                ticks = FALSE,
                width = "100%"
              )
            )
          ),
          
          # Outlier list =======================================================
          dataTableOutput("outlier_list"),
          uiOutput("boundary_display")
        )
      )
    )
  )
))
