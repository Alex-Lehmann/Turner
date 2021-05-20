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
          fileInput("user_file", "Upload CSV data", accept = ".csv"),
          dataTableOutput("data_preview"),
          hr(),
          
          # Procedure setup ====================================================
          h3("Bootstrap Settings"),
          fluidRow(
            # Bootstrap parameters ---------------------------------------------
            column(width = 6,
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
            
            # Model-specific controls ------------------------------------------
            column(width = 6,
              h4("Model/Statistic Selection"),
              selectInput("param_stat",
                "Model/Statistic:",
                c("Mean", "Median", "Correlation")
              ),
              uiOutput("var_selector")
            )
          ),
          
          # Go button ----------------------------------------------------------
          actionButton("settings_next", "Compute", width = "100%")
        )
      )
    )
  )
))
