library(shiny)

shinyUI(fluidPage(
  navbarPage(title="Turner (WIP)",
    tabPanel("Bootstrap",
      tabsetPanel(type = "hidden", id = "wizard", selected = "welcome",
        # Welcome page =========================================================
        tabPanelBody("welcome",
          welcome_ui("welcome"),
          actionButton("welcome_next", "Start", width="100%")
        ),
        
        # Data upload page =====================================================
        tabPanelBody("data_upload",
          titlePanel("Data"),
          fileInput("user_file", "Upload your data as a CSV", accept = ".csv"),
          dataTableOutput("data_preview"),
          uiOutput("data_upload_next")
        ),
        
        # Bootstrap setup page =================================================
        tabPanelBody("settings",
          titlePanel("Bootstrap Setup"),
          # Settings -----------------------------------------------------------
          numericInput(
            "param_B",
            "Number of bootstrap samples:",
            min=1,
            value=1000,
            step=1,
          ),
          selectInput("param_statistic", "Statistic:", c("Mean", "Median")),
          uiOutput("select_variable"),
          numericInput(
            "param_seed",
            "Random Seed",
            min = 0,
            value = NULL,
            step = 1
          ),
          hr(),
          # Navigation ---------------------------------------------------------
          fluidRow(
            column(width = 6,
              actionButton("settings_previous", "Previous", width = "100%")
            ),
            column(width = 6,
              actionButton("settings_next", "Next", width = "100%")
            )
          )
        ),
        
        # Preliminary results page =============================================
        tabPanelBody("prelim_results",
          titlePanel("Preliminary Results"),
          boot_results_ui("prelim"),
          # Navigation ---------------------------------------------------------
          fluidRow(
            column(width = 6,
              actionButton("boot_results_previous", "Previous", width = "100%")
            ),
            column(width = 6,
              actionButton("boot_results_next", "Next", width = "100%")
            )
          )
        ),
        
        # Outlier detection ====================================================
        tabPanelBody("outlier_detection",
          # Outlier detection controls -----------------------------------------
          titlePanel("Outliers"),
          plotlyOutput("jab_plot"),
          dataTableOutput("outliers"),
          # Navigation ---------------------------------------------------------
          fluidRow(
            column(width = 6,
              actionButton("outliers_previous", "Previous", width = "100%")
            ),
            column(width = 6,
              actionButton("outliers_next", "Next", width = "100%")
            )
          )
        ),
        
        # Outlier-removed results page =========================================
        tabPanelBody("outlier_results",
          titlePanel("Results with Selected Outliers Removed"),
          boot_results_ui("outliers")
        ),
        
        # Next page ============================================================
        tabPanelBody("next_page",
          
        )
      )
    )
  )
))
