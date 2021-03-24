library(shiny)

shinyUI(fluidPage(
  navbarPage(title="Turner (WIP)",
    tabPanel("Bootstrap",
      tabsetPanel(type = "hidden", id = "wizard", selected = "welcome",
        # Welcome page =========================================================
        tabPanelBody("welcome",
          sidebarLayout(
            # Contact info sidebar ---------------------------------------------
            sidebarPanel(
              titlePanel("Contact"),
              fluidRow(width=12, align="center",
                HTML("<script type='text/javascript' src='https://platform.linkedin.com/badges/js/profile.js' async defer></script><div class='LI-profile-badge'  data-version='v1' data-size='medium' data-locale='en_US' data-type='vertical' data-theme='light' data-vanity='alex-lehmann-ds'><a class='LI-simple-link' href='https://ca.linkedin.com/in/alex-lehmann-ds?trk=profile-badge'>Alex Lehmann</a></div>"),
                HTML("<br><b>Email: </b><a href='mailto:alex.lehmann@cmail.carleton.ca'>alex.lehmann@cmail.carleton.ca</a>")
              )
            ),
            
            # Intro panel ------------------------------------------------------
            mainPanel(
              titlePanel("Welcome to Turner!"),
              HTML("<p>Turner is an app built to make boostrapping procedures and
               their related diagnostics accessible and easy to use. Turner uses
               the <a href='https://www.tidymodels.org/'>tidymodels</a>
               framework to resample your provided data and employs a number of
               diagnostic methods to ensure your results are robust."),
              HTML("<p><b>Important note: Turner is a work-in-progress. Some
               features may be missing or may not work properly. The user
               interface may be ugly. Use at your own risk.</b>"),
              hr(),
              actionButton("welcome_next", "Start", width="100%")
            )
          )
        ),
        
        # Data upload page =====================================================
        tabPanelBody("data_upload",
          titlePanel("Data"),
          fileInput("user_file", "Upload your data as a CSV", accept = ".csv"),
          DT::dataTableOutput("data_preview"),
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
          selectInput("param_statistic", "Statistic:", c("Mean" = mean)),
          uiOutput("select_variable"),
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
        
        # Bootstrap results page ===============================================
        tabPanelBody("boot_results",
          titlePanel("Preliminary Results"),
          # Results ------------------------------------------------------------
          uiOutput("results_summary"),
          plotlyOutput("results_hist"),
          sliderInput("ci_alpha",
            "Confidence level:",
            min = 0.01, max = 0.15,
            value = 0.05,
            ticks = FALSE,
            width = "100%"
          ),
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
        
        tabPanelBody("outlier_detection",
          titlePanel("Outliers"),
          plotOutput("jab_plot")
        )
      )
    )
  )
))
