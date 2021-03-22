library(shiny)

shinyUI(fluidPage(
  navbarPage(title="Turner: A Bootstrap App",
    tabPanel("Bootstrap",
      sidebarLayout(
        # Working panel ========================================================
        mainPanel(
          tabsetPanel(
            # Data preview -----------------------------------------------------
            tabPanel("Data Preview", DT::dataTableOutput("data_preview")),
            
            # Results visualization --------------------------------------------
            tabPanel("Bootstrap Results",
              fluidRow(
                # Histogram with confidence bounds -----------------------------
                column(width=9,
                  HTML("<br>"),
                  plotlyOutput("boots_histogram"),
                  sliderInput(
                    "ci_alpha",
                    "Confidence level:",
                    min = 0.01,
                    max = 0.15,
                    value = 0.05,
                    ticks = FALSE,
                    width = "100%"
                  )
                ),
                # Numeric results ----------------------------------------------
                column(width = 3, htmlOutput("boots_results")),
              )
            )
          )
        ),
        
        # Settings panel =======================================================
        sidebarPanel(
          h2("Setup"),
          # Data upload --------------------------------------------------------
          h4("Data File"),
          fileInput("user_file", "Data upload:", accept=".csv"),
          
          # Bootstrap settings -------------------------------------------------
          h4("Bootstrap Settings"),
          numericInput(
            "param_B",
            "Number of bootstrap samples:",
            min=1,
            value=1000,
            step=1,
          ),
          selectInput("param_statistic", "Statistic:", c("Mean", "Median")),
          uiOutput("select_variable"),
          
          # Go button ----------------------------------------------------------
          actionButton("go", "Bootstrap!", icon=icon("refresh"), width="100%")
        )
      )
    )
  )
))
