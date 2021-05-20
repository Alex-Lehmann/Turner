# UI ###########################################################################
results_ui <- function(id) {
  ns = NS(id)
  
  tagList(
    uiOutput(ns("title"))
  )
}

# Server #######################################################################
results_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    # Title ====================================================================
    output$title <- renderUI({
      # Generate title
      if (values$spec$stat %in% "Correlation") {
        template <- paste0(
                      values$spec$stat,
                      " Between ",
                      values$spec$var1, " and ", values$spec$var2
                    )
      } else template <- paste0(values$spec$stat," of ", values$spec$var)
      title_text <- paste0("Bootstrap Results for ", template)
      
      # Create UI element
      return(h2(title_text))
    })
  })
}
