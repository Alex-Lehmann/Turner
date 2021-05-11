welcome_ui = function(id) {
  ns = NS(id)
  
  tagList(
    # Contact info ===============================================================
    sidebarLayout(
      sidebarPanel(
        titlePanel("Contact"),
        fluidRow(width = 12, align = "center",
          HTML("<script type='text/javascript' src='https://platform.linkedin.com/badges/js/profile.js' async defer></script><div class='LI-profile-badge'  data-version='v1' data-size='medium' data-locale='en_US' data-type='vertical' data-theme='light' data-vanity='alex-lehmann-ds'><a class='LI-simple-link' href='https://ca.linkedin.com/in/alex-lehmann-ds?trk=profile-badge'>Alex Lehmann</a></div>"),
          HTML("<br><b>Email: </b><a href='mailto:alex.lehmann@cmail.carleton.ca'>alex.lehmann@cmail.carleton.ca</a>")
        )
      ),
    
    # Intro panel ================================================================
      mainPanel(
        titlePanel("Welcome to Turner!"),
        # Blurb ------------------------------------------------------------------
        HTML("<p><b>Important note: Turner is a work-in-progress. Some features
              may be missing or may not work properly. The user interface may be
              ugly. Use at your own risk.</b>"),
        HTML("<p>Turner is an app built to make boostrapping procedures and
              their related diagnostics accessible and easy to use. Turner uses
              the <a href='https://www.tidymodels.org/'>tidymodels</a>
              framework to resample your provided data and employs a number of
              diagnostic methods to ensure your results are robust."),
        HTML("<a href='https://github.com/Alex-Lehmann/Turner'>View the full source code on GitHub.</a>")
      )
    )
  )
}
