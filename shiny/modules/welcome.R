# WELCOME UI

# Landing page:
# first page that users will see with some info and instructions

welcome_ui = function(...) {
  
  list(
    fluidRow(column(
      offset = 1,
      width = 10,
      vsm(3),
      div(class = "header_title_1", "Welcome"),
      vsm(1),
      h2("OPUF Health Preference Survey"),
      
      fluidRow(
        column(
          width = 6,
          HTML(
            "<p><b>This is a demo version of the Online PUF survey instrument.
                      Please feel free to start the survey, explore the tool, and try the different tasks.
                           </b></p>"
          ),
          
          p(
            "The following web pages will guide you through a series of
                        questions and exercises. In some questions, we will ask you about your
                        own health, and to compare different health problems.
                        In other questions, we will ask you to make choices between different
                        scenarios involving poor health, disability, and death. There are
                        no right or wrong answers - it's just about your own perspective."
          ),
          
          h4("The survey will take about 10 minutes to complete.")
          
        ),
        
        column(
          width = 6,
          
          
          p(
            "At the end of the survey, all your responses will be combined.
                        You can then compare your personal results to the responses of the general population."
          ),
          
          p(
            "This is only a demo version. Your responses will not be used for
                      the research project. All your data will be deleted at the end of this
                      session, i.e. when you close this window."
          ),
          
          HTML(
            "<p><span class ='hl_text_inline'> Thank you very much</span> for
                           your interest in this research project!</p>"
          ),
          
          column(
            12,
            align = "center",
            vsm(1),
            actionBttn("give_consent", "Start the survey", style = "jelly")
          )
        )
      )
    )),
    vsm(2),
    fixedPanel(
      id = "welcome_footer",
      left = 0,
      right = 0,
      bottom = 0,
      column(
        offset = 1,
        5,
        div(id = "h3w", "Want to receive updates?"),
        HTML(
          "If you would like to receive updates about this
                    research project, want to collaborate, or if you have
                    any other questions, please contact:"
        ),
        vsm(2)
      ),
      column(
        5,
        HTML("<b>Paul Schneider</b><br>"),
        HTML("School of Health and Related Research<br>"),
        HTML("University of Sheffield<br>"),
        HTML(
          '<a href = "mailto: p.schneider@sheffield.ac.uk"><u>p.schneider@sheffield.ac.uk</font></a>'
        )
      )
    )
    
  )
}