# lvlRate



# lvlRating ui
lvlRate_ui = function(...){
  
  # initil lvl Rating noUi slider values
  handle_values <- c(0,1,2,3,100)
  
  list(
    
    fluidRow(
      column(
        offset = 1,
        width = 5,
        vsm(1),
        h2("Severity levels"),
        
        HTML("
        <p>In this task we again use a scale from 100 to 0. </p>
        
        <p>Here we ask you to indicate where on this scale 
        <span class='hl_text_inline'>YOU</span> think 
             three different levels of health problems are.</p>
        "),
        
        # "It's about how you understand the terms 'slight', 'moderate' and 'severe' with regard to health problems.
        vsm(1),
        p(HTML(
          paste0(
            tags$li(
              HTML("A person with <b>100% health</b> has <b>no health problems.</b>")
            ),
            tags$li(
              HTML(
                "A person with <b>0% health</b> has <b>extreme health problems.</b>"
              )
            ),
            tags$li(
              HTML(
                "At what level of health does a person have <b>slight</b>, <b>moderate</b>, and <b>severe problems</b>?"
              )
            )
          )
        )),
        
        vsm(1),
        # note box
        div(class = "card", style = "text-align:left; padding-bottom: 40px; ",
            p(
              uiOutput("lvlRate_preamble"),
              div(id = "lvlRate_spinner_container",
                  withSpinner(
                    id = "lvlRate_spinner",
                    uiOutput("lvlRate_prompt"),
                    proxy.height = "105px"
                  )
              )
            )),
        
      ),
      
      # main left
      
      
      
      # MAIN RIGHT
      column(
        offset = 0,
        width = 5,
        
        vsm(4),
        
        # noUi Lvls rating with heavy js support
        
        div(
          class = "card",
          div(class = "hl_text_inline", "Health Scale"),
          div(style = "margin-left: 170px",
              
              noUiSliderInput(
                inputId = "lvlRate",
                label = "",
                tooltips = T,
                value = handle_values,
                min = 0,
                behaviour = "snap",
                max = 100,
                step = NULL,
                direction = "rtl",
                padding = 0,
                update_on = "change",
                connect = c(F,F,F,F,T),
                pips = list(
                  mode = "positions",
                  values = seq(0, 100, 10),
                  density = 2,
                  format = wNumbFormat(decimals = 0, suffix = "%")
                ),
                format = wNumbFormat(decimals = 0, suffix = ""),
                orientation = "vertical",
                width = "75%",
                height = "343px",
                margin = 1,
                inline = T
              )
          ),
          
          # include js script on page
          uiOutput("lvlRateJs")
        )
      )
    ))
}



# server function to take lvl rating user progress and return next prompt
lvlRatePreamble = function(index){
  
  if(is.null(index)){
    return("Click on the scale on the right to use it. Make sure you place all three levels (slight, moderate, severe) on the scale before you proceed.")
  }
  
  if(index == 3){
    return(
      div(span(class = "hl_text_inline", "Note:"),HTML("You can modify and change your responses by moving/dragging the arrows up or down."))
    )
  } else {
    return("Click on the scale on the right to use it. Answer intuitively.")
  }
}

lvlRatePrompter = function(index, purples){
  
  lvlAutoComplete = function(lvl, bg){
    
    
    div(
      class ="lvl_prompt", 
      HTML(paste0(
        "How much health does a person with 
          <span class = 'lvl_noUi_lab' style='background-color:",
        bg,"'><b>", lvl, 
        "</b> health problems</span> have left?"
      ))
    )
    
  }
  
  if(is.null(index)){
    return(lvlAutoComplete("Slight", purples[2]))
  }
  if(index==1){
    return(lvlAutoComplete("Moderate", purples[3]))
  }
  if(index==2){
    return(lvlAutoComplete("Severe", purples[4]))
  }
  if(index==3){
    return(NULL)
  }
  return("Error - no value defined.")
}


# check if user finished task
lvlRate_completed = function(..., progress = NULL){
  # uses input$next_handle defined throgh js script
  if(!is.null(progress)){
    if(progress==3){
      return(T)
    }
  }
  return(F)
}


