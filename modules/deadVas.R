
#-----------------------------------------
# deadVas ---------------------------

deadVas_ui = function(...){
  
  list(
    
    # headline
    fluidRow(
      column(
        offset = 1, 5,
        vsm(1),
        h2("Life & Death continued"),
        HTML("<p>You indicated that, <span class = 'hl_text_inline'>for you</span>, 
             having extreme health problems <b>(Scenario A)</b> is better than being dead. 
             Now, please consider the scale on the right.</p>"),
        
        
        HTML("<ul>
        <li>The scale is numbered from 100 to 0.</li>
        <li>100 means <u>No health problems</u>.</li>
        <li>0 means <span class = 'dead_vas_badge'>being dead</span>.</li>
             </ul>"),
        HTML("<p><b>Where on this scale does Scenario A lie <u>for you</u>?</b></p>"),
        
    
    vsm(1),
    
    # scenario A: the pit state
      shinydashboard::box(
        id = "pit_state_box", title = "Scenario A", status = "primary", solidHeader = T,
        width = 12,
          div(id = "pit_state_tbl_div", dataTableOutput("pit_state_tbl"))
      )
    ),
    
  # deadVas
  # scenario B: dead scenario
      column(
        offset = 0,
        width = 4,
        
        vsm(4),
        
        div(
          class = "card",
          column(
            width = 12,
            align = "center",
            div(HTML("No health problems"), style = "margin-bottom: -10px; margin-top: 5px;"),
            div(
              id = "dead_vas_slider_div",
              noUiSliderInput(
                inputId = "dead_vas_slider",
                label = "",
                tooltips = T,
                min = 0,
                max = 100,
                # color = "#EC008C",
                step = NULL,
                direction = "rtl",
                pips = list(
                  mode = "positions",
                  values = seq(0, 100, by = 5),
                  density = 1
                ),
                format = wNumbFormat(decimals = 0),
                value = 0,
                orientation = "vertical",
                width = 300,
                height = 400
              )
            ),
            
            div(
              style = "text-align:left; margin-left: 50px; margin-top: 30px; min-height: 40px; margin-bottom: -20px;",
              
              conditionalPanel(
                condition = "input.deadVas_moved",
                id = "deadVas_conditional_panel",
                HTML("YOUR SELECTION = "),
                div(class = "shy_value_box",
                    textOutput("deadVas_out", inline = T))
              )
            )
            
          ),
          hsm(1)
        )
      )
    )
  )
}

deadVas_completed = function(...,deadVas_moved = F){
  deadVas_moved
}

# some js code to draw the line between the health state and the VAS
deadVas_js <- function(){
  runjs("
  // draw dynamic line between state and VAS scale
  // at load, the handle+line is invisible to avoid anchoring
  // after first click, both appear
  
  var start_line = document.getElementById('pit_state_box');
  var end_line = document.querySelector('#dead_vas_slider_div .noUi-handle');
  var dead_vas_slider = document.querySelector('#dead_vas_slider');
  
  line = new LeaderLine(start_line, end_line, {
    color: 'transparent',
    endPlugSize: 1,
    startSocket: 'right', 
    endSocket: 'left'
  });
  
  // update line when the handle is dragged or clicked
  dead_vas_slider.noUiSlider.on('update', function(){
    line.position();
  });
  dead_vas_slider.noUiSlider.on('set', function(){
    setTimeout(function() {
      line.position();
      }, 300) // when click-jump, wait until handle is in final position
  });
  
  // make line and handle visible after first click
  dead_vas_slider.addEventListener('click', function(){
    $('.noUi-handle').css('visibility','visible');
    line.setOptions({color: 'black'});
    
    // allow user to move to next page
    Shiny.setInputValue('deadVas_moved', true);
    
    // add event listener to remove line when user moves on
    var nav_btn_nxt = document.querySelector('#nav_btn_nxt');
    nav_btn_nxt.addEventListener('click', function(){
      line.remove()
    })
    
  }, {once:true})

  // Overwrite pip values on nouislider
  var dead_vas_slider_vals = document.querySelector('#dead_vas_slider').querySelectorAll ('.noUi-value-vertical');
  dead_vas_slider_vals[0].textContent = 'being dead';
        ")  
}
