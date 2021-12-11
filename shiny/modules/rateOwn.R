
# rateOwn: classification and VAS

#-----------------------------------------
# Classification ---------------------------

rateOwn_ui = function(ds,...) {

list(
    # headline
    fluidRow(
        column(
            offset = 1, width = 10,
            vsm(1),
            h2("Health Questionnaire (PLACEHOLDER)")
        )
    ),
    # radio group buttons
    fluidRow(
        # instructions on the left
        column(
            offset = 1,
            width = 5,
            div(
                class = "card",
                HTML("
                Under each heading, please tick the ONE box that 
                best describes
                your health <span class='hl_text_inline'> TODAY</font></span>.")
            )
        ),
        column(
            offset = 0,
            width = 5,
            lapply(seq_along(ds), function(i) {
                div(class="card",style="padding:10px;",HTML(paste0(
                    
                    prettyRadioButtons(
                        inputId = paste0("rateOwn_", names(ds)[i]),
                        label = ds[[i]]$name,bigger = T,outline = T,
                        choices = ds[[i]]$levels,
                        status = "danger",
                        width = "100%",
                        selected = character(0)
                    )
                )))
            })
        )
    )
)


}

rateOwn_help = function(...){""}

rateOwn_completed = function(input,ds,...){
    vals = unlist(lapply(paste0("rateOwn_", names(ds)), function(x) {
        input[[x]]
    }))
    if(is.null(vals)){
        return(F)
    }
    if(length(vals)==5){
        return(T)
    } else {
        return(F)
    }

}

#-----------------------------------------
# VAS ---------------------------

vasOwn_ui = function(...){

    list(
        fluidRow(
            column(
                offset = 1, width = 10,
                vsm(1),
                h2("Your health today")
            )
        ),
        fluidRow(
            column(
                offset = 1,
                width = 5,
                div(
                    class = "card",
                    style = "min-height: 340px;",
                    div(id = "vas_list",style = "margin-right:10px;margin-top:10px;",
                        HTML("
                    <ul>
                    <li>We would like to know how good or bad your health is <span class='hl_text_inline'> TODAY</font></span>.</li>
                    <li>This scale is numbered from 0 to 100.</li>
                    <li>100 means the <u>best</u> health you can imagine.<br>
                    0 means the <u>worst</u> health you can imagine.</li>
                    <li>Please indicate on the scale how your health is <span class='hl_text_inline'> TODAY</font></span>.</li>
                    </ul>
                    ")
                    ),
                    vsm(1),
                    
                        div(
                            style = "text-align:left; margin-left: 50px;",
                            
                            conditionalPanel(
                                condition = "output.rateOwnVas_moved",
                                id = "rateOwnVas_conditional_panel",
                                HTML("YOUR HEALTH TODAY = "),
                            div(
                                class = "shy_value_box",
                                textOutput("rateOwnVas_out", inline = T)
                            )
                        )
                    )
                )
            ),
            column(
                offset = 0,
                width = 5,
                div(
                    class = "card",
                    div(style = "
                            color: var(--primary);
                            padding:0;
                            font-size: 120%;
                            font-weight: 600;
                            margin-bottom: 20px;",
                        HTML("Click on the scale to use it.")
                    ),
                    column(12,
                           align = "center",
                           HTML("The best health<br>you can imagine<br>"),
                           noUiSliderInput(
                               inputId = "rateOwnVas",
                               label = "",
                               tooltips = T,
                               min = 0, max = 100,
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
                           ),
                           div(style = "margin-top:-20px", HTML("The worst health<br>you can imagine"))
                    ),
                    hsm(1)
                )
            )
        )
    )
}

vasOwn_help = function(){""}

rateOwnVas_completed = function(...,rateOwnVas_moved){
    rateOwnVas_moved
}

