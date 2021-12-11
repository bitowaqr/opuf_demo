# deadDCE

drawScenarioTbl = function(
  ds, hs = c(1,2,3,4,5),
  purples = c("#F2F0F7","#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA")
){
  
  # create dead state card
  if (hs[1] == "dead") {
    dim_labs = rep("Being dead", 5)
    lvl_labs = rep("Being dead", 5)
    cell_cols = "darkgray"
    hs_df = data.frame(
      dim_labs,
      lvl_labs
    )
  } else {
    # create normal tto state card
    dim_labs = unlist(lapply(1:5, function(i) {
      ds[[i]]$name2
    }))
    lvl_labs = unlist(lapply(1:5, function(i) {
      ds[[i]]$levels[hs[i]]
    }))
    cell_cols = purples[hs]
    
    hs_df = data.frame(
      dim_labs,
      lvl_labs
    )
  }
  
  dtable <- datatable(
    hs_df,
    escape = FALSE, colnames = c("",""),
    selection = 'none',
    rownames=FALSE,
    class = c('cell-border'),
    
    options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      rowsGroup = list(c(1),c(0)),
      initComplete = JS("function(settings, json) {",
                        "$(this.api().table().container()).css({'font-size': '120%'});",
                        "$(this).css('border-radius','10px');",
                        "}"),
      rowCallback = DT::JS(
        'function(row,data) {
        $("td:eq(1)",row).css("border","1px solid white");
        $("td:eq(0)",row).css("border","1px solid white");
    }'       ),
      columnDefs = list(list(visible=F, targets=0),
                        list(width = '10px', targets = c(0)),
                        list(width = '2000px', targets = c(1)),
                        list(className = 'dt-center', targets = 0:1)
      )
    )
  ) 
  
  # dead dce style
  if(hs[1] == "dead"){
    path <- "./www/" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency("RowsGroup", "3.0.0",path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep,dep))
    
    dtable = dtable %>%
      formatStyle(2, height = "247px") %>%
      formatStyle(2,'border'="1px solid darkgray") %>%
      formatStyle(2,'border-radius'="5px") %>%
      formatStyle(2,'font-weight'="600") %>%
      formatStyle(2,'color'="white") %>%
      formatStyle(2,  backgroundColor = "#333333")
    
    # formatStyle(2,'color'="black") %>%
    # formatStyle(2,  backgroundColor = "lightgray")
  } else {
    dtable = dtable %>%
      formatStyle(1:2,height="35px") %>%
      formatStyle(1,'border-top-left-radius'="5px") %>%
      formatStyle(1,'border-bottom-left-radius'="5px") %>%
      formatStyle(2,'border-top-right-radius'="5px") %>%
      formatStyle(2,'border-bottom-right-radius'="5px") %>%
      formatStyle(1,'font-weight'="600") %>%
      formatStyle(1:2,valueColumns = 2,  backgroundColor = styleEqual(lvl_labs,cell_cols))
  }
  
  
  
  
  return(dtable)
}



deadDCE_ui = function( ...){

  list(

      # headline
      fluidRow(
          column(
              offset = 1, 5,
              vsm(1),
              h2("Life & Death"),
              p("This task is similar to the previous one, but now 
              you are asked to compare a state with health problems with being dead.
              Again, try to imagine what it would be like to be living with the health problems.
                "), 
              HTML("<p><b>Which scenario do you think is better?</b></p>")
          )
      ),
      vsm(1),

      # scenario A
      column(
          offset = 1, width = 5,
          br(),
          shinydashboard::box(
              id = "deadDCE_box_a", title = "Scenario A", status = "primary", solidHeader = T,
              width = 12,
              withSpinner(
                id = "tto_spinner",
                dataTableOutput("deadDCE_A_tbl"),
                proxy.height = "266px"
              )
          )
      ),

      # scenario B
      column(
          offset = 0, width = 5, align = "left",
          br(),
        shinydashboard::box(
            id = "deadDCE_box_b", title = "Scenario B", status = "primary", solidHeader = T,
            width = 12,
            dataTableOutput("deadDCE_B_tbl")
        )
      ),
      
      column(
          offset = 3, width = 6,
          align = "center",
          #div(class="card",
          h4("Which is better?"),
          div(id= "no_wrap_col",
            actionBttn("dead_A","Scenario A",style = "jelly",  size ="lg"),
            # hsm(2),
            # actionBttn("dead_equal","Both are the same",style = "jelly",size ="lg"),
            hsm(3),
            actionBttn("dead_B","Scenario B",style = "jelly",size ="lg")
          )
      )
      
  )

}


deadDCESelector = function(trail,hs_ranks,ds_expanded){
    
    len = nrow(ds_expanded)
    
    if (is.null(trail)) {
        selected_hs = as.numeric(ds_expanded[len, ])
        return(selected_hs)
    }
    if(substr(trail,1,1)=="A"){
        # 11111 is used as a dummy for: 55555 is better than dead
       selected_hs = as.numeric(ds_expanded[1, ])
       return(selected_hs)
    }
    
    space_max = len*2
    space_min = 1
    is_at = len
    choices = unlist(strsplit(trail, ""))
    for (i in choices) {
        if (i == "B") {
            space_max = is_at - 1
            is_at = floor((space_max + space_min) / 2)
        }
        if (i == "A") {
            space_min = is_at
            is_at = floor((space_max + space_min) / 2)
        }
    }
    
    hs_index = which(hs_ranks == is_at)
    selected_hs = as.numeric(ds_expanded[hs_index, ])
    return(selected_hs)
}

deadDCEOver = function(trail){
    if (is.null(trail)) {
        return(FALSE)
    }
    if (grepl("E", trail)) {
        return(TRUE)
    }
    if (substr(trail,1,1) =="A") {
        return(TRUE)
    }
    if (nchar(trail) >= 6) {
        return(TRUE)
    }
    return(FALSE)
}



deadDCEConfirm = function(session){
  ask_confirmation(
    session = session,
    inputId = "deadDCE_confirm",
    type = "success",
    btn_labels = c("Restart this task","Continue"),
    closeOnClickOutside = F,
    btn_colors = c("darkgray","green"),
    title = "",html = T,
    text = HTML("<b>Your response has been recorded.</b> <br><br>Please proceed to the next task."),
    showCloseButton = F
  )
}

