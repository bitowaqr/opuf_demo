# anyDCE

#------------------------------------------------------
# NOTE: Module requires functions from 'deadDCE' module
#------------------------------------------------------

# select states for dce tasks
getRefDCEState <- function(
  scaled_values, 
  ds_expanded, 
  quants = c(0.75, 0.5, 0.25), 
  diff  = 0.20
  ){
  
  scaled_values_ordered = scaled_values[order(scaled_values)]
  quants_ranks = ceiling(quants * length(scaled_values_ordered))
  relevant_vals <- scaled_values_ordered[quants_ranks]
  
  # state A
  hs_indices <- match(relevant_vals, scaled_values)
  res_A <- t(ds_expanded[hs_indices,])
  val_A <- scaled_values[hs_indices]
  
  # state B
  res_B <- lapply(seq_along(val_A), function(x){
    
    dominations <- apply(ds_expanded,1,function(y){
      abs(sum(sign(y - res_A[,x]))) == sum(sign(abs(y - res_A[,x])))
    })
    
    diff_i <- ifelse(length(diff) == 1, diff, diff[x]) 
    hs_i_index <- which.min(abs(abs(val_A[x] - scaled_values[!dominations]) - diff_i))
    
    
    res <- ds_expanded[!dominations,][hs_i_index,]
    
    if(length(res)==0){
      res <- ds_expanded[sample(1:nrow(ds_expanded),size = 1),]
    }
    return(res)
  })
  
  res_B <- matrix(unlist(res_B), ncol = length(res_B))
  
  rownames(res_A) <- paste0("A_", c("mo","sc","ua","pd","ad"))
  rownames(res_B) <- paste0("B_", c("mo","sc","ua","pd","ad"))
  
  res_list <- list(
    "A" = res_A,
    "B" = res_B
  )
  
  return(res_list)
}


anyDCE_ui = function(...){
  
  list(
    
    # headline
    fluidRow(
      column(
        offset = 1, 5,
        vsm(1),
        h2("Choosing between different health states"),
        p("Consider the two scenarios below. Imagine what it would be like to be living in either of the scenarios."),
        HTML("<p><b>Which scenario do you think is better?</b></p>")
      ),
    ),
    
    # scenario A
    column(
      offset = 1, width = 5,
      br(),
      shinydashboard::box(
        id = "DCE_box_a", title = "Scenario A", status = "primary", 
        solidHeader = T,
        width = 12,
        withSpinner(
         id = "dce_spinner1",
          dataTableOutput("DCE_A_tbl"),
           proxy.height = "266px"
        )
      )
    ),
    
    # scenario B
    column(
      offset = 0, width = 5, align = "left",
      br(),
      shinydashboard::box(
        id = "DCE_box_b", title = "Scenario B", status = "primary", solidHeader = T,
        width = 12,
        withSpinner(
        id = "dce_spinner2",
          dataTableOutput("DCE_B_tbl"),
          proxy.height = "266px"
        )
      )
    ),
    
    # buttons to choose scenario
    column(
      offset = 3, width = 6,
      align = "center",
      h4("Which is better?"),
      div(id= "no_wrap_col",
          actionBttn("dce_A","Scenario A",style = "jelly",  size ="lg"),
          hsm(3),
          actionBttn("dce_B","Scenario B",style = "jelly",size ="lg")
      )
    )
  )
  
}



# popup to indicate that DCE task is over
anyDCEConfirm = function(session){
  ask_confirmation(
    session = session,
    inputId = "anyDCEConfirm",
    type = "success",
    btn_labels = c("Continue"),
    closeOnClickOutside = F,
    btn_colors = c("green"),
    title = "",html = T,
    text = HTML("<b>Your response has been recorded.</b> <br><br>Please proceed to the next task."),
    showCloseButton = F
  )
}