
#----------------------------------------------------
#
# ONLINE ELICTIATION OF PERSONAL UTILITY FUNCTIONS (OPUF) DEMO APP
# 11 December 2021
# Version 1.0 
#
# Paul Schneider
# The University of Sheffield
# p.schneider@sheffielf.ac.uk
#
#----------------------------------------------------
#
# NB: This shiny app was originally built for the EQ-5D-5L instrument.
# Due to licensing issues, we are not allowed to publish the orignal version.
# This code therefore contains placeholders 'dimension A', 'dimension B' etc
# EQ-5D labels can be requested free of charge at https://registration.euroqol.org/
#----------------------------------------------------


# LOAD REQUIRED PACKAGES
#remotes::install_github("hrbrmstr/ggchicklet")
#remotes::install_github("rstudio/shinyvalidate")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) 
library(shinyvalidate)
library(shinyjs)
library(sortable)
library(ggplot2)
library(ggchicklet)
library(ggthemes)
library(DT)
library(fmsb)
library(showtext)
library(RColorBrewer)
library(waiter)
library(eq5d)
library(statebins)


# SOURCE R SCRIPTS (MODULES AND R FUNCTIONS)
paths = c("./R", "./modules")
for(p in paths){
  files.sources = paste0(p, "/", list.files(path = p))
  sapply(files.sources, source)
}


# Generate PLACEHOLDER descriptive system object
ds = placeholderGen()
ds_expanded = dsExpand(ds)
start_at_page = 1      # set starting page (for debugging)

# DEFINE PATH THROUGH THE TOOL
  # NB: the code for each module is provided in
  # separate files in the ./modules folder
agenda = list(
  "Welcome" = welcome_ui,                     # 1
  "Self-reported health status" = rateOwn_ui, # 2
  "Subjective health status" = vasOwn_ui,     # 3
  "Level Rating" = lvlRate_ui,                # 4
  "Dimension Ranking" = dimRank_ui,           # 5
  "Dimension Weighting" = swingRate_ui,       # 6
  
  "Health States Choices" = anyDCE_ui,        # 7
  "Life & Death" = deadDCE_ui,                # 8
  "Life & Death continued" = deadVas_ui,      # 9
  
  "Survey" = survey_ui,                       # 10
  "Results page" = resultsViewer_ui           # 11
)

# DEFINE CRITERIA FOR ALLOWING USER TO PROCEED TO THE NEXT PAGE
responses_check = list(
  "Welcome" = function(...){T},                         # 1
  "Self-reported health status" = rateOwn_completed,    # 2
  "Subjective health status" = rateOwnVas_completed,    # 3
  "Level Rating" = lvlRate_completed,                    # 4
  "Dimension Ranking" = dimRank_completed,              # 5
  "Dimension Weighting" = function(...){T},             # 6
  
  "Health States Choices" = function(...){F},           # 7
  "Life & Death" = function(...){F},                    # 8 
  "Life & Death continued" = deadVas_completed,         # 9
  
  "Survey" =   function(...){T},#survey_complete,       # 10
  "Results" = function(...){F}                          # 11
)

# some additional styling
font_add_google("Inter", "Inter")
showtext_auto()
purples = RColorBrewer::brewer.pal(9, "Purples")[c(2, 4, 5, 6, 7)]


# USER INTERFACE ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- fluidPage(

  # include CSS
  includeCSS("custom.css"),

  # show loading screen
  use_waiter(), 
  waiter_show_on_load(loading_screen()), 
  
  # activate js and dashboard style
  useShinyjs(),
  useShinydashboard(),
  
  # prevent use on mobile phones and small screens
  tags$head(avoid_mobile_view()),

  # header ui
  header(),

  # main panel: one page app, shows element conditional on progress
    # NB: the conditional panel function iterates through the 'agenda' list, 
    # defined above which contains the ui code for each page/module
    mainPanel(
      id = "main_panel",
      width = 12,
      lapply(seq_along(agenda), function(i) {
        conditionalPanel(paste0("output.nav_pos_is==", i), agenda[[i]](
          ds = ds, 
          purples = purples 
          ))

      }),
      # hide page and show msg if mobile user
      tags$script(src = "checkMobile.js") # show spinner without calling sleep()
    ),

    # footer ui (not shown on first page)
    conditionalPanel(
      condition = "output.nav_pos_is>1",
      footer_panel(agenda)
    ),
  
  # load additional js libraries
  tags$script(src = "spinner2.js"), # show spinner without calling sleep()
  tags$script(src = "leader-line.min.js"), # draw line for deadVas
  tags$script(src = "plain-draggable.min.js"), # re-draw line for deadVas
  # save whole page as pdf (take snapshot and then save)
  tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"), 
  tags$script(src = "saveAs.js")

) # ui end






# SERVER ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input,output,session){

# puf results: stores all important reactive data
  puf = reactiveValues()

# will hide *on_load waiter
  waiter_hide() 


# NAVIGATION CONTOLS -----------------------------------
  # create disabled fake buttons to show tooltips
  disable("nav_btn_nxt_disabled")
  disable("nav_btn_prv_disabled")

  # reactive values to monitor navigation button and update current page
  consent_given = reactiveVal(0)
  nav_pos_is = reactiveVal(start_at_page)
  data_submitted = reactiveVal(0)

  # 1st page consent btn gets user to 2nd page
  observeEvent(input$give_consent, {
    consent_given(1) # register that consent was given
    nav_pos_is(2) # move to 2nd page
  })


  # NXT BUTTON LOGIC (Part 1)
  observeEvent(input$nav_btn_nxt, {
    # if nxt btn is clicked on second to last page,
    # ask to confirm data submission
    # on any other page, nxt btn moves user to next page
    if (nav_pos_is() == length(agenda) - 1) {
      modalSubmitData(input, data_submitted)
    } else {
      nav_pos_is(nav_pos_is() + 1)
    }
  })

  # prv btn moves user one page back
  observeEvent(input$nav_btn_prv,{
    nav_pos_is(nav_pos_is() - 1)
  })

  # help button (specific to each task)
  observeEvent(input$help_btn,{
    modalhelp(nav_pos_is,agenda)
  })

# RATE OWN VAS -------------------------------
# your health today feedback text box
  # check if the slider was move before allowing move to next task
  rateOwnVas_moved = reactiveVal(F)
  observeEvent(input$rateOwnVas,once = T, ignoreInit = T, ignoreNULL = T,{
    rateOwnVas_moved(T)
  })
  
  # only show 'your health today' when slider was moved
  output$rateOwnVas_moved <- reactive({
    rateOwnVas_moved()
  })
  outputOptions(output, "rateOwnVas_moved", suspendWhenHidden = FALSE)
  
output$rateOwnVas_out <- renderText(
  if(rateOwnVas_moved()){
    return(input$rateOwnVas)
  } else {
    return("__")
  }
)
# hide slider before the slider was moved the first time
runjs("$( '#rateOwnVas .noUi-base').click(
  function(){
      $('#rateOwnVas .noUi-handle').css('visibility','visible')})")


# DIMRANK -------------------------------------
# disable bucket when it is empty
observeEvent(input$dimRank_ranking,{
  if(length(input$dimRank_ranking)==5){
    runjs('document.getElementById("rank_list_id_1").classList.remove("rank-list");')
    runjs('document.getElementById("rank_list_id_1").classList.remove("rank-list-empty");')
  }
})


# SWINGRATE -----------------------------------
# swingRate 1st rank update
# take 1st rank from dimRank task and set it to 100 (fixed)
observeEvent(input$dimRank_ranking, {
  swingrate_slider_update(input$dimRank_ranking, ds, session)
  if(length(input$dimRank_ranking)==5){
    runjs(paste0("$('#swingRate_card_", input$dimRank_ranking[4], "').insertBefore('#swingRate_card_", input$dimRank_ranking[5], "');"))
    runjs(paste0("$('#swingRate_card_", input$dimRank_ranking[3], "').insertBefore('#swingRate_card_", input$dimRank_ranking[4], "');"))
    runjs(paste0("$('#swingRate_card_",input$dimRank_ranking[2],"').insertBefore('#swingRate_card_",input$dimRank_ranking[3],"');"))
    runjs(paste0("$('#swingRate_card_",input$dimRank_ranking[1],"').insertBefore('#swingRate_card_",input$dimRank_ranking[2],"');"))
  }

})
# rearrange


# swingRate plot update
observeEvent(
  lapply(c(paste0("swingRate_",names(ds)),"dimRank_ranking"), function(x) {input[[x]]}),{
    if (length(input$dimRank_ranking) == 5) {
      output$swingRate_plot = swingRate_plot(input, ds)
      hide("dimRank_css_empty")
    }
  }
)

# LVLRATE -----------------------------------
# lvlRate txt update
output$lvlRateJs <- renderUI({
  includeScript(path = "www/lvlRateUtil.js")
})

observeEvent(input$next_handle, ignoreNULL = F,{ 
  
  output$lvlRate_preamble <- renderUI({
    lvlRatePreamble(input$next_handle)
  })
  output$lvlRate_prompt <- renderUI({
    lvlRatePrompter(input$next_handle, purples)
  })
  
  
})



# SCALED PUF FOR DCE; DEADDCE -------
###### + SET DCE states ----
  # compute personal hs ranking to determine which hs to compare
  # when user is on dce, deaddce, or tto page
observeEvent(nav_pos_is(), {
  
  if (names(agenda)[nav_pos_is()] == "Health States Choices" |
      names(agenda)[nav_pos_is()] == "Life & Death" |
      names(agenda)[nav_pos_is()] == "Time trade-off" 
  ) {
    # if scaled puf has not been computed, compute it once
    if(is.null(puf[["scaled"]])){
      
      puf[["scaled"]] = scaledFit(input, ds)
      puf[["scaled_hs_vals"]] = scaledRank(ds_expanded, puf[["scaled"]],return_rank = F)
      puf[["hs_ranks"]] = scaledRank(ds_expanded, puf[["scaled"]])
      

      
      dce_quants <- c(0.25, 0.5, 0.75)
      dce_quants <- sample(dce_quants)
      dce_difficulties <- c(0.1, 0.2, 0.3)
      dce_difficulties <- sample(dce_difficulties)
      
      puf[["dce_quants"]] = dce_quants
      puf[["dce_difficulties"]] = dce_difficulties
      
      dce_states$states <- getRefDCEState(
        scaled_values = puf[["scaled_hs_vals"]],
        ds_expanded = ds_expanded,
        quants = dce_quants,
        diff = dce_difficulties
        )
      
      renderDCE(T)
    }
  }
})




# anyDCE MECHANICS -------------------

# go through k DCE exercises
anyDCE_btn_states <- reactiveVal(c(-1,-1)) # track which button was pressen
anyDCE_counter <- reactiveVal(1)           # show i'th DCE and complete when all finished
dce_states <- reactiveValues()
renderDCE <- reactiveVal(NULL)



observeEvent(list(input$dce_A,input$dce_B,input$dce_equal, renderDCE()), {
  
  
  if(!is.null(dce_states$states)){
    
    # after init, i.e. when first choice is made
    new_btn_state <- c(
      input$dce_A,
      # input$dce_equal, 
      input$dce_B
      )
    
    if(sum(new_btn_state) > 0){
      # determine which btn was pressed
      rec_change <- c(new_btn_state) - anyDCE_btn_states()
      anyDCE_btn_states(anyDCE_btn_states() + rec_change)
      rec_change <- which.max(rec_change)
      selected_dce <- ifelse(rec_change == 1, "A", "B")
      
      selected_dce <- c(dce_states$states$A[,anyDCE_counter()], dce_states$states$B[,anyDCE_counter()], selected_dce)
      # save hs pair and choice
      puf[["anyDCE"]] <- rbind(puf[["anyDCE"]], selected_dce)
      rownames(puf[["anyDCE"]]) <- 1:nrow(puf[["anyDCE"]])
      # update hs until task is over
      if(anyDCE_counter() < ncol(dce_states$states$A)){
        anyDCE_counter(anyDCE_counter() + 1)  
      } else {
        # when task is over, go to next page
        lapply(c("dce_A", "dce_B"), disable)
        anyDCEConfirm(session)
      }
    }
    
    # render tables until table columns end
    if(anyDCE_counter() < ncol(dce_states$states$A)){
      
      # dce scenario a
      output$DCE_A_tbl <- renderDataTable({
        
        drawScenarioTbl(
          ds = ds, 
          hs = dce_states$states$A[,anyDCE_counter()],
          purples = purples
          )
        
      })
      
      # dce scenario b
      output$DCE_B_tbl <- renderDataTable({
        drawScenarioTbl(
          ds = ds, 
          hs = dce_states$states$B[,anyDCE_counter()],
          purples = purples
          )
        
      })
    }
    
  }
})

observeEvent(input$anyDCEConfirm,{
  nav_pos_is(nav_pos_is()+1)  
})






# DEADDCE MECHANICS -----------------------------------------------------

  # track responses to dce
  deadDCE_trail <- reactiveVal(NULL)

  # observe user choices in deadDCE and update trail
  observeEvent(input$dead_A, deadDCE_trail(paste0(deadDCE_trail(), "A")))
  observeEvent(input$dead_B, deadDCE_trail(paste0(deadDCE_trail(), "B")))
  observeEvent(input$dead_equal, deadDCE_trail(paste0(deadDCE_trail(), "E")))

  # rendering scenarion B hs (static: dead)
  output$deadDCE_B_tbl <- renderDataTable(drawScenarioTbl(ds = ds, hs = "dead",purples = purples))

  # rendering scenarion A hs (responsive)
  output$deadDCE_A_tbl <- renderDataTable(server = T, {
    
    scenario = deadDCESelector(
      trail = deadDCE_trail(),
      hs_ranks = puf[["hs_ranks"]],
      ds_expanded = ds_expanded
    )

    # check if deaddce is over (after 6 it, dead < 55555, or indifference)
    deadDCE_complete = deadDCEOver(deadDCE_trail())
    if (deadDCE_complete) {
      # if end, ask user to confirm or reset
      deadDCEConfirm(session)
    }

    # draw scenario A table
    drawScenarioTbl(ds = ds, hs = scenario,purples = purples)
  })

  # deadDCE is over: disable or reset
  observeEvent(input$deadDCE_confirm, {
    if (input$deadDCE_confirm) {

      puf[["pos_dead"]] = deadDCESelector(
        trail = deadDCE_trail(),
        hs_ranks = puf[["hs_ranks"]],
        ds_expanded = ds_expanded
      )
      lapply(c("dead_A", "dead_B", "dead_equal"), disable)
      
      
      # go to deadVas?
      if(length(deadDCE_trail()) == 1 &  deadDCE_trail() == "A"){
        # if pit > dead: go to deadVas 
        nav_pos_is(nav_pos_is()+1)
      } else {
        # else: skip deadVas and go to survey
        nav_pos_is(nav_pos_is()+2)  
      }
      
    } else {
      deadDCE_trail(NULL)
    }
  })
  
  
  
  
# DEADVAS ----------------------------------------
  
  # run neccessary js code chunks
  deadVas_js() 
  
  # draw pit state table
  output$pit_state_tbl <- renderDataTable(server = T, { 
    drawScenarioTbl(ds = ds, hs = c(5,5,5,5,5), purples = purples)
  })
  
  # if moved, show utility box and enable moving on
  deadVas_moved = reactiveVal(F)
  observeEvent(input$deadVas_moved, ignoreInit = T, ignoreNULL = T,{
    deadVas_moved(input$deadVas_moved)
  })
  
  # show selected pit state utility in seperate value box
  output$deadVas_out <- renderText(
    if(deadVas_moved()){
      
      puf[["dead_vas_value"]] <- c(as.numeric(input$dead_vas_slider/10))
      puf[["dead_vas_hs"]] <- cbind(c(5,5,5,5,5))
      
      return(input$dead_vas_slider)
    } else {
      return("__")
    }
  )
  

    
# SURVEY rules --------------------------------------
  # if user tries next page, but hasnt completed all fields
  # show 'required' signs to highlight missings
    onevent("mouseover", "nav_btn_nxt_tt", {
      # only apply this logic on the survey page
      if(names(agenda)[nav_pos_is()] ==  "Survey"){
        iv_list <- surveyValidator(input, output, session)
        lapply(iv_list, function(x) x$enable())
      }
    })


  # Survey: disable 'prefer not to say' questions
  observe({
    if (names(agenda)[nav_pos_is()] == "Survey") {
      survey_update(input, session)
    }
  })
  
  # increase input text field width, otherwise text is hidden
  runjs(code = '$(".selectize-input input[placeholder]").attr("style", "width: 100%;");')
  


# NEXT PAGE ALLOWED? -------------------------------------
# check if next page is allowed
observe({
  
  # responses_check(...) checks whether the
  # fields/tasks required  for the current page 
  # have been completed
  task_completed = responses_check[[nav_pos_is()]](
    input = input, 
    ds = ds, 
    rateOwnVas_moved = rateOwnVas_moved(),
    deadVas_moved = deadVas_moved(),
    progress = input$next_handle
    )  
  
  # enable/disable navigation btn accordingly
  if (task_completed) {
    hide("nav_btn_nxt_tt")
    show("nav_btn_nxt")
  } else {
    show("nav_btn_nxt_tt")
    hide("nav_btn_nxt")
  }

})

# ACTIONS TTRIGGERED AT ANY PAGE CHANGE -------------------------------------------------
# trigger next page actions
  observeEvent(nav_pos_is(),{

    # jump to top
    shinyjs::runjs("window.scrollTo(0, 0)")

    # draw header title
    output$header_title = header_title_drawer(nav_pos_is, agenda)

    # swingRate 1st ranked txt
    output$dimRank_1st_txt <- renderText({
      rank1 = which(names(ds) == input$dimRank_ranking[1])
      if(length(rank1)==0){
        "NO"
      } else {
        return(ds[[rank1]]$name2)
      }
    })
    # swingRate 1st ranked txt 2nd occurence
    output$dimRank_1st_txt3 <- output$dimRank_1st_txt2 <- renderText({
      rank1 = which(names(ds) == input$dimRank_ranking[1])
      if(length(rank1)==0){
        "NO"
      } else {
        return(ds[[rank1]]$name2)
      }
    })
# swingRate 1st ranked lvl improvement txt
    output$dimRank_1st_lvls_txt <- renderUI({
      rank1 = which(names(ds) == input$dimRank_ranking[1])
      if(length(rank1)==0){
        "NO"
      } else {
        hstr = HTML(paste0(tags$b(ds[[rank1]]$levels[5]), "<br>to: ", tags$b(ds[[rank1]]$levels[1]),"<br>"))
        return(hstr)
      }
    })



    # RESULTS PAGE GENERATION -------------------------------------------------------
    if(nav_pos_is() == length(agenda)){

    output$resultsViewer_ranks <- renderUI({
      resultsViewerRanking(input, ds)
    })
    output$resultsViewer_barplot = renderPlot({
      resultsViewer_barplot(input, ds, purples = purples)
    })

    # HS Value Comparisons
    # hs for user - population comparison
    compare_hs <- list(
      c(2, 1, 2, 1, 1),
      c(2, 3, 1, 3, 2),
      c(4, 4, 3, 3, 3),
      c(5, 5, 5, 5, 5)
    )

    # generate tables for mild, moderate, severe, and very severe HS
    output$res_hs_tbl_1 <- renderDataTable({resultsScenarioTbl(ds, compare_hs[[1]])})
    output$res_hs_tbl_2 <- renderDataTable({resultsScenarioTbl(ds, compare_hs[[2]])})
    output$res_hs_tbl_3 <- renderDataTable({resultsScenarioTbl(ds, compare_hs[[3]])})
    output$res_hs_tbl_4 <- renderDataTable({resultsScenarioTbl(ds, compare_hs[[4]])})

    # predict hs utility scores for user
    c_hs_vals_user = pufPred(
        ds_expanded = ds_expanded,
        scaled_hs_values = puf[["scaled_hs_vals"]],
        hs_eq_dead = puf[["pos_dead"]],
        dead_vas_hs = puf[["dead_vas_hs"]][,ncol(puf[["dead_vas_hs"]])],
        dead_vas_value = puf[["dead_vas_value"]][length(puf[["dead_vas_value"]])],
        new.hs = lapply(compare_hs, function(x) paste0(x,collapse = ""))
      )
    

    output$res_hs_1_user <- renderText(paste0(round(c_hs_vals_user[1], 2)*100,"%"))
    output$res_hs_2_user <- renderText(paste0(round(c_hs_vals_user[2], 2)*100,"%"))
    output$res_hs_3_user <- renderText(paste0(round(c_hs_vals_user[3], 2)*100,"%"))
    output$res_hs_4_user <- renderText(paste0(round(c_hs_vals_user[4], 2)*100,"%"))

  # predict hs utility scores for pop
    compare_hs_mat <- matrix(unlist(compare_hs),ncol = 5,byrow = T)
    colnames(compare_hs_mat) <- c("MO", "SC", "UA", "PD", "AD")
    c_hs_vals_pop<- eq5d(compare_hs_mat, version = "5L", type = "VT",country = "England")

    output$res_hs_1_pop <- renderText(paste0(round(c_hs_vals_pop[1], 2)*100,"%"))
    output$res_hs_2_pop <- renderText(paste0(round(c_hs_vals_pop[2], 2)*100,"%"))
    output$res_hs_3_pop <- renderText(paste0(round(c_hs_vals_pop[3], 2)*100,"%"))
    output$res_hs_4_pop <- renderText(paste0(round(c_hs_vals_pop[4], 2)*100,"%"))

    output$res_hs_1_qual <- renderText({uPCompare(c_hs_vals_user[1], c_hs_vals_pop[1])})
    output$res_hs_2_qual <- renderText({uPCompare(c_hs_vals_user[2], c_hs_vals_pop[2])})
    output$res_hs_3_qual <- renderText({uPCompare(c_hs_vals_user[3], c_hs_vals_pop[3])})
    output$res_hs_4_qual <- renderText({uPCompare(c_hs_vals_user[4], c_hs_vals_pop[4])})

    output$res_qq_trade <- renderUI({
        qqHelper(c_hs_vals_user[4],c_hs_vals_pop[4])
      
    })


      ### Download results as png (not an optimal solution, but another format would require much more work)
      observeEvent(input$download_res, {
        shinyjs::runjs(
          'html2canvas(document.querySelector("body")).then(canvas => {
                  saveAs(canvas.toDataURL(), "myResults.png");
            });'
        )
      })
      
    
    }
      
      
# NAVIGATION button logic------------------------------
      # disable/enable navigation buttons as required
    # disable next button, active tooltip & wait for task completion
    if (nav_pos_is() >= 2) {
      hide("nav_btn_nxt")
      show("nav_btn_nxt_tt")
    }
    if(nav_pos_is()<=2){
      hide("nav_btn_prv")
      show("nav_btn_prv_tt")
    }
    if(nav_pos_is()>=7){
      hide("nav_btn_prv")
      show("nav_btn_prv_tt")
    }
    if (nav_pos_is() == 3) { # avoid active prv btn on last page!
      show("nav_btn_prv")
      hide("nav_btn_prv_tt")
    }
    
    # solves a bug where the next button is disbaled at the beginning of page 2
    task_completed = responses_check[[nav_pos_is()]](input = input, ds = ds,rateOwnVas_moved = rateOwnVas_moved())
    
  if (task_completed) {
    hide("nav_btn_nxt_tt")
    show("nav_btn_nxt")
  }


# FOOTER ------------------------------
    # footer: update progress bar
    updateProgressBar(
      id = "progress_bar",
      session = session,
      value = nav_pos_is()-1,
      total= length(agenda)-1
      )
    # don't show footer at landing page
    output$nav_pos_is <- reactive(nav_pos_is())
    outputOptions(output, "nav_pos_is", suspendWhenHidden = FALSE)

  })




# keep track of time per page
tab_times <- reactiveValues()
observeEvent(nav_pos_is(),{
  page_is = names(agenda)[nav_pos_is()]
  # account for multiple page (re)visits of pages
  page_index = isolate(
    as.numeric(input$nav_btn_nxt) + as.numeric(input$nav_btn_prv) +
      consent_given() + data_submitted()
  )
  tab_times[[paste0(page_index,"_",page_is)]] <- Sys.time()
})


# COLLECT DATA ---------------------
# triggered by 'SUBMIT btn'
observeEvent(input$submit_data_btn,priority = 1, {
  disable("submit_data_btn")
})

  observeEvent(input$submit_data_btn,{

    data_submitted(1)

    # show a spinner for 2 secs of drama
    show("submit_data_spinner")

    # wrapped in rendertext for the spinner, no text is rendered
    output$data_sent <- renderText({

      if(session$clientData$url_search != ""){
        file_prefix = paste0(session$clientData$url_search,"_")
      } else {
        file_prefix = "n1000"
      }
      
      collect_responses(
        input = input,
        ds = ds, 
        deadDCE_trail = isolate(deadDCE_trail()),
        puf = puf,
        tab_times = tab_times,
        file_prefix = file_prefix
      )


      print(paste0(Sys.time(), " - Done!")) # print to console log

      ""
    })

    removeModal()

    show("nav_btn_nxt_tt")
    hide("nav_btn_nxt")

    nav_pos_is(nav_pos_is() + 1)
  })

  
}

shinyApp(ui = ui, server = server)

# fin.
