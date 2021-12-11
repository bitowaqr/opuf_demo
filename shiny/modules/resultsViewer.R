# results viewer


legend_drawer = function(lvls, purples){
    res = list()
        for (l in seq_along(lvls)) {
            res[[l]] =

                    div(class="rect_wrap",

                        div(
                            class = "rectangle",
                            style = paste0("background-color:", purples[l])
                        ),
                        HTML(lvls[l])
                    )

        }
    return(res)
}



userPopHsCompare <- function(i,title = "Very severe health state"){
    column(
                        offset = 0, width = 6,
                        div(
                            class = "card",
                            column(
                                5,
                                div(title,class="hs_hl_text"),
                                HTML("Compared to the general public, you assigned this health state a <b>"),
                                textOutput(paste0("res_hs_",i,"_qual"), inline = T),
                                HTML("</b> score."),
                                br(),br(),
                                div(class = "hs_val_pre","Your score: "),
                                div(class = "hs_res_score",
                                    textOutput(paste0("res_hs_",i,"_user"), inline = T)
                                ),
                                br(),br(),
                                div(class = "hs_val_pre","Population: "),
                                div(class = "hs_res_score",
                                    textOutput(paste0("res_hs_",i,"_pop"), inline = T)
                                ),
                            ),
                            column(
                                width = 7, 
                                div(
                                    style = "border:1px solid gray; border-radius:10px; padding: 5px; padding-top: -5px;",
                                    dataTableOutput(paste0("res_hs_tbl_", i))
                                )
                                ),
                            hsm(1)
                        )
                    )
}


resultsViewer_ui <- function(purples,ds,...) {


      list(

          # headline
          fluidRow(
            
            
            
            
            
              column(
                  offset = 1, 5,
                  vsm(1),
                  h2("Your Results"),
                  p("View your personal results and compare them against the responses of the general population."),
                  p("You can also download your results - just click on the download button below.")
                  ),
              # note box
              column(
                  width = 5,
                  vsm(5),
                  panel(
                      span(
                          class = "hl_text_inline",
                          "Note:"
                      ),
                      HTML("The <i>population average</i> summarises the values of
                      several hundred people into a single score. But everyone has their own preferences.
                      Your results may thus well differ from the average. ")
                  )
              )

          ),

          
          fluidRow(
            
            # dim ranking you vs pop
            uiOutput("resultsViewer_ranks", width = "100%"),

              # BARPLOT + legend
              column(
                  offset = 0,
                  width = 5,
                  align="center",
                  div(class = "card",
                  column(12,align="left",div("Health problems",class="hl_text")),
                      legend_drawer(c("Slight","Moderate","Severe","Extreme"), purples[-1]),
                      vsm(2),
                      plotOutput("resultsViewer_barplot",height = "230px")

                  )

              )
          ),


            # HS value comparisons
          fluidRow(
              column(
                  width = 10, offset = 1,

                  # intro
                      div(
                          class = "card",
                          fluidRow(
                          column(
                            width = 5,style = "padding-left: 50px;",
                            div(class = "hs_hl_text", "Scoring health problems"),
                          p("Based on your responses, we predict your attitude towards health. You can compare this with the general population."),
                          p("The scores below reflect how much health you think is in those health states.")
                          ),
                          column(
                            offset = 1,
                            width = 5,
                            "Health scores:",
                          tags$li("A score of 100% means perfect health, no health is lost"),
                          tags$li("A score of 50% means 50% of your health is lost."),
                          tags$li("A score of 0% means all health is lost."),
                          tags$li("Scores lower than 0% are assigned to problems that seem worse than being dead."),
                            )),
                          hsm(1)
                      
              ),

                  # HS COMPARISONS
                  userPopHsCompare("1", "Mild health problems"),
                  userPopHsCompare("2", "Moderate health problems"),
                  userPopHsCompare("3", "Severe health problems"),
                  userPopHsCompare("4", "Extreme health problems")
              ),


              column(10,offset = 1,
                # QQ tradeoff
                  column(
                        6,
                        div(
                            class = "card",
                            div(class = "hs_hl_text", "Quality-quantity trade-off"),
                            br(),
                            p(HTML("
                            For some people, it is more important to live a <i>long</i> life,
                            while others put more emphasis on the <i>quality</i> of their life."
                            )),
                            uiOutput("res_qq_trade", inline = T)
                        )
                    ),

                     # print/download results
                  column(
                        6,
                        div(
                            class = "card",
                            div(class = "hs_hl_text", "Save your results"),
                            p(
                                "You can download your results to your computer in a .png format."
                            ),
                            br(),
                            column(
                                12,align="center",
                                actionBttn("download_res","Download My Results",style = "jelly",  size ="md")
                            ),
                            hsm(1)
                        )
                        )
                    )
              ),
            
            fluidRow(
                vsm(2),
                column(6,
                    offset = 3, align = "center",
                    
                    

                    
                        div(
                            class = "card",style = "padding:40px;",
                            div(class = "hs_hl_text", "Thank you!", style = "font-size: 130%; margin:30px;"),
                            p("
                            Thank you for participating in this study. 
                            We very much appreciate your time and hope you found 
                            the survey interesting. If you have any questions or comments,
                            please send an email.
                            "),
                            br(),
                            HTML("<b>Paul Schneider</b><br>"),
                            HTML("School of Health and Related Research<br>"),
                            HTML("University of Sheffield<br>"),
                            HTML('<a href = "mailto: p.schneider@sheffield.ac.uk"><u>p.schneider@sheffield.ac.uk</a>')
                        )
                    )
                

            )
      )
}

resultsViewerRanking <- function(input, ds){
  
  pop_ranking <- c("D","E","A","B","C")
  user_ranking <- input$dimRank_ranking
  
  dim_labs <- c()
  for(d in seq_along(ds)){
    dim_labs <- c(dim_labs, ds[[d]]$name2)
  }
  pop_labs <- dim_labs[match(pop_ranking,names(ds))]
  pop_labs <- paste0(1:5,". ", pop_labs)
  user_labs <- dim_labs[match(user_ranking,names(ds))]
  user_labs <- paste0(1:5,". ", user_labs)
  
  
    column(
      width = 5,offset = 1,
      bucket_list(
        header = "",
        group_name = "pop_labs_g",
        orientation = "horizontal",
        options = sortable_options(disabled = T),
        add_rank_list(text = HTML(paste0(
          div(class = "hl_text", "Ranking", style = "margin-top: -10px;"),
          div("Your ranking:", style="margin-left: 10px;")
          )),
                      labels = user_labs,
                      options = sortable_options(disabled = T),
                      input_id = "user_labs_x"
                      ),
        add_rank_list(
          HTML(paste0(
            div(class = "hl_text", hsm(1), style = "margin-top: -10px;"),
          div("General population:", style="margin-left: 10px;")
      )),
                      labels = pop_labs,
                      options = sortable_options(disabled = T),
                      input_id = "pop_labs_x"
        )
        )
      )
  
  
}

resultsViewer_barplot = function(input, ds,purples = c("#F2F0F7","#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA")){

    require(ggplot2)

    pop_average = c(0, 0.1492338, 0.6751080, 0.7624641, 1) * 100 # see pop_norm.R file

    res = c(input$lvlRate)

    len = length(res)
    whose_res <- c(rep(1 , len-1) , rep(2 , len-1))
    lvl <-  as.factor(rep((len-1):1 ,  2))

    value <- c(res[-1],pop_average[-1])
    bar_dat <- data.frame(whose_res,lvl,value,stringsAsFactors = F)
    bar_dat$lvl = as.numeric(bar_dat$lvl)
    bar_dat$whose_res = as.factor(bar_dat$whose_res)
    bar_dat$value = abs(bar_dat$value)
    bar_labs = ds$E$lvlsRate_args
    bar_dat = bar_dat[order(bar_dat$lvl), ]

    # # Stacked
    ggplot(bar_dat, aes(fill=lvl, y=value, x=as.factor(whose_res)),alpha=0.5) +
      # geom_bar(position="fill",stat="identity") +
      geom_chicklet(width = .5, alpha = 1,radius = grid::unit(10, "pt"),position = "identity") +
      scale_fill_gradientn(colours = c("transparent",purples),
                           breaks=c(-1,0,1,2,3,4,Inf),
                           limits = c(0,4),name = "",
                           labels=c("",as.character(bar_labs),"INF")) +
      scale_x_discrete(labels = c("Your result", "General population", "People with\n health problems")) +
          scale_y_continuous(
              breaks = c(0, .25, .5, .75, 1) * 100,
              labels = c(0, 25, 50, 75, 100),
              name = ""
              #name = "Valuation of different levels of health") +
          ) +
              
          xlab("") +
          # ggtitle("Severity") +
          theme_minimal() +
          theme(
              legend.position = "none",
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.y = element_text(size=18)
          )
    #     guides(fill = guide_legend(label.vjust = -6,title.vjust = -4 ,reverse = F)) +
    #   theme(
    #     legend.spacing.y = unit(.5, "char"), # adds spacing to the left too
    #     legend.justification = c(1, 1),
    #     legend.margin = margin(c(5, 5, 5, 0)),
    #     legend.text = element_text(margin = margin(b = 10,t=10, unit = "pt"),size = 12),
    #     legend.title = element_text(margin = margin(b = 3,t=3, unit = "pt"),size = 14,face = "bold"))


}




resultsScenarioTbl = function(
    ds, hs = c(1,2,3,4,5),
    purples = c("#F2F0F7","#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA")
    ){

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


dtable <- dtable %>%
        formatStyle(1:2,height="50px") %>%
        formatStyle(1,'border-top-left-radius'="5px") %>%
        formatStyle(1,'border-bottom-left-radius'="5px") %>%
        formatStyle(2,'border-top-right-radius'="5px") %>%
        formatStyle(2,'border-bottom-right-radius'="5px") %>%
        formatStyle(1,'font-weight'="600") %>%
        formatStyle(1:2,valueColumns = 2,  backgroundColor = styleEqual(lvl_labs,cell_cols))


    return(dtable)
}


# utility function for detemrinig whether user or pop has higher/lower 
# score and turn that into a string
uPCompare = function(x,y, dec = 2){
    x = round(x,dec)
    y = round(y,dec)
  diff = abs(x - y)
  if(diff<0.01){
    suffix = ""
  }
  if(diff>=0.01 &  diff<0.1){
    suffix = "slightly "
  }
  if(diff>=0.1 & diff < 0.25){
    suffix = ""
  }
  if (diff >= 0.25) {
    suffix = "much "
  }
  
  if(x > y){str = ("higher")}
  if(x < y){str = ("lower")}
  if (x == y) {
    str = ("similar")
  }
  str_s = paste0(suffix, str)
  return(str_s)
}



# utility function to evaluate Q-Q tradeoff and generate string
qqHelper <- function(val_user, val_pop) {

        res = "Your responses are very similar to the general public's view. 
        You seem to put similar weight on quality of life and length of life."
        
        qq_tradeoff_comp = ((1 - val_user) - (1 - val_pop))

        if (qq_tradeoff_comp > 0.1) {
          res = "
          Your responses indicate that you prioritise <b>quality of life</b>: 
          compared to the general public, you put more weight on
          being in good health."
        }
        
        if (qq_tradeoff_comp < -0.1) {
          res = "
          Your responses indicate that you prioritise <b>longevity</b>: 
          compared to the general public, you put more weight on
          living a long life."
        }

        return(HTML(res))
    }

