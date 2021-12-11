# dimRank


dimRank_ui <- function(ds,...){
    require(sortable)

labels = unlist(lapply(ds, function(x) x$levels[length(x$levels)]))

res = list(

    # headline
    fluidRow(
        column(
            offset = 1, 5,
            vsm(1),
            h2("Ranking"),
            HTML("
            
            <p>Consider the following five statements.</p>
            Imagine what it would be like for you to experience that problem, 
            but no other health problems.</p> 
            
            <p>Rank all five problems 
            by dragging them from the left to the box on the right.</p>")
        ),
        # note box
        column(
            width = 5,
            vsm(7),
            panel(
                span(
                    class = "hl_text_inline",
                    "Note:"
                ),
                HTML("Start with the <span class='hl_text_inline'>WORST</font></span> (Rank 1): which health problem would have the most negative impact on you?")
            )
        )
    ),
vsm(1),
    # bucket list 1
    fluidRow(
        column(
            offset = 1, 5,
            bucket_list(
                header = "",
                group_name = "bucket_list_group",
                orientation = "vertical",
                class = c("default-sortable","drag-sortable"),
                add_rank_list("Health problems:",
                    labels = labels,
                    input_id = "dimRank_bucket",
                    css_id = c("dimRank_css_empty")
                )
            )
        ),

        # bucket list 2
        column(
            width = 5,
            div(
                class = "custom",
                bucket_list(
                    header = "",
                    group_name = "bucket_list_group",
                    orientation = "horizontal",
                    class = c("default-sortable","drag-sortable"),
                    add_rank_list("Ranking:",
                        labels = NULL,
                        input_id = "dimRank_ranking",
                        css_id = c("dimRank_css_1")
                    )
                )
            )
        )
    )
)

return(res)
}




dimRank_completed = function(input,id = "dimRank_ranking", ...){
    responded = input[[id]]
    if (is.null(responded)) {
        return(F)
    }
    if(length(responded)==5){
        return(T)
    } else {
        return(F)
    }
}


