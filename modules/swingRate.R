

# swingRate
# generates page for dimension swing weights

swingRate_ui = function(input, ds, ...) {
    list(
        # headline
        fluidRow(
            column(
                offset = 1,
                width = 7,
                vsm(1),
                h2("Comparing health improvements"),
                HTML("<p>You selected <span class='hl_text_inline'>"),
                textOutput("dimRank_1st_txt", inline = T),
                HTML(
                    "</span> as the area that is most important to you.</p>

                Now, if an improvement from:"
                ),
                uiOutput("dimRank_1st_lvls_txt", inline = T),
                HTML(
                    "is worth <b>100 health points</b>, how many
                points would you give to improvements in other areas?</p>
                    "
                )
            ),
            column(4)
        ),
        # slider colors match bars in barplot
        setSliderColor(unlist(lapply(seq_along(ds), function(x) {
            ds[[x]]$col
        })), c(1:5)),
        # slider inputs
        fluidRow(
            # note 1
            column(
                offset = 1,
                width = 5,
                div(
                    class = "card",
                    span(class = "hl_text_inline", "Note:"),
                    HTML("100 points are assigned to "), textOutput("dimRank_1st_txt2", inline = T),"- ",
                    HTML("This is fixed. Use it as a yardstick to rate the other areas"),
                    tags$li(
                        "100 points means the area is as important as",
                        textOutput("dimRank_1st_txt3", inline = T)
                    ),
                    tags$li("50 points means it is half as important"),
                    tags$li("0 points means it is not important at all")
                ),
                
                # validation plot output
                div(
                    class = "card",
                    # add css to keep div in viewport when scrolling
                    style = "
                    position: sticky;
                    position: -webkit-sticky;
                    top: 100px;",
                    plotOutput("swingRate_plot", width = "100%")
                ),
                
                div(id="sticky_spacer", style="height: 120vh;")
            ),
            
            # slider column right
            column(
                offset = 0,
                width = 5,
                lapply(seq_along(ds), function(i) {
                    div(
                        class = "card",
                        id = paste0("swingRate_card_", names(ds)[i]),
                        div(class = "control-label",
                            ds[[i]]$name, style = "padding-bottom:10px;"),
                        column(
                            width = 12,
                            align = "center",
                            HTML("<p><b>How important is this improvement to you? </b></p>"),
                            vsm(1),
                            div(
                                style = "display: flex; justify-content: center;",
                                div(class = "swingRate_lvl_box",
                                       ds[[i]]$levels[5]),
                                div(style = "display: inline-block; width: 15%; font-size: 150%; text-align: center;",
                                    vsm(1),
                                    HTML("</b> &rarr;</b> ")
                                    ),
                                div(class = "swingRate_lvl_box",
                                       ds[[i]]$levels[1])
                        )),
                        sliderInput(
                            inputId = paste0("swingRate_", names(ds)[i]),
                            label = "",
                            width = "95%",
                            value = 0,
                            min = 0,
                            max = 100
                        )
                    )
                })
    )
    )

)
}

# take 1st rank from dimRank task and set it to 100 (fixed)
swingrate_slider_update = function(input, ds, session) {
    if (!is.null(input)) {
        rank1 = names(ds) == input[1]
        not_rank1 = names(ds)[names(ds) != input[1]]
        lapply(seq_along(rank1), function(i) {
            updateSliderInput(session,
                              paste0("swingRate_", names(ds)[i]),
                              value = ifelse(rank1[i], 100, 0))
        })
        # fix value at 100
        disable(paste0("swingRate_", names(ds)[rank1]))
        
        lapply(not_rank1, function(x)
            enable(paste0("swingRate_", x)))
    }
}

# generate the validation bar plot
swingRate_plot = function(input, ds) {
    require(ggplot2)
    require(statebins)
    require(ggthemes)
    
    dims = names(ds)
    dim_labels = unlist(lapply(ds, function(x)
        x$name2))
    dim_colors = unlist(lapply(ds, function(x)
        x$col))
    weigths = unlist(lapply(dims, function(x) {
        input[[paste0("swingRate_", x)]]
    }))
    
    x <- match(names(ds), input$dimRank_ranking)
    
    df = data.frame(dims, weigths, dim_labels, dim_colors, x = x)
    df = df[order(df$dims), ] # important to have the colors match
    df5 = df[df$weigths <= 5, ]
    df10 = df[df$weigths <= 10 & df$weigths > 5, ]
    df20 = df[df$weigths <= 20 & df$weigths > 10, ]
    df100 = df[df$weigths > 20,]
    shadow_df = data.frame(x = 1:5, weigths = 100)
    
    
    p_rendered = renderPlot({
        ggplot() +
            # adjusting the round edges depending on the height, 
            # to avoid weird looking bars
            geom_chicklet(
                data = shadow_df,
                width = 0.5,
                aes(x = x, y = weigths),
                fill = "#D8DBE1",
                alpha = 0.3,
                radius = grid::unit(20, "pt")
            ) +
            geom_chicklet(
                data = df5,
                width = .5,
                aes(
                    x = x,
                    y = weigths,
                    fill = dims
                ),
                alpha = 0.7,
                radius = grid::unit(0, "pt")
            ) +
            geom_chicklet(
                data = df10,
                width = .5,
                aes(
                    x = x,
                    y = weigths,
                    fill = dims
                ),
                alpha = 0.7,
                radius = grid::unit(10, "pt")
            ) +
            geom_chicklet(
                data = df20,
                width = .5,
                aes(
                    x = x,
                    y = weigths,
                    fill = dims
                ),
                alpha = 0.7,
                radius = grid::unit(15, "pt")
            ) +
            geom_chicklet(
                data = df100,
                width = .5,
                aes(
                    x = x,
                    y = weigths,
                    fill = dims
                ),
                alpha = 0.7,
                radius = grid::unit(20, "pt")
            ) +
            # simple theme
            theme_hc() +
            scale_y_continuous(position = "left",
                               limits = c(0, 100),
                               name = "") +
            # set labels for axes
            scale_x_continuous(
                name = "",
                breaks = df$x,
                labels = df$dim_labels
            ) +
            scale_fill_manual(values = df$dim_colors) +
            xlab("") +
            theme(
                legend.position = "none",
                axis.text.x = element_text(
                    angle = 15,
                    vjust = 0.5,
                    face = "bold",
                    size = 13
                ),
                axis.text.y = element_text(size = 14, hjust = -0.1)
            )
    })
    return(p_rendered)
}