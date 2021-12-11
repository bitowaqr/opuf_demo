# style elements
# some ui elements that are used in the app

# utility functions to create horizontaal or vertical space
hsm = function(n = 1) {
  HTML(paste0(rep("&nbsp;", n), collapse = ""))
}
vsm = function(n = 1) {
  HTML(paste0(rep("<br>", n), collapse = ""))
}


# Avoid mobile use
# the app is not optimised for small screens
# if a mobile / small screen is detected, this message is shown
avoid_mobile_view = function() {
  div(
    class = "mobile-notification",
    br(),
    h1("Sorry..."),
    h4("This page is not optimised for mobile devices."),
    br(),
    HTML("Please use a computer, laptop, or a tablet to access the "),
    span(class = "hl_text_inline", "OPUF Health Preference Survey"),
    HTML("."),
    br(),
    br(),
    br(),
    HTML("If you continue to experience problems, please contact:"),
    HTML(
      '<a href = "mailto: p.schneider@sheffield.ac.uk">p.schneider@sheffield.ac.uk</a>'
    )
  )
}

# loading page screen ui
loading_screen = function() {
  div(
    span(class = "loading_txt1",
         "Loading... "),
    br(),
    br(),
    br(),
    span(class = "loading_txt2",
         "The OPUF Health Preference Survey"),
    br(),
    br(),
    spin_wave()
  )
}

# header ui
header = function() {
  logo_right = img(src = "wellcome.png", height = "70px")
  logo_left = img(src = "sheffield_logo.png", height = "70px")
  
  
  
  absolutePanel(
    left = 0,
    right = 0,
    top = 0,
    class = "header_panel",
    fluidRow(
      column(
        offset = 1,
        width = 2,
        logo_left,
        align = "left",
        style = "padding-top:2px"
      )
      ,
      column(
        width = 6,
        align = "center",
        style = "padding-top:25px",
        uiOutput("header_title", inline = T),
      ),
      column(
        width = 2,
        logo_right,
        align = "right",
        style = "padding-top:2px"
      ),
      
    )
  )
}

# header server side
header_title_drawer = function(nav_pos_is, agenda) {
  renderUI({
    if (nav_pos_is() > 0) {
      header1 = paste0("Part ", nav_pos_is() - 1, ":")
      header2 = names(agenda)[nav_pos_is()]
    } else {
      header1 = header2 = ""
    }
    
    res = list(div(class = "header_title_1", header1),
               div(class = "header_title_2", header2))
    
    return(res)
    
  })
}


# footer ui side
# input ids:
# - progress_bar
# - nav_btn_prv
# - nav_btn_nxt
footer_panel = function(agenda) {
  fixedPanel(
    class = "footer",
    left = 0,
    right = 0,
    bottom = 0,
    
    column(
      offset = 1,
      width = 2,
      div(
        style = "font-size:90%; color: var(--background); line-height: 20px;",
        HTML(
          "If you encounter any technical issues, please
               <a href = 'mailto: p.schneider@sheffield.ac.uk'><u>contact us</a>."
        )
      )
      
    ),
    
    column(
      offset = 0,
      width = 6,
      progressBar(
        id = "progress_bar",
        value = 2,
        total = length(agenda),
        display_pct = T,
        unit_mark = "",
        range_value = c(1:length(agenda)),
        striped = F
      )
    ),
    column(
      id = "no_wrap_col",
      offset = 1,
      width = 2,
      circleButton(
        inputId = "nav_btn_prv",
        icon = icon("arrow-left"),
        status = "danger",
      ),
      span(
        id = "nav_btn_prv_tt",
        'data-toggle' = "tooltip",
        'title' = "It is not possible to return to the previous page",
        circleButton(
          inputId = "nav_btn_prv_disabled",
          icon = icon("arrow-left"),
          status = "danger"
        )
      ),
      hsm(2),
      circleButton(
        inputId = "nav_btn_nxt",
        icon = icon("arrow-right"),
        status = "danger"
      ),
      span(
        id = "nav_btn_nxt_tt",
        'data-toggle' = "tooltip",
        'title' = "Please complete all tasks on this page before you continue",
        circleButton(
          inputId = "nav_btn_nxt_disabled",
          icon = icon("arrow-right"),
          status = "danger"
        )
      )
    )
  )
}



# modal dialogs

# final modal, shown after the last task, before the user proceeds to the results page
modalSubmitData = function(input, data_submitted) {
  showModal(modalDialog(
    title = NULL,
    div(
      style = "padding: 20px; padding-top:50px; font-size:100%; text-align:center;",
      img(src = "./trophy.svg"),
      br(),
      h1("You have completed all tasks!"),
      br(),
      h3("Thank you for your participation."),
      br(),
      
      HTML(paste0(
        h4(
          "Proceed to the next page to view your ",
          span(class = "hl_text_inline", "personal results")
        )
      )),
      br(),
      div(id = "submit_data_spinner",
          withSpinner(
            type = 6,
            textOutput("data_sent", inline = T)
          )),
    ),
    easyClose = F,
    size = "m",
    footer = column(
      offset = 1,
      width = 10,
      align = "center",
      span(
        "data-dismiss" = "modal",
        actionBttn("modal_cancel", "Cancel", style = "jelly")
      ),
      hsm(2),
      actionBttn(
        "submit_data_btn",
        "View results",
        style = "jelly",
        color = "success"
      )
    )
  ))
  hide("submit_data_spinner")
}


modalhelp = function(nav_pos_is, agenda) {
  showModal(modalDialog(title = "Instructions",
                        HTML(
                          paste0(
                            "Instructions for the following task will be shown here: <br><br>",
                            span(style = "padding-left:30px", class = "hl_text", names(agenda)[nav_pos_is()])
                          )
                        ),
                        easyClose = T))
}





# spinner with min spin duration of 1.5 secs

withSpinner <-
  function (ui_element,
            type = getOption("spinner.type", default = 1),
            color = getOption("spinner.color", default = "#0275D8"),
            size = getOption("spinner.size", default = 1),
            color.background = getOption("spinner.color.background"),
            custom.css = FALSE,
            proxy.height = NULL,
            id = NULL,
            image = NULL,
            image.width = NULL,
            image.height = NULL,
            hide.ui = TRUE)
  {
    stopifnot(type %in% 0:8)
    if (grepl("rgb", color, fixed = TRUE)) {
      stop("Color should be given in hex format")
    }
    if (is.character(custom.css)) {
      stop(
        "It looks like you provided a string to 'custom.css', but it needs to be either `TRUE` or `FALSE`. ",
        "The actual CSS needs to added to the app's UI."
      )
    }
    if (is.null(id)) {
      id <- paste0("spinner-", digest::digest(ui_element))
    }
    if (is.null(image)) {
      css_size_color <- shiny::tagList()
      if (!custom.css && type != 0) {
        if (type %in% c(2, 3) && is.null(color.background)) {
          stop("For spinner types 2 & 3 you need to specify manually a background color.")
        }
        color.rgb <- paste(grDevices::col2rgb(color), collapse = ",")
        color.alpha0 <- sprintf("rgba(%s, 0)", color.rgb)
        color.alpha2 <- sprintf("rgba(%s, 0.2)", color.rgb)
        css_file <-
          system.file(glue::glue("loaders-templates/load{type}.css"),
                      package = "shinycssloaders")
        base_css <- ""
        if (file.exists(css_file)) {
          base_css <- paste(readLines(css_file), collapse = " ")
          base_css <- glue::glue(base_css, .open = "{{",
                                 .close = "}}")
        }
        size <- round(c(11, 11, 10, 20, 25, 90, 10, 10)[type] *
                        size * 0.75)
        base_css <-
          paste(base_css, glue::glue("#{id} {{ font-size: {size}px; }}"))
        css_size_color <- add_style(base_css)
      }
    }
    proxy_element <- get_proxy_element(ui_element, proxy.height,
                                       hide.ui)
    shiny::tagList(
      shiny::singleton(shiny::tags$head(
        shiny::tags$link(rel = "stylesheet",
                         href = "shinycssloaders-assets/spinner.css"),
        shiny::tags$script(src = "spinner2.js")
      )),
      if (is.null(image))
        shiny::singleton(shiny::tags$head(
          shiny::tags$link(rel = "stylesheet",
                           href = "shinycssloaders-assets/css-loaders.css")
        )),
      if (is.null(image))
        css_size_color,
      shiny::div(
        class = paste(
          "shiny-spinner-output-container",
          if (hide.ui)
            "shiny-spinner-hideui"
          else
            "",
          if (is.null(image))
            ""
          else
            "shiny-spinner-custom"
        ),
        shiny::div(
          class = paste(
            "load-container",
            "shiny-spinner-hidden",
            if (is.null(image))
              paste0("load", type)
          ),
          if (is.null(image))
            shiny::div(id = id, class = "loader", (if (type ==
                                                       0)
              ""
              else
                "Loading..."))
          else
            shiny::tags$img(
              id = id,
              src = image,
              alt = "Loading...",
              width = image.width,
              height = image.height
            )
        ),
        proxy_element,
        ui_element
      )
    )
  }
# overwrite normal spinner
environment(withSpinner) <- asNamespace('shinycssloaders')
# assignInNamespace("withSpinner",withSpinner,ns="shinycssloaders")
