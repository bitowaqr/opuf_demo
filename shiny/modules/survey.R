# survey
# generates a survey page

# utility function to create cards with different types of questions
surveySelectize = function(
  id = "sex",
  question = "What is your sex?",
  placeholder = "Select from the list",
  choices = c("Female", "Male", "Other"),
  multiple = F,
  radio = F
  ) {
  
  list(fluidRow(
    column(
      style = "font-size:105%; font-weight:350;",
      if (radio == T |
          radio == "checkbox") {
        width = 10
      } else{
        width = 8
      },
      offset = 0,
      
      if (radio == T) {
        prettyRadioButtons(
          inputId = paste0("survey_radio_", id),
          label = span(class = "survey_question", question),
          choices = c(choices, "Prefer not to say"),
          status = "primary",
          bigger = T,
          # selected = "Prefer not to say",
          selected = character(0),
          width = "100%",
          outline = T,
          inline = T
        )
      },
      
      if (radio == "checkbox") {
        checkboxGroupInput(
          inputId = paste0("survey_radio_", id),
          label = span(class = "survey_question", question),
          choices = c(choices, "Prefer not to say"),
          # status = "primary",bigger = T,
          # selected = "Prefer not to say",
          selected = character(0),
          # outline = T,
          inline = F,
          width = "100%"
        )
        
        
      } ,
      
      if (radio == F) {
        selectizeInput(
          paste0("survey_select_", id),
          width = "100%",
          label = span(class = "survey_question", question),
          options = list(
            placeholder = placeholder,
            onInitialize = I('function() { this.setValue(""); }')
          ),
          choices = choices,
          multiple = multiple
        )
      }
      
    ),
    column(if (radio == T |
               radio == "checkbox") {
      width = 1
    } else {
      width = 4
    },
    style = "padding-top:25px; padding-left:20px;",
    if (radio == T  |
        radio == "checkbox") {
      hsm(1)
    } else {
      checkboxInput(paste0("survey_ptns_", id), "Prefer not to say")
    },)
  ))
}


# generate UI element
survey_ui = function(...){

    list(
        # headline
        fluidRow(
            column(
                offset = 1, width = 7,
                vsm(1),
                h2("Demographic Information"),
                h4("Fill in the form below to the best of your ability."),
            ),
            column(5)
        ),

        # PERSONAL 1
        fluidRow(
            column(
                offset = 1,
                width = 7,

                div(
                    class = "card",style="padding-left:40px;",

                    div(class="hl_survey_label","Personal Information"),
                    vsm(1),

                    # AGE ------------
                    surveySelectize(
                        id = "age",
                        question = "How old are you?",
                        choices = c(
                            "18-24 years old",
                            paste0(20 + 5 * 1:11, "-", 24 + 5 * 1:11, " years old"),
                            "80 years or older"
                        )
                    ),
                    br(),

                    # PARTNER ----------------------------
                    surveySelectize(
                        id = "partner",
                        question = "What is your partnership status",
                        choices = c(
                            "Single",
                            "Married/Civil partnered/Living together",
                            "Divorced/Separated",
                            "Widowed",
                            "Other"
                        )
                    ),
                    br(),

                    # SEX ----------------------------
                    surveySelectize(
                        id = "sex",
                        radio = T,
                        question = "What is your sex?",
                        choices = c("Female", "Male", "Other"),
                    ),
                    br(),


                    # CHILDREN ----------------------------
                    surveySelectize(
                        id = "children",
                        question = "Do you have children?",
                        radio = T,
                        choices = c("Yes","No")
                    )
                ),


         # Ethnicity and religion carc
         div(
             class = "card", style = "padding-left:40px;",
             div(class = "hl_survey_label", "Nationality & Religion"),
             vsm(1),
             # NATIONALITY ------------------
             surveySelectize(
                 id = "nationality",
                 question = "How would you describe your national identity?",
                 placeholder = "Please select all that apply",
                 choices = c(
                     "English ",
                     "Welsh ",
                     "Scottish ",
                     "Northern Irish ",
                     "British ",
                     "European/EEA ",
                     "Other"
                 ),
                 multiple = T
             ),
             br(),


        # COUNTRY OF BIRTH ------------------
             surveySelectize(
                 id = "birthcountry",
                 question = "Were you born in the UK?",
                 placeholder = "Please select all that apply",
                 choices = c(
                     "Yes","No"
                 ),
                 radio = T,
                 multiple = F
             ),
             br(),
             
             # RELIGION ------------------
             surveySelectize(
                 id = "faith",
                 question = "How important is religion, spirituality, or faith in your life?",
                 choices = c(
                     "Very important",
                     "Moderately important",
                     "Slightly important",
                     "Not important"
                 )
             ),
             br(),

             # FAITH ------------------
             surveySelectize(
                 id = "religious_practice",
                 question = "How often do you engage in 
                 religious or spiritual activities, such as
                 prayer, attending services, meditation, etc.?",
                 choices = c(
                     "Every day",
                     "A few times a week ",
                     "Once a week",
                     "A few times a month",
                     "A few times a year",
                     "Never or practically never"
                 ),
             ),
             br()
         ),


         # WORK AND EDUCATION CARD
         div(
             class = "card", style = "padding-left:40px;",
             div(class = "hl_survey_label", "Work & Education"),
             vsm(1),

             # Education ------------
             surveySelectize(
                 id = "education",
                 question = "What is your highest level of education?",
                 choices = c(
                     "Left school without qualifications",
                     "GCSE/Standard grade",
                     "A-Level/Higher grade",
                     "Certificate/Diploma/NVQ",
                     "Degree",
                     "Post-graduate"
                 )
             ),
             br(),

             # WORK ------------
             surveySelectize(
                 id = "work",
                 question = "Which of these best describes your usual situation in regard to work?",
                 choices = c(
                   "Employed or self\u2212employed",
                     "Retired",
                     "Student",
                     "Looking after home or family",
                   "Long\u2212term sick or disabled",
                     "Other/none of the above"
                 )
             ),
             br(),

             # INCOME ------------------
             surveySelectize(
                 id = "income",
                 question = "What is your annual household income before taxes?",
                 choices = c(
                   # gbp sign
                   "\u00A30 \u2212 \u00A310,000",
                   "\u00A310,001 \u2212 \u00A320,000",
                   "\u00A320,001 \u2212 \u00A330,000",
                   "\u00A330,001 \u2212 \u00A340,000",
                   "\u00A340,001 \u2212 \u00A350,000",
                   "\u00A350,001 \u2212 \u00A360,000",
                   "\u00A360,001 \u2212 \u00A370,000",
                   "\u00A370,001 \u2212 \u00A380,000",
                   "\u00A380,001 \u2212 \u00A390,000",
                   "\u00A390,001 \u2212 \u00A3100,000",
                   ">\u00A3100,000"
                 )
             )
         ),


         # Poor health and caring
         div(
             class = "card", style = "padding-left:40px;",
             div(class = "hl_survey_label", "Experience of poor health"),
             vsm(1),
             
             # # POOR HEALTH EXPERIENCE------------------
             surveySelectize(
                 id = "health_experience",
                 question = "Which of the following statements about your experience with severe health problems apply? (Please select all that apply)",
                 radio = "checkbox",
                 choices = c(
                     "I have severe health problems.",
                     "I have experienced severe health problems in the past.",
                     "Someone in my family has or had severe health problems.",
                     "I am caring for someone with health problems.",
                     "I work in the healthcare sector.",
                     "None of the above apply."
                 )
             ),
             br(),
         )
         )
        )
    )
}

# update question element if 'prefer not to say' is selected (remove response and disable)
survey_update = function(input,session){
    survey_ptns_instruments_status = unlist(lapply(grep("survey_ptns",names(input),value=T),function(x){input[[x]]}))
    survey_ptns_instruments = grep("survey_ptns", names(input), value = T)
    survey_instruments = gsub("survey_ptns_","survey_select_",survey_ptns_instruments)

    for(s in seq_along(survey_ptns_instruments_status)){
      if(survey_ptns_instruments_status[s] == "TRUE"){
          disable(survey_instruments[s])
          updateSelectizeInput(session,survey_instruments[s],selected = "")
        } else {
          enable(survey_instruments[s])
        }
    }
    
    if("Prefer not to say" %in% input$survey_radio_health_experience){
      updateCheckboxGroupInput(session,"survey_radio_health_experience",selected = "Prefer not to say")
    }
    
    
}


# input validate - check individual fields and highlgiht as required
# is missing - to be triggered when mouse pover inactive next button
surveyValidator <- function(input, output, session){

    #   radio button fields
        iv_radios <- InputValidator$new()
        iv_radios$add_rule("survey_radio_sex", sv_required())
        iv_radios$add_rule("survey_radio_children", sv_required())
        iv_radios$add_rule("survey_radio_birthcountry", sv_required())
        iv_radios$add_rule("survey_radio_health_experience", sv_required())
        
        #   selectize fields with prefer not to say conditional
        iv_age <- InputValidator$new()
        iv_age$condition(~ input$survey_ptns_age == F)
        iv_age$add_rule("survey_select_age", sv_required())

        iv_partner <- InputValidator$new()
        iv_partner$condition(~ input$survey_ptns_partner == F)
        iv_partner$add_rule("survey_select_partner", sv_required())

        iv_nationality <- InputValidator$new()
        iv_nationality$condition(~ input$survey_ptns_nationality == F)
        iv_nationality$add_rule("survey_select_nationality", sv_required())

        iv_faith <- InputValidator$new()
        iv_faith$condition(~ input$survey_ptns_faith == F)
        iv_faith$add_rule("survey_select_faith", sv_required())

        iv_religious_practice <- InputValidator$new()
        iv_religious_practice$condition(~ input$survey_ptns_religious_practice == F)
        iv_religious_practice$add_rule("survey_select_religious_practice", sv_required())

        iv_education <- InputValidator$new()
        iv_education$condition(~ input$survey_ptns_education == F)
        iv_education$add_rule("survey_select_education", sv_required())

        iv_work <- InputValidator$new()
        iv_work$condition(~ input$survey_ptns_work == F)
        iv_work$add_rule("survey_select_work", sv_required())
        
        iv_income <- InputValidator$new()
        iv_income$condition(~ input$survey_ptns_income == F)
        iv_income$add_rule("survey_select_income", sv_required())

        res_list = list(
            iv_radios,
            iv_age,
            iv_partner,
            iv_nationality,
            iv_faith,
            iv_religious_practice,
            iv_education,
            iv_work,
            iv_income
        )

        return(res_list)
}
  


# check if all survey questions complete?
survey_complete = function(input,...){
    vars_survey = grep("survey_", names(input), value = T)
    vars_survey = lapply(vars_survey,function(x) grep(x, names(input), value = T))
    
    vars_survey_res = lapply(vars_survey, function(x) {
          if(is.logical(input[[x]])){
              input[[x]][1] == T
          } else {
              input[[x]][1] != ""
          }
      })
    
    survey_res = unlist(as.numeric(vars_survey_res))
    is_complete = sum(survey_res,na.rm=T) >= 12
return(is_complete)
}


