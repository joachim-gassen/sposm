library(tidyverse)
library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(shinyjs, quietly = TRUE)

source("survey_vars.R")
path_dbase <- "../../data/sposm_survey.sqlite3"

DEBUG <- TRUE

disableActionButton <- function(id, session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#", id, "').prop('disabled',true)"
                                             , sep="")))
}

# See https://calligross.de/post/using-cookie-based-authentication-with-shiny/
# for cookie implementation
jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }

  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie", params);
  }

  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

ui <- fluidPage(
  tags$head(
    tags$script(src = "js.cookies.js")
  ),
  useShinyjs(), 
  extendShinyjs(text = jsCode),
  titlePanel("SPOSM: Intro Survey"),
  uiOutput("greeting"),
  uiOutput("language_selection_block"),
  uiOutput("language_feedback_block"),
  uiOutput("overall_block"),
  uiOutput("test_items_block"),
  uiOutput("objectives_block"),
  uiOutput("name_block")
)


server <- function(input, output, session) {
  time_submitted <- NA
  time_in <- Sys.time()
  submitted <- reactiveVal(FALSE)
  wrong_selection <- reactiveVal(FALSE)
  
  has_participated <- reactiveVal(
    Sys.time() < lubridate::ymd_hm("2019-10-09 09:00", tz = "CEST")
  )
  
  response <- reactive({
    ret_list <- list(
      input$languages[1], input$languages[2], input$languages[3],
      as.integer(input$usability_1), 
      ifelse(is.null(input$usability_2), 0, as.integer(input$usability_2)), 
      ifelse(is.null(input$usability_3), 0, as.integer(input$usability_3)),
      ifelse(is.null(input$ease_1), 0, as.integer(input$ease_1)), 
      ifelse(is.null(input$ease_2), 0, as.integer(input$ease_2)), 
      ifelse(is.null(input$ease_3), 0, as.integer(input$ease_3)),  
      as.integer(input$overall)
    )
    for (i in 1:length(test_items)) {
      ret_list <- c(
        ret_list,
        as.integer(test_items[i] %in% input$test_items)
      )
    }
    ret_list <- c(
      ret_list,
      input$learning_objectives,
      input$name
    )
    ret_list
  })
  
  # check if a user already has participated cookie is present  
  observe({
    js$getcookie()
    if (!is.null(input$jscookie) && 
        input$jscookie == "HAS_PARTICIPATED_IN_SPOSM_INTRO_SURVEY" &&
        !DEBUG) {
      has_participated(TRUE)
    }
  })

  store_user_response <- function(response) {
    con <- dbConnect(RSQLite::SQLite(), path_dbase)
    var_str <- paste0(paste0('?', names(vars), ', '), collapse = "")
    var_str <- str_sub(var_str, 1, str_length(var_str) - 2)
    
    response <- c(
      as.integer(difftime(Sys.time(), time_in, units = "secs")),
      response
    )
    names(response) <- names(vars)
    
    query <- sqlInterpolate(con, 
      paste0("INSERT INTO answers VALUES (", var_str, ")"), .dots = response)
    res <- dbSendQuery(con, query)
    dbClearResult(res)
    dbDisconnect(con)   
  } 
  
  observeEvent(input$submit, {
    store_user_response(response())
    disable("submit")
    submitted(TRUE)
    has_participated(TRUE)    
    js$setcookie("HAS_PARTICIPATED_IN_SPOSM_INTRO_SURVEY") 
  })
  
  output$greeting <- renderUI(
    if(!has_participated()) {
      fluidRow(
        column(
          12, 
          p("Welcome! This little survey serves the purpose to collect",
            "some information about your background and your expectations",
            "with regards to the course."),
          p("No worries. Your responses will not be graded ;-)"),
          p("Thank you for participating!"),
          br()
        )
      )
    } else {
      fluidRow(
        column(12, align = "center",
               p(),
               p("Thank you! you can now close this window."),
               p()
        ))      
    }
  )

  observeEvent(input$languages, {
    if (length(input$languages) == 3) {
      disable("languages")
    } 
  })
  
  # --- User Interface --------------------------------------------------------
  
  language_feedback <- reactive({
    req(input$languages)
    languages <- input$languages
    output_list <- NULL
    if (length(languages) > 0) {
      output_list <- vector("list", length(languages) + 1)
      for (i in 1:length(languages)) {
        output_list[[i]] <-
          fluidRow(
            column(
              6,
              selectizeInput(
                paste0("usability_", i),
                label = sprintf(
                  paste0(
                    "How do you assess the usability of %s? (on a scale ", 
                    "from 1 - horrible to 10 - wonderful)"
                  ), languages[i]
                ),
                choices = 1:10,
                options = list(
                  placeholder = "",
                  onInitialize = I('function() { this.setValue(""); }')
                ),
                width = "100%"
              )
            ),
            column(
              6,
              selectizeInput(
                paste0("ease_", i),
                label = sprintf(
                  paste0(
                    "How easy is %s to learn? (on a scale ", 
                    "from 1 - a nightmare to 10 - a total breaze)"
                  ), languages[i] 
                ),
                choices = 1:10, 
                options = list(
                  placeholder = "",
                  onInitialize = I('function() { this.setValue(""); }')
                ),
                width = "100%"
              )
            )
          )
      }
    }
    output_list[[length(languages) + 1]] <- hr()
    output_list
   })
  
  
  output$language_selection_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12, 
          checkboxGroupInput(
            "languages", 
            label = paste(
              "Please select up to three statistical programming languages",
              "that you are reasonable familiar with."
            ), 
            choices = c(
              "EViews", "Gretl", "Julia", "Matlab", "Python", "R", "SAS", 
              "SPSS", "Stata"
            ),
            inline = TRUE,
            width = "100%"
          ),
          hr()
        )
      )
    } 
  )
  
  output$language_feedback_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12,  
          if (length(input$languages) > 0) language_feedback()
          else {
            fluidRow(
              column(
                12, 
                p("Please select some languages from the list above."),
                hr()
              )
            )
          }
        )
      )
    }
  )
  
  output$overall_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12,
          selectizeInput(
            "overall",
            label = paste(
                "Overall, how competent do you feel you are in terms of",
                "statistical programming? (on a scale", 
                "from 1 - not at all competent to 10 - extremely competent)"
            ),
            choices = 1:10, 
            options = list(
              placeholder = "",
              onInitialize = I('function() { this.setValue(""); }')
            ),
            width = "100%"
          ), 
          hr()
        )
      )
    }
  )
  
  output$test_items_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12,
          checkboxGroupInput(
            "test_items", 
            label = paste(
              "Please select all terms that you are reasonably", 
              "familiar with, meaning that you feel competent to explain others",
              "what they mean"
            ), 
            choices = test_items, 
            inline = TRUE,
            width = "100%"
          ),
          hr()
        )
      )
    }
  )
  
  output$objectives_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12,
          textInput(
            "learning_objectives",
            "What do you expect to learn during this course?",
            width = "100%"
          ),
          hr()
        )
      )
    }
  )
  
  output$name_block <- renderUI(
    if (!has_participated()) {
      fluidRow(
        column(
          12,
          h4("Thank you! One last thing:"),
          textInput(
            "name",
            paste(
              "If you want, you can share your name in the text field below.", 
              "While this entirely voluntary and of course your identity will",  
              "be held confidential, this would help me to construct teams for", 
              "assignments, etc."
            ),
            width = "100%"
          ),
          hr(),
          actionButton("submit", "I am done!", align = "center"),
          hr()
        )
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

