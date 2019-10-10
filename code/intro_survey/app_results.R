library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(tidyverse)
library(grid)
library(gridExtra)
library(kableExtra)
library(ggwordcloud)
library(tm)

source("../utils.R")
source("survey_vars.R")
path_dbase <- "../../data/sposm_survey.sqlite3"

read_response_from_db <- function(path = path_dbase) {
  con <- dbConnect(RSQLite::SQLite(), path)
  res <- dbSendQuery(con, "SELECT * FROM answers")
  df <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  df
}

create_languages_reponse_df <- function (df) {
  rbind(
    df %>%
      select(language_1, usability_1, ease_1) %>%
      rename(language = language_1, usability = usability_1, ease = ease_1),
    df %>%
      select(language_2, usability_2, ease_2) %>%
      rename(language = language_2, usability = usability_2, ease = ease_2),
    df %>%
      select(language_3, usability_3, ease_3) %>%
      rename(language = language_3, usability = usability_3, ease = ease_3)) %>%
    filter(language != "NA") %>%
    mutate(
      usability = ifelse(usability > 0, usability, NA),
      ease = ifelse(ease > 0, ease, NA)
    )
}

table_language <- function(lr_df) {
  lr_df %>%
    group_by(language) %>%
    summarise(
      nobs = n(), 
      mean_usability = mean(usability, na.rm = TRUE),
      mean_ease = mean(ease, na.rm = TRUE)
    ) %>%
    arrange(-nobs)
}

plot_bar_graph <- function(df, var) {
  df[, var] <- factor(df[, var], levels = 1:10)
  ggplot(data = df, aes_string(x = var)) + 
    geom_bar(fill = trr266_yellow) +
    scale_x_discrete(drop = FALSE) +
    labs(x = sprintf("Assessment for %s \n(1: bad, 10: good)", 
                     str_to_title(var))) +
    theme_minimal()
}

plot_bar_graph_language <- function(lr_df, lang_str, var) {
  df <- lr_df %>%
    filter(language == lang_str) %>%
    select(language, !! var)
  
  plot_bar_graph(df, var) + 
    labs(title = sprintf("%s: %s", lang_str, str_to_title(var)))
}

plot_know_terms_graph <- function(df) {
  df %>% select(12:(11 + length(test_items))) %>%
    pivot_longer(everything(), names_to = "term", values_to = "know") %>%
    group_by(term) %>%
    summarise(pct_know = sum(know)/n()) -> df
  
  df$term <- factor(df$term, 
                    levels = tolower(str_replace_all(test_items, "[^[:alnum:]]", "")),
                    labels = test_items)
  
  ggplot(df, aes(x = term, y = pct_know)) + 
    geom_col(fill = trr266_yellow) +
    labs(x = "Term", y = "Share that knows the term") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}


if (Sys.time() > lubridate::ymd_hm("2019-10-10 09:00", tz = "CEST")) {
  ui <-  fluidPage(
    titlePanel("SPOSM Intro Survey: The Findings"),
    h4("This is your response!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        sliderInput("exclude_below_time",
                    "Only observations with response time in seconds larger than...",
                    value = 0,
                    min = 0,
                    max = 60),
        downloadButton("download", "Download the anonymized data")
      ),
      mainPanel(
        h3("Statistical Programming Languages"),
        p("Which languages are you familiar with?"),
        tableOutput("languages_table"),
        hr(),
        h3("The Top 3"),
        p("How do you assess the usability and ease of learning for",
          "the three most commonly named langauges?"),
        plotOutput("bar_plots_languages"),
        hr(),
        h3("Overall competency assessment"),
        p("How competent overall do you assess yourself to be in the area",
          "of statistical programming?"),
        plotOutput("bar_plot_overall"),
        hr(),
        h3("Familiarity with software engineering terms"),
        p("How many of you feel competent to explain the following terms?"),
        plotOutput("know_terms_graph"),
        hr(),
        h3("What do you expect form the seminar?"),
        plotOutput("expectations_wordcloud"),
        hr(),
        h3("And a not-so-serious test"),
        p("Are the ones that feel more confident more likely",
          "to disclose their name?"),
        tableOutput("test_table"),
        hr()
      )
    )
  )  
} else {
  ui <- fluidPage(
    titlePanel("SPOSM Intro Survey: The Findings"),
    h4("You will have to wait until tomorrow. Sorry.")
  )
}


server <- function(input, output, session) {
  raw_df <- read_response_from_db() 
  
  d <- reactive({
    raw_df %>% filter(time >= input$exclude_below_time)    
  })

  output$languages_table <- function() {
    lr_df <- create_languages_reponse_df(d())
    lang_tab <- table_language(lr_df)
    names(lang_tab) <- c("Language", "Times mentioned", 
                         "Mean Usability (1 bad - 10 good)",
                         "Easiness to learn (1 bad - 10 good)")
    knitr::kable(lang_tab, "html", digits = c(0, 0, 2, 2)) %>%
      kable_styling("striped", full_width = F)
  }

  output$bar_plots_languages <- renderPlot({
    lr_df <- create_languages_reponse_df(d()) 
    languages <- table_language(lr_df)$language[1:3]
    p <- vector(mode = "list", 6)
    for (l in 1:3) {
      p[[2*l - 1]] <- plot_bar_graph_language(lr_df, languages[l], "usability")
      p[[2*l]] <- plot_bar_graph_language(lr_df, languages[l], "ease")
    }
    grid.arrange(grobs = p, nrow = 3, ncol = 2)
  })
  
  output$bar_plot_overall <- renderPlot({
    df <- d()
    plot_bar_graph(df, "overall")
  })
  
  output$know_terms_graph <- renderPlot({
    df <- d()
    plot_know_terms_graph(df)
  })
  
  output$expectations_wordcloud <- renderPlot({
    expectations <- 
      VCorpus(VectorSource(d()$learning_objectives)) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    
    dtm <- TermDocumentMatrix(expectations, 
                              control = list(wordLengths=c(1, Inf)))
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    df <- data.frame(word = names(v),freq=v)
    ggplot(df, aes(color = factor(sample.int(5, nrow(df), replace = TRUE)))) + 
      geom_text_wordcloud(aes(label = word, size = freq)) +
      scale_size_area(max_size = 12) +
      scale_color_manual(values = c("black", trr266_blue,
                                    trr266_petrol, trr266_yellow,
                                    trr266_red)) +
      theme_minimal()
  })
  
  output$test_table <- function() {
    df <- d() 
    df$provided_name <- df$name != ""
    if (length(df$overall[df$provided_name]) > 1 &
        length(df$overall[!df$provided_name]) > 1) {
      tt <- t.test(overall ~ provided_name, data = df)
      rt <- wilcox.test(overall ~ provided_name, data = df)
      
      print_df <- rbind(c(sum(df$provided_name)/nrow(df), NA),
                        c(tt$statistic, tt$p.value),
                        c(rt$statistic, rt$p.value))
      
      colnames(print_df) <- c("Statistic", "P-value (two sided)")
      rownames(print_df) <- c("Percentage that disclosed name",
                              "T-test for mean differences",
                              "Wilcoxon test for distribution differences")
      
      options(knitr.kable.NA = '')
      knitr::kable(print_df, "html", digits = c(2, 4)) %>%
        kable_styling("striped", full_width = FALSE)
      
    } else {
      print_df <- data.frame(sum(df$provided_name)/nrow(df))
      
      colnames(print_df) <- "Statistic"
      rownames(print_df) <- "Percentage that disclosed name"
      
      options(knitr.kable.NA = '')
      knitr::kable(print_df, "html", digits = c(2)) %>%
        kable_styling("striped", full_width = FALSE)     
    }
  }
  
  output$download <- downloadHandler(
    filename <- function() {
      paste0('sposm_intro_survey_data-', Sys.Date(), '.csv')
    },
    content <- function(con) {
      write.csv(raw_df %>% 
                  mutate(name_provided = name != "") %>% 
                  select(-name), con)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

