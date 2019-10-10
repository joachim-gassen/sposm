library(tidyverse)
library(DBI)
library(scales)

read_response_from_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), path_dbase)
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
  
  plot_bar_graph(df, var)
}

plot_know_terms_graph <- function(df) {
  df <- raw_response
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
    scale_y_continuous(labels = percent) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

