library(tidyverse)

#* @apiTitle Tidy Bundestag
#* @apiDescription Access the speaches made during the 19th Wahlperiode 
#*   of the Deutsche Bundestag. Data is as provided by 
#*   https://www.bundestag.de/services/opendata. 
#* @apiVersion 0.0.1.9000

source("code/btag_open_data_scrape_data.R")
load("data/btag_open_data_19wp_reden.RData")

reden_texte %>%
  left_join(redner, by = c("wahlperiode", "sitzungs_nr", "rede_pos")) %>%
  left_join(sitzungen, by = c("wahlperiode", "sitzungs_nr")) %>%
  arrange(sitzungs_nr, rede_pos, pos) -> merged_data

#' Provide speach data by Sitzung and Rede
#' @param sitzungs_nr The number of the Sitzung as specified by the Deutsche Bundestag
#' @param rede_pos The speach that you want (speaches are numbered consecutively by Sitzung)
#' @get /rede_data_by_sitzung_and_rede_pos
#' @json
rede_data_by_sitzung_and_rede_pos <- function(sitzungs_nr, rede_pos) {
  merged_data %>%
    filter(sitzungs_nr == !!sitzungs_nr,
           rede_pos == !!rede_pos)
}


#' Provide speach data by Sitzung
#' @param sitzungs_nr The number of the Sitzung as specified by the Deutsche Bundestag
#' @get /rede_data_by_sitzung
#' @json
rede_data_by_sitzung <- function(sitzungs_nr) {
  merged_data %>%
    filter(sitzungs_nr == !!sitzungs_nr)
}


#' Provide speach data by speaker ID
#' @param id The speaker ID as specified by the Deutsche Bundestag
#' @get /rede_data_by_id
#' @json
rede_data_by_id <- function(id) {
  merged_data %>%
    filter(id == !!id)
}


#' Provide speach text by Sitzung and Rede
#' @param sitzungs_nr The number of the Sitzung as specified by the Deutsche Bundestag
#' @param rede_pos The speach that you want (speaches are numbered consecutively by Sitzung)
#' @get /rede_text_by_sitzung_and_rede_pos
#' @json
rede_text_by_sitzung_and_rede_pos <- function(sitzungs_nr, rede_pos) {
  merged_data %>%
    filter(sitzungs_nr == !!sitzungs_nr,
           rede_pos == !!rede_pos) %>% 
    mutate(text = paste(text, collapse = " ")) %>%
    select(-pos, -kommentar) %>%
    distinct()
}


#' Provide speach texts by Sitzung
#' @param sitzungs_nr The number of the Sitzung as specified by the Deutsche Bundestag
#' @get /rede_text_by_sitzung
#' @json
rede_text_by_sitzung <- function(sitzungs_nr) {
  merged_data %>%
    filter(sitzungs_nr == !!sitzungs_nr) %>% 
    group_by(rede_pos) %>%
    mutate(text = paste(text, collapse = " ")) %>%
    select(-pos, -kommentar) %>%
    ungroup() %>%
    distinct() %>%
    arrange(rede_pos)
}


#' Provide speach texts by speaker id
#' @param id The speaker ID as specified by the Deutsche Bundestag
#' @get /rede_text_by_id
#' @json
rede_text_by_id <- function(id) {
  merged_data %>%
    filter(id == !!id) %>% 
    group_by(sitzungs_nr, rede_pos) %>%
    mutate(text = paste(text, collapse = " ")) %>%
    select(-pos, -kommentar) %>%
    ungroup() %>%
    distinct() %>%
    arrange(sitzungs_nr, rede_pos)
}

# pr <- plumber::plumb("code/btag_open_data_plumber.R")
# pr$run()
