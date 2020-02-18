library(tidyverse)

source("code/btag_open_data_scrape_data.R")
load("data/btag_open_data_19wp_reden.RData")

file <- "data/btag_open_data_xml/19001-data.xml"
pp_xml <- read_xml(file)
reden_xml <- xml_find_all(pp_xml, ".//rede")
reden_list <- as_list(reden_xml)
pp_as_list <- as_list(pp_xml)
reden_texte %>%
  left_join(redner, by = c("wahlperiode", "sitzungs_nr", "rede_pos")) %>%
  left_join(sitzungen, by = c("wahlperiode", "sitzungs_nr")) -> merged_data

unique_redner <- redner %>%
  select(id, vorname, nachname) %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(nachname, vorname)

merged_data %>%
  filter(!kommentar) %>%
  mutate(wc = str_count(text, '\\w+')) %>% 
  group_by(id) %>% 
  summarise(spoken_words = sum(wc)) %>%
  left_join(unique_redner) %>%
  arrange(-spoken_words) 

merged_data %>%
  group_by(id, sitzungs_nr, rede_pos) %>% 
  summarise(n_kommentar = sum(kommentar),
            spoken_words = sum(str_count(text, '\\w+'))) %>%
  group_by(id) %>%
  filter(sum(spoken_words) > 10000) %>%
  summarise(kommentar_words = mean(1000*(n_kommentar/spoken_words))) %>%
  left_join(unique_redner) %>%
  arrange(-kommentar_words) 

merged_data %>% filter(kommentar) -> kommentare
kommentare$beifall_str <- str_extract(kommentare$text, "(?<=\\()Beifall bei(.*?)(?=\\))")
kommentare$beifall_str <- str_split_fixed(kommentare$beifall_str, " – ", 3)[, 1]

kommentare %>% filter(beifall_str != "") %>%
  mutate(
    beifall_linke = str_detect(beifall_str, fixed("LINKE")),
    beifall_gruene = str_detect(beifall_str, fixed("GRÜNEN")),
    beifall_spd = str_detect(beifall_str, fixed("SPD")),
    beifall_fdp = str_detect(beifall_str, fixed("FDP")),
    beifall_cducsu = str_detect(beifall_str, fixed("CDU/CSU")),
    beifall_afd = str_detect(beifall_str, fixed("AfD"))
  ) %>%
  select(sitzungs_nr, datum, rede_pos, pos, id, vorname, nachname, starts_with("beifall")) %>%
  group_by(sitzungs_nr, rede_pos) %>%
  filter(pos != max(pos)) -> beifall

library(ExPanDaR)
df <- as.data.frame(lapply(beifall[, 9:14], as.numeric))
prepare_correlation_graph(df)

