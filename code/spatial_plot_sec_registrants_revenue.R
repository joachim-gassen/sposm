# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# See LICENSE file for details 
#
# SPOSM: Solution to the add-on task spatial plotting of SEC registrants revenue
# ------------------------------------------------------------------------------

library(tidyverse)
library(zipcode)
library(leaflet)
library(widgetframe)
# devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)
data(zipcode)
data(fifty_states)

#--- Prepare data --------------------------------------------------------------

source("code/extract_revenues_sec_fin_stat_data.R")

revenue %>%
  left_join(read_csv("data/sub.csv")) %>%
  mutate(zip = clean.zipcodes(zipba),
         address = str_remove(paste0(paste(bas1, bas2, sep = "\n"),
                                     "\n", cityba, ", ", stprba), "\nNA")) %>%
  filter(str_length(zip) == 5) %>%
  select(cik, name, address, sic, zip, total_revenue) %>%
  inner_join(zipcode) %>%
  unique() -> loc_sec_registrants


#--- State level plot ----------------------------------------------------------

us_states <- 
  read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>%
  mutate(state = tolower(State),
         state_code = Abbreviation) %>%
  select(state, state_code) 

df <- loc_sec_registrants %>%
  mutate(state = ifelse(state == "DC", "VA", state)) %>%
  group_by(state) %>%
  summarise(revenue = sum(total_revenue)/1e9) %>%
  rename(state_code = state) %>%
  left_join(us_states)

ggplot(df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = revenue), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_continuous(trans = 'log10', breaks = c(1, 10, 100)) +
  labs(x = "", y = "", fill  = "Total revenue of SEC registrants in Bil. US-$") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())


#--- Dynamic Zip level plot ----------------------------------------------------

df <- loc_sec_registrants %>%
  mutate(rev_radius = total_revenue/1e6)

map_frame <- leaflet(data = df) %>%
  addTiles(group="OSM") %>%
  addCircles(popup = ~address, label = ~name, radius = ~rev_radius) %>%
  setView(lat = 38, lng = -90, zoom = 4)

frameWidget(map_frame)
