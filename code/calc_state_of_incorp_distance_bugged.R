library(tidyverse)
library(geosphere)
library(zipcode)

# This code should calculate the distance between the current business address
# of each U.S. based SEC registrant (identified by its CIK) and its state of 
# incorporation (if in the U.S.). The idea is to get the average distance by
# state of incorporation to assess whether some states indeed have a founding
# infrastructure (e.g., corporate law) that is attractive to out of state 
# corporations.

# Unfortunately, the code does not run.

df <- read_csv("data/sub.csv")

us_states <- 
  read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>%
  rename(state = Abbreviation, state_name = State)

us_area_code_city_loc <- 
  read_csv("https://raw.githubusercontent.com/ravisorg/Area-Code-Geolocation-Database/master/us-area-code-cities.csv", 
           col_names = c("phone_area_code", "city", "state_name", "cpuntry_name", "lat", "long")) 

data(zipcode)

state_loc <- us_area_code_city_loc %>%
  left_join(us_states) %>%
  select(state, state_name, lat, long) %>%
  group_by(state) %>%
  summarise(lat = mean(lat),
            long = mean(long))

df %>%
  filter(countryinc == "US") %>%
  filter(countryba == "US") %>%
  filter(!is.na(stprinc)) %>%
  rename(state_inc = stprinc) %>%
  mutate(zip = clean.zipcodes(zipba)) %>%
  select(cik, name, sic, zip, state_inc) %>%
  distinct(cik, .keep_all = TRUE) %>%
  left_join(state_loc, by = c(state_inc = "state"))  %>%
  rename(
    lat_inc = lat,
    long_inc = long
  ) %>%
  left_join(zipcode) %>%
  rename(
    state_ba = state,
    lat_ba = latitude,
    long_ba = longitude
  ) %>%
  select(cik, name, sic, ends_with("_inc"), ends_with("_ba"))  %>%
  filter(!is.na(long_ba)) %>%
  mutate(dist_inc_ba = distGeo(cbind(long_inc, lat_inc), 
                             cbind(long_ba, lat_ba))/1000) %>%
  group_by(state_inc) %>%
  summarise(
    nobs = n(),
    mean_dist_inc_ba = mean(dist_inc_ba),
    median_dist_inc_ba = median(dist_inc_ba)
  ) %>%
  left_join(us_states, by = c("state_inc" = "state")) %>%
  select(state_inc, state_name, nobs, 
         mean_dist_inc_ba, median_dist_inc_ba) %>%
  filter(nobs >= 30) %>%
  arrange(-mean_dist_inc_ba)
