library(tidyverse)
library(DBI)
library(lubridate)

source("code/intro_survey/survey_vars.R")

path_dbase <- "data/sposm_survey.sqlite3"

# Create empty data base
con <- dbConnect(RSQLite::SQLite(), path_dbase)

dbCreateTable(con, "answers", vars)
dbDisconnect(con)

