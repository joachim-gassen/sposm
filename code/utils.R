library(tidyverse)
library(lubridate)
library(datamodelr)

# The time table of the SPOSM 19 course

sposm19_time_table <- tibble(
  dtime = c(ymd_hm("2019-10-10 9:00"), ymd_hm("2019-10-10 9:30"),
            ymd_hm("2019-10-10 10:30"), ymd_hm("2019-10-10 11:00"),
            ymd_hm("2019-10-10 12:30"), ymd_hm("2019-10-10 14:00"),
            ymd_hm("2019-10-10 15:30"), ymd_hm("2019-10-10 16:00"),
            ymd_hm("2019-10-10 19:30"),
            ymd_hm("2019-10-11 9:00"), ymd_hm("2019-10-11 10:30"),
            ymd_hm("2019-10-11 11:00"), ymd_hm("2019-10-11 12:30"),
            ymd_hm("2019-10-11 13:30"), ymd_hm("2019-10-11 14:30"),
            ymd_hm("2019-10-11 15:30"), ymd_hm("2019-10-11 16:00"),
            ymd_hm("2020-02-17 10:00"), ymd_hm("2020-02-17 10:30"),
            ymd_hm("2020-02-17 12:00"), ymd_hm("2020-02-17 13:30"),
            ymd_hm("2020-02-17 15:00"), ymd_hm("2020-02-17 15:30"),
            ymd_hm("2020-02-17 16:30"),
            ymd_hm("2020-02-18 9:00"), ymd_hm("2020-02-18 10:30"),
            ymd_hm("2020-02-18 11:00"), ymd_hm("2020-02-18 12:30"),
            ymd_hm("2020-02-18 13:30"), ymd_hm("2020-02-18 15:00"),
            ymd_hm("2020-02-18 15:30")),
  title = c("Welcome and Introduction", 
            "The development environment and project organization",
            "Coffee", "Using Git and Github", "Lunch",
            "Statistical programming languages: An overview", "Coffee",
            "Functional versus object-oriented programming",
            "Pizza at Due Forni, Schönhauser Allee 12",
            "Writing readable and reusable code", "Coffee",
            "Debugging tools", "Lunch and coffee",
            "Relational databases and the concept of normalized data",
            "Data wrangling and visualization fundamentals",
            "Assignments and wrap up", "End of event",
            "Welcome and Coffee", "Tidy data scraping", "Lunch", 
            "Code along: Unit testing in function development", "Coffee",
            "Group work presentations", "End of Day",
            "Explore your researcher degrees of freedom", "Coffee", 
            "Providing data access via RESTful APIs", "Lunch",
            "Group Work Presentations", "Coffee and Wrap Up", "End of Event")
)

# 'Corporate' colors used by our TRR 266 Accountinig for Transparency project
# See http://www.accounting-for-transparency.de for more info

lighten <- function(color, factor = 0.5) {
  if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col)*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

trr266_petrol <- rgb(27, 138, 143, 255, maxColorValue = 255)
trr266_blue <- rgb(110, 202, 226, 255, maxColorValue = 255)
trr266_yellow <- rgb(255, 180, 59, 255, maxColorValue = 255)
trr266_red <- rgb(148, 70, 100, 255, maxColorValue = 255)
trr266_lightpetrol <- lighten(trr266_petrol, 0.5)

#' This function tries to identify straightforward keys
#' in `datamodelr` data model objects. It is half-baked
#' an essentially untested.
#' 
#' To identify keys and references the code relies on the each 
#' table having either
#'    - a single primary key with a name that can be derived 
#'      from its table name by the provided function 
#'      pk_function() or
#'    - a set of foreign primary keys that constitutes the pimary key
#'    
#' It then generates references from each foreign key to 
#' its according primary key instance at the relational table      

dm_auto_create_keys_and_references <- 
  function(dm, id = "_id",
           pk_function = function(table) {paste0(table, id)}) {
  pot_pkeys <- unique(unname(unlist(sapply(dm$tables$table, pk_function))))
  for (i in 1:nrow(dm$tables)) {
    tab <- dm$table[i, ]
    pkeys_in_table <- pk_function(tab$table) %in% 
      dm$columns$column[dm$columns$table == tab$table]
    if (any(pkeys_in_table)) {
      for (key in pk_function(tab$table)[pkeys_in_table]) {
        dm <- dm_set_key(dm, tab$table, key)
      }
    } else {
      foreign_keys_in_table <- pot_pkeys[pot_pkeys %in% 
                                           dm$columns$column[dm$columns$table == tab$table]]
      for (key in foreign_keys_in_table) {
        dm <- dm_set_key(dm, tab$table, key)
      }
    }
  }
  
  for (i in 1:nrow(dm$columns)) {
    col <- dm$columns[i, ]
    if (col$column %in% pot_pkeys & 
        !any(col$column %in% pk_function(col$table))) {
      ref_to <- unique(dm$columns$table[pk_function(dm$columns$table) == col$column])
      dm <- dm_add_reference_(dm, col$table, col$column, ref_to, col$column)
    } 
  }
  dm
}


# Some functions that process meta data frames of the `ExPanDaR` package
# to assign variables 

assign_vars <- function(var_name, definition) {
  assignments <- paste0(var_name, " = ", definition, ",")
  assignments[length(assignments)] <- substr(assignments[length(assignments)], 1,
                                             nchar(assignments[length(assignments)])-1)
  return(assignments)
}

calc_variables <- function(df, var_name, definition, type, can_be_na) {
  cs_id <- definition[type == "cs_id"]
  ts_id <- definition[type == "ts_id"]
  
  code <- c("df %>% arrange(",
            paste(c(cs_id, ts_id), collapse=", "),
            ") %>%")
  
  vars_to_assign <- which(var_name %in% cs_id)
  code <- c(code, "mutate(",
            assign_vars(var_name[vars_to_assign], definition[vars_to_assign]),
            ") %>% ")
  
  code <- c(code,"group_by(",
            paste(cs_id, collapse=", "),
            ") %>%")
  
  vars_to_assign <- which(!var_name %in% cs_id)
  code <- c(code, "transmute(",
            assign_vars(var_name[vars_to_assign], definition[vars_to_assign]),
            ") %>%")
  code <- c(code, "drop_na(",
            paste(var_name[can_be_na != 1], collapse = ","),
            ") -> ret ")
  
  eval(parse(text = code))
  return(as.data.frame(ret))
}