library(tidyverse)

init_data <- function() {
  for(df_name in c("sub", "tag", "num", "pre")) {
    assign(df_name, read_csv(paste0("data/", df_name, ".csv")))
  }
  num %>%
    select(adsh, tag, version, ddate, qtrs, uom, value)
  sub %>%
    mutate(cik = sprintf("%d", as.integer(cik))) %>%
    select(adsh, cik, name, sic, countryba)
  tag %>%
    select(tag, version, datatype, tlabel)
  pre %>%
    select(adsh, report, line, stmt, tag, version, plabel)
  list(num = num, pre = pre, sub = sub, tag = tag)
}

lookup_firm <- function(cik, dl) {
  firm_data <- dl$sub %>% 
    filter(cik == !! cik) %>%
    distinct(cik, name, sic)
  if (nrow(firm_data) == 0) 
    stop(sprintf("Firm with CIK %s not found in SEC data", cik))
  if (nrow(firm_data) > 1)
    stop(sprintf("Firm with CIK %s has inconsistent double entries in SEC data", cik))
  message(
    sprintf("Firm %s (CIK %s) found in SEC data. Belongs to industry %s",
            firm_data$name, cik, firm_data$sic)
  )
  list(
    cik = cik,
    name = as.vector(firm_data$name),
    sic = as.vector(firm_data$sic)
  )
}

get_fin_stmt <- function(cik, dl, stmt) {
  stmt_data <- dl$sub %>%
    filter(cik == !! cik,
           fp == "Q1") %>%
    left_join(dl$pre %>% filter(stmt == !! stmt), by = "adsh") %>%
    left_join(dl$num, by = c("adsh", "tag", "version")) %>%
    filter(is.na(qtrs) | qtrs <= 1) %>%
    group_by(cik) %>%
    filter(is.na(ddate) | ddate == max(ddate, na.rm = TRUE)) %>%
    ungroup() %>%
    select(report, line, plabel, ddate, qtrs, uom, value) %>%
    arrange(line)
}

dl <- init_data()
firm_data <- lookup_firm("12927", dl)
stmt <- get_fin_stmt("12927", dl, "BS")

