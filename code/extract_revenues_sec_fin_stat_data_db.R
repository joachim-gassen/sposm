library(tidyverse)
library(ExPanDaR)
library(DBI)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/sec.sqlite3")

tbl(con, "sub") %>% 
  filter(countryba == "US", 
         str_sub(fp, 1, 1) == 'Q') %>%
  select(cik, period) %>%
  group_by(cik) %>%
  summarise(period = max(period, na.rm = TRUE)) %>%
  mutate(lqr = TRUE) -> last_quarterly_report

tbl(con, "sub") %>% 
  filter(countryba == "US", 
         str_sub(fp, 1, 1) == 'Q') %>%
  left_join(last_quarterly_report) %>%
  filter(lqr) %>%
  select(adsh, cik, name, sic, fp, period, fye) -> firm_level_data
  
tbl(con, "pre") %>%
  filter(stmt == "IS",
         (tag == "Revenues") | 
           (tag == "RevenueFromContractWithCustomerExcludingAssessedTax") |
           (tag == "RevenueFromContractWithCustomerIncludingAssessedTax")) %>%
  inner_join(firm_level_data) %>%
  left_join(tbl(con, "num")) %>%
  filter(ddate == period, 
         qtrs == 1L,
         is.null(coreg),
         uom == "USD",
         !is.na(value)) %>%
  mutate(total_revenue = value)  %>% 
  select(adsh, cik, name, sic, fp, fye, 
         ddate, total_revenue) -> query_result 
    
revenue <- collect(query_result) %>% 
  group_by(adsh, cik, name, sic, fp, fye, ddate) %>%
  top_n(1, wt = total_revenue) %>%
  ungroup() %>%
  distinct(cik, name, sic, fp, fye, ddate, .keep_all = TRUE) %>%
  arrange(cik)

nrow(revenue)

dbDisconnect(con)
