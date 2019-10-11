library(tidyverse)
library(ExPanDaR)

for (df_name in c("sub", "tag", "num", "pre")) {
  assign(df_name, read_csv(paste0("data/", df_name, ".csv")))
}

sub %>%
  filter(countryba == "US",
         str_sub(fp, 1, 1) == "Q") %>%
  select(adsh, cik, name, sic, fp, period, fye) %>%
  group_by(cik) %>%
  top_n(1, wt = period) %>%
  left_join(pre) %>%
  filter(stmt == "IS",
         (tag == "Revenues") |
           (tag == "RevenueFromContractWithCustomerExcludingAssessedTax") |
           (tag == "RevenueFromContractWithCustomerIncludingAssessedTax")) %>%
  distinct(adsh, tag, version, .keep_all = TRUE) %>%
  left_join(num) %>%
  filter(ddate == period,
         qtrs == 1L,
         is.na(coreg),
         uom == "USD",
         !is.na(value)) %>%
  mutate(total_revenue = value)  %>%
  select(adsh, cik, name, sic, fp, fye,
         ddate, total_revenue) %>%
  group_by(adsh, cik, name, sic, fp, fye, ddate) %>%
  top_n(1, wt = total_revenue) %>%
  ungroup() %>%
  distinct(cik, name, sic, fp, fye, ddate, .keep_all = TRUE) %>%
  arrange(cik) -> revenue
  
nrow(revenue)

length(unique(revenue$cik)) == nrow(revenue)
table(revenue$ddate)
  
prepare_descriptive_table(revenue %>% select(-ddate, -cik))$df
  
revenue %>% arrange(-total_revenue)
  
#  ggplot(revenue) +
#    geom_density(aes(x = log(1 + total_revenue)),
#                 color = NA, fill = "lightblue") +
#    theme_minimal()
  
#  ggplot(revenue) +
#    geom_histogram(aes(x = log(1 + total_revenue)),
#                   color = NA, fill = "lightblue") +
#    theme_minimal()
