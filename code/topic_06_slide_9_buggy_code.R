library(tidyverse)

read_csv("raw_data/stock_price.csv") %>%
  mutate(return = (adj_prc - lag(adj_prc))/lag(adj_prc)) %>% 
  summarise(mn_return = mean(return, na.rm = TRUE),
            var_return = sd(return, na.rm = TRUE)^2)
