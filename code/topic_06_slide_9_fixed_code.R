library(tidyverse)

df <- read_csv("raw_data/stock_price.csv") 

date_list <- tibble(date = seq(min(df$date), max(df$date), by = 'weeks'))

df_full <- date_list %>% left_join(df, by = "date")

df_full %>%
  mutate(return = (adj_prc - lag(adj_prc))/lag(adj_prc)) %>%
  summarise(mn_return = mean(return, na.rm = TRUE),
            var_return = sd(return, na.rm = TRUE)^2)


# --- Code for data simulation -------------------------------------------------

library(lubridate)
set.seed(266)

# Set to TRUE if you want to regenerate "data"
rewrite <- FALSE

time_series <- rep(NA, 1000)

time_series[1] <- 100

for (i in 2:1000) {
  time_series[i] <- rnorm(1, 1.0005, 0.005) * time_series[i - 1]
}

df <- data.frame(
  date = ymd("2000-07-21") + weeks(1:1000), 
  adj_prc = round(time_series, 2) 
)

df$adj_prc[sample(1000, 50)] <- NA

df <- df %>% na.omit() 
rownames(df) <- NULL

ggplot(data = df) + 
  geom_line(aes(x = date, y = adj_prc)) +
  theme_minimal()

ggplot(data = df) + 
  geom_line(aes(x = date, y = adj_prc)) +
  scale_y_continuous(trans='log10') +
  theme_minimal()

if (rewrite) write_csv(df, "raw_data/stock_price.csv")
