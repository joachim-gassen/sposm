library(tidyverse)
library(tidyquant)
library(BatchGetSymbols)
library(lubridate)

dj30 <- tq_index("DOW") %>% rename(ticker = symbol)

dta <- BatchGetSymbols(tickers = dj30$ticker,
                      first.date = ymd("1999-01-01"),
                      last.date = ymd("2019-11-22"))


dta$df.tickers %>%
  group_by(ref.date) %>%
  summarise(mreturn = mean(ret.adjusted.prices)) %>%
  rename(date = ref.date) %>%
  select(date, mreturn) %>%
  filter(date > ymd("1999-01-01")) -> dj30_mreturn

dta$df.tickers %>%
  rename(date = ref.date,
         return = ret.adjusted.prices) %>%
  select(ticker, date, return) %>%
  filter(date > ymd("1999-01-01")) -> dj30_freturn


reg_results <- function(x) {
  df <- as.data.frame(x)
  fm <- lm(mreturn ~ return, data = df)
  beta <- coef(fm)[[2]]
  ci <- confint(fm)[2, ]
  c(beta = beta, ci_lb = ci[[1]], ci_ub = ci[[2]])
}

roll <- function(x) rollapplyr(x, 250, reg_results, by.column = FALSE, fill = NA)

dj30_freturn %>%
  left_join(dj30_mreturn, by = "date") %>%
  group_by(ticker) %>%
  do(data.frame(select(., mreturn, return) %>% roll)) %>%
  bind_cols(date = dj30_freturn$date) %>%
  ungroup() %>%
  filter(!is.na(beta)) %>%
  select(ticker, date, beta, ci_lb, ci_ub) %>%
  filter(date >= ymd("2000-01-01")) -> betas

save(betas, dj30, file = "code/betas/betas.Rdata")

betas %>%
  group_by(date) %>%
  summarise(mean_beta = mean(beta),
            min_ci_lb = min(ci_lb),
            max_ci_ub = max(ci_ub)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y=mean_beta), colour="blue") + 
  geom_ribbon(aes(ymin=min_ci_lb, ymax=max_ci_ub), alpha=0.2) + 
  labs(y = "Beta factor of 250-day CAPM regression ending on date",
       caption = paste("The blue lines indicates the average beta factor over",
                       "all 28 DJ 30 firms with full price data.\nThe gray",
                       "area indicates the lowest and highest firm-level",
                       "confidence interval for the given date.")) + 
  theme_minimal()

ggsave("output/assigment_3.png")

