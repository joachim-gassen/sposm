library(tidyverse)
library(rvest)
library(lubridate)
library(ExPanDaR)

# --- Scrape Raw Data ----------------------------------------------------------

url_sp500_const <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

url_sp500_const %>%
  read_html() %>%
  html_node(xpath = '//*[@id="constituents"]') %>%
  html_table() -> sp500_constituents_raw


url_sp500_const %>%
  read_html() %>%
  html_node(xpath = '//*[@id="constituents"]') %>%
  html_nodes("td:nth-child(2) a") %>% 
  html_attr("href")-> links

# This was ill-linking to the normal corporate webpage
# links[52] <- "wiki/Aptiv" - fixed it on Wikipedia instead

links <- paste0("https://en.wikipedia.org", links)

get_wiki_infobox <- function(url) {
  url <- links[2]
  xml_data <- read_html(url) %>%
    html_node('#mw-content-text div table.infobox.vcard')
  
  # To reflect page wraps in text caused by tags
  xml_find_all(xml_data, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(xml_data, ".//li") %>% xml_add_sibling("p", "\n")
  
  if (class(xml_data) != "xml_missing") {
    xml_data  %>%
      html_table(fill = TRUE) %>%
      rename(tag = X1, content = X2) %>%
      filter(tag != "")
  } else tibble(tag = "xml_missing", content = "TRUE")
}

get_wiki_page_info <- function(url) {
  page_info_html <- paste0(url, "?action=info") %>%
    read_html() 
  
  page_info_html %>%
    html_node('#mw-content-text table:nth-child(6)') %>%
    html_table(fill = TRUE) -> df
  
  redirect <- df$X2[df$X1 == "Redirects to"]
  if(length(redirect) != 0) {
    page_info_html %>%
      html_node('#mw-pageinfo-redirectsto td:nth-child(2) a')%>% 
      html_attr("href") -> path
    
    page_info_html <-  
      paste0("https://en.wikipedia.org", path, "?action=info") %>%
      read_html()  
    
    page_info_html %>%
      html_node('#mw-content-text table:nth-child(6)') %>%
      html_table(fill = TRUE) -> df
  }
  
  df %>%
    bind_rows(
      page_info_html %>%
        html_node('#mw-content-text table:nth-child(11)') %>%
        html_table(fill = TRUE)    
    ) %>%
    rename(tag = X1, content = X2) %>%
    filter(tag != "")
}

sp500_wiki_info_box_raw <- lapply(links, get_wiki_infobox) %>%
  bind_rows(.id = "tab_pos") %>%
  pivot_wider(id_cols = tab_pos, names_from = tag, values_from = content)

sp500_wiki_page_info_raw <- lapply(links, get_wiki_page_info) %>%
  bind_rows(.id = "tab_pos") %>%
  pivot_wider(id_cols = tab_pos, names_from = tag, values_from = content)

save(
  sp500_constituents_raw,
  sp500_wiki_info_box_raw,
  sp500_wiki_page_info_raw, 
  file = "data/sp500_wiki_raw.Rdata"
)

# --- Load and tidy raw data ---------------------------------------------------

load("data/sp500_wiki_raw.Rdata")

get_min_4digits_from_str <- function(raw_str) {
  fyrs_list <- str_extract_all(raw_str, "\\d{4}")
  sapply(fyrs_list, 
         function (x) ifelse(
           length(x) > 0,
           min(as.numeric(unlist(x))),
           NA
         )
  )
}

sp500_constituents <- sp500_constituents_raw %>%
  rename(
    ticker = Symbol,
    name = Security,
    gics_sector = `GICS Sector`,
    gics_sub_sector = `GICS Sub Industry`,
    hquarter = `Headquarters Location`,
    date_first_added = `Date first added`,
    cik = CIK
  ) %>% mutate(
    year_first_founded = get_min_4digits_from_str(Founded) 
  ) %>% 
  select(ticker, cik, name, gics_sector, gics_sub_sector, hquarter, 
         date_first_added, year_first_founded)

# Check whether 'ticker' is unique (it is)

any(duplicated(sp500_constituents$ticker)) # FALSE


# Check for duplicated gic codes (there are five)

which(duplicated(sp500_constituents$cik)) 

# We have five companies with two securities each included

# Extract data from 'Traded as' field

mat <- str_split_fixed(sp500_wiki_info_box_raw$`Traded as`, fixed("\n"), n = 10)

# Check whether 10 fields is enough (it is)
all(mat[, 10] == "") # TRUE

as_tibble(mat, .name_repair = NULL) %>%
  bind_cols(ticker = sp500_constituents$ticker, .) %>% 
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "pos", 
    values_to = "trading_raw_str"
  ) %>%
  filter(trading_raw_str != "") ->
  trading_raw_str_long

trading_raw_str_long %>%
  filter(str_detect(trading_raw_str, fixed(":"))) %>%
  mutate(exchange = str_split_fixed(trading_raw_str, fixed(":"), 2)[, 1]) %>%
  select(ticker, exchange) %>%
  distinct() -> sp500_exchanges

table(sp500_exchanges$exchange)

# We have two atypical strings separating two different classes of stocks
# fix:

sp500_exchanges$exchange[sp500_exchanges$exchange == "Common non-voting NYSE"] <- "NYSE" 
sp500_exchanges$exchange[sp500_exchanges$exchange == "Common NYSE"] <- "NYSE"
sp500_exchanges %>% distinct() -> sp500_exchanges

sp500_exchanges %>%
  group_by(exchange) %>%
  summarise(no_firms = n()) %>%
  arrange(-no_firms)

trading_raw_str_long %>%
  filter(!str_detect(trading_raw_str, fixed(":"))) %>%
  mutate(
    sp100 = str_detect(trading_raw_str, fixed("S&P 100")),
    nasdaq100 = str_detect(trading_raw_str, regex("NASDAQ.*100", ignore_case = TRUE)),
    djia = str_detect(trading_raw_str, regex("DJIA", ignore_case = TRUE))
  ) %>%
  group_by(ticker) %>%
  summarise(
    sp100 = any(sp100),
    nasdaq100 = any(nasdaq100),
    djia = any(djia)
  ) -> sp500_index_membership

sp500_founded_website <- sp500_wiki_info_box_raw %>%
  mutate (
    founded_as_in_info_box = get_min_4digits_from_str(Founded),
    website = ifelse(
      str_detect(Website, fixed(".")),
      paste0(
        "http://", 
        str_split_fixed(Website, regex("\\s+"), 2)[, 1]
      ),
      NA
    )
  ) %>%
  select(founded_as_in_info_box, website) %>%
  bind_cols(ticker = sp500_constituents$ticker, .)

isin_str <- str_replace(sp500_wiki_info_box_raw$ISIN, fixed("/"), " ")
mat_isin <- str_split_fixed(isin_str, regex("\\s+"), 3)
# Check whether 10 fields is enough (it is)
all(mat_isin[, 3] == "") # TRUE

sp500_ticker_isin <- 
  as_tibble(mat_isin, .name_repair = NULL) %>%
  bind_cols(ticker = sp500_constituents$ticker, .) %>% 
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "pos", 
    names_prefix = "V",
    values_to = "isin"
  ) %>%
  filter(isin != "") %>%
  mutate(pos = as.integer(pos))

sp500_ticker_isin %>%
  group_by(ticker) %>%
  filter(n() > 1)

# Techically, it should be feasible to convert this to a 1:1 match. Some 
# digging for dual shares would be required. I am leaving this to the
# interested reader ;-)

remove_wiki_footnote <- function(raw_string, numbers_only = FALSE) {
  if (numbers_only) str_remove_all(raw_string, regex("\\[\\d+\\]"))
  else str_remove_all(raw_string, regex("\\[[\\w\\s]+\\]"))
}

people <- str_split_fixed(
  remove_wiki_footnote(sp500_wiki_info_box_raw$`Key people`), fixed("\n"), 12
)
occupation_trigger <- c("(", "ceo", "chair", "chief", "executive", "president", 
                        "cfo", "general")
for (i in 1:nrow(people)) {
  for (j in 1:ncol(people)) {
    people[i, j] <- str_trim(people[i, j])
    people[i, j] <- str_remove_all(people[i, j], fixed(":129"))
    if (j > 1 && any(startsWith(tolower(people[i, j]), occupation_trigger))) {
      l <- 1
      while (people[i, j - l] == "") {l <- l + 1}
      people[i, j - l] <- paste(people[i, j - l], people[i, j])
      people[i, j] <- ""
    }
  }
}

as_tibble(people, .name_repair = NULL) %>%
  bind_cols(ticker = sp500_constituents$ticker, .) %>% 
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "pos", 
    names_prefix = "V",
    values_to = "people_string"
  ) %>%
  mutate(pos = as.integer(pos)) %>%
  filter(people_string != "") %>%
  group_by(ticker) %>%
  arrange(ticker, pos) %>%
  mutate(pos = row_number()) ->
  sp500_people

# I spotted some left-over parsing problems. I am leaving those and a separation
# of 'people_str' into names and roles for future work


# The function below will most likely silently fail in some instances. 
# For most of the data it works though.

extract_value <- function(raw_str, monetary = TRUE) {
  raw_str <- remove_wiki_footnote(raw_str)
  raw_str <- str_split_fixed(raw_str, fixed("\n"), 2)[, 1]
  raw_str <- str_remove_all(raw_str, regex("\\(.*\\)"))
  
  if (monetary) {
    non_usd <- !str_detect(raw_str, fixed("$")) & !str_detect(raw_str, fixed("US"))
    raw_str <- str_remove_all(raw_str, fixed("US-$"))
    raw_str <- str_remove_all(raw_str, fixed("USD"))
    raw_str <- str_remove_all(raw_str, fixed("US$"))
    raw_str <- str_remove_all(raw_str, fixed("$"))
    raw_str <- tolower(raw_str)
    unit <- ifelse(str_detect(raw_str, "t") , 6,
                   ifelse(str_detect(raw_str, "b") , 3,
                          ifelse(str_detect(raw_str, "m"), 0, -6)))
  } else {
    raw_str <- str_remove_all(raw_str, fixed("c."))
    unit <- ifelse(str_detect(tolower(raw_str), "million"), 6, 0)
  }
  
  value_str <- str_replace_all(str_match(raw_str, regex("(-?[0-9,\\.])+"))[,1], 
                               fixed(","), "")
  value <- ifelse(!is.na(value_str), as.numeric(value_str)*10^unit, NA)
  if (!monetary) value
  else ifelse(non_usd, NA, value)
}

sp500_wiki_info_box_raw %>%
  mutate(
    revenue_musd = extract_value(Revenue),
    op_income_musd = extract_value(`Operating income`),
    net_income_musd = extract_value(`Net income`),
    total_assets_musd = extract_value(`Total assets`),
    total_equity_musd = extract_value(`Total equity`),
    employees = extract_value(`Number of employees`, FALSE)
  ) %>%
  rename(
    revenue_str = Revenue,
    op_income_str = `Operating income`,
    net_income_str = `Net income`,
    total_assets_str = `Total assets`,
    total_equity_str = `Total equity`,
    employees_str = `Number of employees`
  ) %>%
  select(revenue_musd, revenue_str, op_income_musd, op_income_str, 
         net_income_musd, net_income_str, total_assets_musd, total_assets_str,
         total_equity_musd, total_equity_str, employees, employees_str) %>% 
  bind_cols(ticker = sp500_constituents$ticker, .) -> sp500_financials


sp500_base_data <- sp500_constituents %>%
  left_join(sp500_founded_website, by = "ticker") %>%
  left_join(
    sp500_financials %>% 
      select(ticker, ends_with("_musd"), employees), 
    by = "ticker"
  ) %>%
  mutate(
    founded = pmin(year_first_founded, founded_as_in_info_box, na.rm = TRUE),
    cik = as.factor(cik)
  ) %>%
  left_join(sp500_index_membership, by = "ticker") %>%
  select(-year_first_founded, -founded_as_in_info_box)

sp500_wiki_page_info_raw %>%
  rename(
    firm_name = `Display title`,
    page_id = `Page ID`,
  ) %>%
  mutate (
    page_length = as.numeric(str_replace_all(`Page length (in bytes)`, ",", "")),
    page_watchers = ifelse(
      str_detect(`Number of page watchers`, "Fewer"), 
      NA,
      as.numeric(str_replace_all(`Number of page watchers`, ",", ""))
    ),
    page_views_past_30_days = as.numeric(
      str_replace_all(`Page views in the past 30 days`, ",", "")
    ),
    date_page_created = dmy(str_sub(`Date of page creation`, 7)),
    date_last_edited = dmy(str_sub(`Date of latest edit`, 7)),
    total_nr_of_edits = as.numeric(str_replace_all(`Total number of edits`, ",", "")),
    recent_nr_edits = as.numeric(`Recent number of edits (within past 30 days)`), 
    recent_nr_authors = as.numeric(`Recent number of distinct authors`)
  ) %>%
  select(firm_name, page_id, page_length, date_page_created, date_last_edited,
         page_views_past_30_days, page_watchers, total_nr_of_edits, recent_nr_edits,
         recent_nr_authors) %>%
  bind_cols(ticker = sp500_constituents$ticker, .) -> sp500_wiki_data


save(
  sp500_base_data,
  sp500_wiki_data,
  sp500_people,
  sp500_ticker_isin, 
  file = "data/sp500_wiki_clean.Rdata"
) 

# --- Some quick exploration ---------------------------------------------------

load("data/sp500_wiki_clean.Rdata")

sp500_data <- sp500_base_data %>%
  left_join(sp500_wiki_data, by = "ticker")
rownames(sp500_data) <- sp500_base_data$name
ExPanD(sp500_data)