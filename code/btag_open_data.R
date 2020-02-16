# This is a demo code file for in-classroom use to explain headless 
# browsing. Refer to the other 'btag_open_data_*.R" file for the
# real code

library(tidyverse)
library(rvest)
library(RSelenium)

# The following won't work as the Bundestag Open Data webpage
# uses Javascript (great idea!)

url_btag_open_data <- "https://www.bundestag.de/services/opendata"
url_btag_open_data %>%
  read_html() %>%
  html_node(
    xpath = '//*[@id="bt-collapse-543410"]/div[1]/div/div/div[1]/table'
  ) %>% html_table() -> pp_table


# Need to run a docker instance in local development envrionment
# that sets up the "remote" Selenium driver, e.g. (for chrome browser):
#
# docker run -d -p 4445:4444 selenium/standalone-chrome
#

url <- "https://www.bundestag.de/services/opendata" 
rem_dr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)
rem_dr$open(silent = TRUE)
rem_dr$navigate(url)
rem_dr$setWindowSize(1280L, 1500L)
rem_dr$screenshot(display = TRUE)
  
last_arrow <- FALSE
while (!last_arrow) {
  next_arrow <- rem_dr$findElement(using = "class", "slick-next")
  if (next_arrow$getElementAttribute("class") == "slick-next slick-arrow slick-disabled") {
    last_arrow <- TRUE
  } else {
    next_arrow$clickElement()
    Sys.sleep(1)
  }
}
  
rem_dr$screenshot(display = TRUE)

web_elems <- rem_dr$findElements(using = "class", "bt-link-dokument")
pp_links <- unlist(lapply(web_elems, function(x) {x$getElementAttribute("href")}))
rem_dr$close()
