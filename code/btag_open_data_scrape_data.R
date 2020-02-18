library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)
library(xml2)
library(data.table)

# Need to run a docker instance in local development envrionment
# that sets up the "remote" Selenium driver, e.g. (for chrome browser):
#
# docker run -d -p 4445:4444 selenium/standalone-chrome
#

get_pp_urls_from_btag_open_data_webpage <- function(
  url = "https://www.bundestag.de/services/opendata") {
  rem_dr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "chrome"
  )
  rem_dr$open(silent = TRUE)
  rem_dr$navigate(url)
  rem_dr$setWindowSize(1280L, 1500L)

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
  
  web_elems <- rem_dr$findElements(using = "class", "bt-link-dokument")
  bt_links <- unlist(lapply(web_elems, function(x) {x$getElementAttribute("href")}))
  rem_dr$close()
  return(bt_links)  
}


download_pp_xml_file <- function(file, download = FALSE, ...) {
  fpath <- file.path("data/btag_open_data_xml", basename(file))
  if (!file.exists(fpath) | download)
  download.file(file, fpath, ...)
}


parse_reden_data <- function(rede_list) {
  redner <- tibble(
    id = attr(rede_list[[1]]$redner, "id"),
    vorname = ifelse(is.null(rede_list[[1]]$redner$name$vorname), NA,
                     rede_list[[1]]$redner$name$vorname[[1]]),
    nachname = ifelse(is.null(rede_list[[1]]$redner$name$nachname), NA,
                      rede_list[[1]]$redner$name$nachname[[1]])
  )

  if (length(rede_list[[1]]) > 1 & length(names(rede_list[1])) == 1) {
    if (length(rede_list) > 1)
      rede_list <- c(p = rede_list[1]$p[[1]], p = list(rede_list[1]$p[[2]]), 
                     rede_list[2:length(rede_list)])
    else rede_list <- c(p = rede_list[1]$p[[1]], p = list(rede_list[1]$p[[2]]))
  }
  if (length(rede_list) >= 2) {
    if (names(rede_list)[2] == "p") {
      rede_text <- tibble(
        pos = 1L,
        kommentar = FALSE,
        text = rede_list[[2]][[1]]
      ) 
    } else {
      rede_text <- tibble(
        pos = 1L,
        kommentar = TRUE,
        text = rede_list[[2]][[1]]
      )
    }
    
    i <- 3L
    while(i <= length(rede_list)) {
      if (is.list(rede_list[[i]]) & length(rede_list[[i]]) > 0) {
        if ((names(rede_list[i]) == "kommentar") ||
            (names(rede_list[i]) == "p" && 
             (is.null(attr(rede_list[[i]], "klasse")) ||
              attr(rede_list[[i]], "klasse") == "J" || 
              attr(rede_list[[i]], "klasse") == "J_1" || 
              attr(rede_list[[i]], "klasse") == "O"))) 
            {
          if (names(rede_list)[i] == "p") 
            rede_text <- rbind(rede_text, list(i - 1L, FALSE, rede_list[[i]][[1]]))
          else 
            rede_text <- rbind(rede_text, list(i - 1L, TRUE, rede_list[[i]][[1]]))
        } 
      }
      i <- i + 1L
    }
    return(list(redner = redner, rede_text = rede_text))
  } else return(list(redner = redner, 
                     rede_text = tibble(
                       pos = 1L, 
                       kommentar = as.logical(NA), 
                       text = as.character(NA))
  ))
}


parse_pp_xml_file <- function(file) {
  message(sprintf("Parsing %s ... ", file), appendLF = FALSE)
  wahlperiode <- as.integer(substr(basename(file), 1, 2))
  sitzungs_nr <- as.integer(substr(basename(file), 3, 5))
  pp_xml <- read_xml(file)
  pp_list <- as_list(pp_xml)
  ort <- pp_list$dbtplenarprotokoll$vorspann$kopfdaten$veranstaltungsdaten$ort[[1]]
  datum <- dmy(attr(pp_list$dbtplenarprotokoll$vorspann$kopfdaten$veranstaltungsdaten$datum, "date"))
  reden_xml <- xml_find_all(pp_xml, ".//rede")
  reden_list <- as_list(reden_xml)
  
  reden_data_list <- lapply(reden_list, parse_reden_data) 
  redner_list <- lapply(reden_data_list, function(x) x$redner)
  redner <- rbindlist(redner_list, idcol = "rede_pos")
  reden_text_list <- lapply(reden_data_list, function(x) x$rede_text)
  reden_text <- rbindlist(reden_text_list, idcol = "rede_pos")
  message("done!")
  return(list(
    sitzung = tibble(
      wahlperiode = wahlperiode,
      sitzungs_nr = sitzungs_nr,
      ort = ort,
      datum = datum
    ),
    redner = redner %>% add_column(
      wahlperiode = wahlperiode,
      sitzungs_nr = sitzungs_nr,
      .before = TRUE
    ),
    reden_text = reden_text %>% add_column(
      wahlperiode = wahlperiode,
      sitzungs_nr = sitzungs_nr,
      .before = TRUE
    )
  ))
}

scrape_reden_form_plenarprotokolle <- function(download = FALSE) {
  if (download) {
    bt_links <- get_pp_urls_from_btag_open_data_webpage()
    pp_xml_files <- bt_links[str_detect(bt_links, fixed("-data.xml"))]
    if (!dir.exists("data/btag_open_data_xml")) dir.create("data/btag_open_data_xml")
    invisible(lapply(pp_xml_files, function (x) download_pp_xml_file(x, download)))
    invisible(lapply(bt_links[1:3], function(x) download.file(x, file.path("data/btag_open_data_xml", basename(x)))))
    
    pp_xml_files <- lapply(pp_xml_files, function(x) file.path("data/btag_open_data_xml", basename(x)))
  } else pp_xml_files <- list.files(path = "data/btag_open_data_xml",
                                    pattern = "\\-data.xml$", full.names = TRUE)
  
  pp_lst <- lapply(pp_xml_files, parse_pp_xml_file)
  sitzungen_list <- lapply(pp_lst, function(x) x$sitzung)
  sitzungen <- rbindlist(sitzungen_list) %>%
    arrange(wahlperiode, sitzungs_nr)
  redner_list <- lapply(pp_lst, function(x) x$redner) 
  redner <- rbindlist(redner_list) %>%
    arrange(wahlperiode, sitzungs_nr, rede_pos)
  reden_texte_list <- lapply(pp_lst, function(x) x$reden_text)
  reden_texte <- rbindlist(reden_texte_list) %>%
    arrange(wahlperiode, sitzungs_nr, rede_pos, pos)
  
  return(list(sitzungen = sitzungen, redner = redner, reden_texte = reden_texte))
} 


# list2env(scrape_reden_form_plenarprotokolle(download = TRUE), environment())
# save(sitzungen, redner, reden_texte, file = "data/btag_open_data_19wp_reden.RData")
