library(testthat)

# Note: Test files are executed with the path set to the test directory

setwd("../..")
source("code/btag_open_data_scrape_data.R")

context('test whether scraping provides urls of Plenarprotokolle')

test_that('get_pp_urls_from_btag_open_data_webpage() returns character vector', {
  rv <<- get_pp_urls_from_btag_open_data_webpage()
  expect_true(is.character(rv))
})

test_that('get_pp_urls_from_btag_open_data_webpage() returns more than 140 urls', {
  expect_true(length(rv) > 140)
})

test_that('random url of get_pp_urls_from_btag_open_data_webpage() is downloadable', {
  url <- rv[as.integer(round(runif(1, 1, 140)))]
  expect_error(download.file(url, file.path(tempdir(), "test.xml"), quiet = TRUE), NA)
})


context('test whether download_pp_xml_file() succeeds in downloading files')

test_that('random url gets downloaded to correct directory', {
  url <- rv[as.integer(round(runif(1, 1, 140)))]
  fpath <- file.path("data/btag_open_data_xml", basename(url))
  if (file.exists(fpath)) file.remove(fpath)
  download_pp_xml_file(url, quiet = TRUE)
  expect_true(file.exists(fpath))
})


context('test whether parse_reden_data() extracts reden data from XML')

test_that('parse_reden_data() creates two data frames with correct format', {
  url <- rv[as.integer(round(runif(1, 1, 140)))]
  fpath <- file.path("data/btag_open_data_xml", basename(url))
  if (!file.exists(fpath)) download_pp_xml_file(url, quiet = TRUE)
  pp_xml <- read_xml(fpath)
  reden_xml <- xml_find_all(pp_xml, ".//rede")
  reden_list <- as_list(reden_xml)
  list2env(parse_reden_data(reden_list[[1]]), environment())
  
  expect_true(is.data.frame(redner))
  expect_length(redner, 3)
  expect_equal(names(redner), c("id", "vorname", "nachname"))
  expect_equal(nrow(redner), 1)
  expect_true(!is.na(redner$id[1]))
  expect_type(redner$vorname, "character")
  expect_type(redner$nachname, "character")

  expect_true(is.data.frame(rede_text))
  expect_length(rede_text, 3)
  expect_equal(names(rede_text), c("pos", "kommentar", "text"))
  expect_gte(nrow(rede_text), 1)
  expect_type(rede_text$pos, "integer")
  expect_type(rede_text$kommentar, "logical")
  expect_type(rede_text$text, "character")
})


context('test whether parse_pp_xml_file() successfully parses a Plenarprotokoll')

test_that('parse_pp_xml_file() creates three data frames with correct format', {
  url <- rv[as.integer(round(runif(1, 1, 140)))]
  fpath <- file.path("data/btag_open_data_xml", basename(url))
  if (!file.exists(fpath)) download_pp_xml_file(url)
  list2env(parse_pp_xml_file(fpath), environment())

  expect_true(is.data.frame(sitzung))
  expect_length(sitzung, 4)
  expect_equal(names(sitzung), c("wahlperiode", "sitzungs_nr", "ort", 
                                "datum"))
  expect_equal(nrow(sitzung), 1)
  expect_true(!anyNA(sitzung$wahlperiode))
  expect_true(!anyNA(sitzung$sitzungs_nr))
  expect_type(sitzung$wahlperiode, "integer")
  expect_type(sitzung$sitzungs_nr, "integer")
  expect_type(sitzung$ort, "character")
  expect_true(is.Date(sitzung$datum))

  expect_true(is.data.frame(redner))
  expect_length(redner, 6)
  expect_equal(names(redner), c("wahlperiode", "sitzungs_nr", "rede_pos", 
                                "id", "vorname", "nachname"))
  expect_gte(nrow(redner), 1)
  expect_true(!anyNA(redner$wahlperiode))
  expect_true(!anyNA(redner$sitzungs_nr))
  expect_true(!anyNA(redner$rede_pos))
  expect_true(!anyNA(redner$id))
  expect_type(redner$wahlperiode, "integer")
  expect_type(redner$sitzungs_nr, "integer")
  expect_type(redner$rede_pos, "integer")
  expect_type(redner$id, "character")
  expect_type(redner$vorname, "character")
  expect_type(redner$nachname, "character")

  expect_true(is.data.frame(reden_text))
  expect_length(reden_text, 6)
  expect_equal(names(reden_text), c("wahlperiode", "sitzungs_nr", "rede_pos",
                                     "pos", "kommentar", "text"))
  expect_gte(nrow(reden_text), nrow(redner))
  expect_true(!anyNA(reden_text$wahlperiode))
  expect_true(!anyNA(reden_text$sitzungs_nr))
  expect_true(!anyNA(reden_text$rede_pos))
  expect_true(!anyNA(reden_text$pos))
  expect_type(reden_text$wahlperiode, "integer")
  expect_type(reden_text$sitzungs_nr, "integer")
  expect_type(reden_text$rede_pos, "integer")
  expect_type(reden_text$pos, "integer")
  expect_type(reden_text$kommentar, "logical")
  expect_type(reden_text$text, "character")
  
  rede_pos <- sort(unique(reden_text$rede_pos))
  
  expect_equal(redner$rede_pos, 1:nrow(redner))
  expect_equal(rede_pos, redner$rede_pos)
})


context('test whether scrape_reden_form_plenarprotokolle() successfully parses all Plenarprotokolle')

test_that('scrape_reden_form_plenarprotokolle() creates three data frames with correct format', {
  rlst <- suppressMessages(scrape_reden_form_plenarprotokolle())
  list2env(rlst, environment())
  
  expect_true(is.data.frame(sitzungen))
  expect_length(sitzungen, 4)
  expect_equal(names(sitzungen), c("wahlperiode", "sitzungs_nr", "ort", 
                                  "datum"))
  expect_gte(nrow(sitzungen), 145)
  expect_true(!anyNA(sitzungen$wahlperiode))
  expect_true(!anyNA(sitzungen$sitzungs_nr))
  expect_type(sitzungen$wahlperiode, "integer")
  expect_type(sitzungen$sitzungs_nr, "integer")
  expect_type(sitzungen$ort, "character")
  expect_true(is.Date(sitzungen$datum))
  
  expect_true(is.data.frame(redner))
  expect_length(redner, 6)
  expect_equal(names(redner), c("wahlperiode", "sitzungs_nr", "rede_pos", 
                                "id", "vorname", "nachname"))
  expect_gte(nrow(redner), 145)
  expect_true(!anyNA(redner$wahlperiode))
  expect_true(!anyNA(redner$sitzungs_nr))
  expect_true(!anyNA(redner$rede_pos))
  expect_true(!anyNA(redner$id))
  expect_type(redner$wahlperiode, "integer")
  expect_type(redner$sitzungs_nr, "integer")
  expect_type(redner$rede_pos, "integer")
  expect_type(redner$id, "character")
  expect_type(redner$vorname, "character")
  expect_type(redner$nachname, "character")
  
  expect_true(is.data.frame(reden_texte))
  expect_length(reden_texte, 6)
  expect_equal(names(reden_texte), c("wahlperiode", "sitzungs_nr", "rede_pos",
                                    "pos", "kommentar", "text"))
  expect_gte(nrow(reden_texte), nrow(redner))
  expect_true(!anyNA(reden_texte$wahlperiode))
  expect_true(!anyNA(reden_texte$sitzungs_nr))
  expect_true(!anyNA(reden_texte$rede_pos))
  expect_true(!anyNA(reden_texte$pos))
  expect_type(reden_texte$wahlperiode, "integer")
  expect_type(reden_texte$sitzungs_nr, "integer")
  expect_type(reden_texte$rede_pos, "integer")
  expect_type(reden_texte$pos, "integer")
  expect_type(reden_texte$kommentar, "logical")
  expect_type(reden_texte$text, "character")

  reden_texte %>%
    left_join(redner, by = c("wahlperiode", "sitzungs_nr", "rede_pos")) %>%
    left_join(sitzungen, by = c("wahlperiode", "sitzungs_nr")) -> df
  
  expect_true(!anyNA(df$id))
  expect_true(!anyNA(df$datum))
  
  redner %>%
    left_join(reden_texte, by = c("wahlperiode", "sitzungs_nr", "rede_pos")) -> df
  expect_true(!anyNA(df$pos))

  sitzungen %>%
    left_join(redner, by = c("wahlperiode", "sitzungs_nr")) -> df
  expect_true(!anyNA(df$id))
})



