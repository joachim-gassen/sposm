library(jsonlite)

btag_open_data_api <- "http://127.0.0.1:5847/"

reden_merkel <- fromJSON(paste0(btag_open_data_api, "rede_text_by_id?id=11001478"))
