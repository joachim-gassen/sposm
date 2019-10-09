library(tidyverse)
library(zip)

url <- "https://www.sec.gov/files/dera/data/financial-statement-data-sets/2019q2.zip"
temp_dir <- tempdir()

download.file(url, file.path(temp_dir, "sec.zip"))
unzip(file.path(temp_dir, "sec.zip"), exdir = temp_dir)

for(df_name in c("sub", "tag", "num", "pre")) {
  write_csv(
    read_tsv(file.path(temp_dir, paste0(df_name, ".txt")), quote = ""),
    paste0("data/", df_name, ".csv")
  )
}

unlink(temp_dir)