library(tidyverse)
library(testthat)
library(tinytex)

# Note: Test files are executed with the path set to the test directory

test_files <- list.files(pattern = "\\.tex$", full.names = TRUE)

test_cases <- sapply(test_files, read_file)
temp_dir <- tempdir()

# source("assignments/coding_sprint_2/color_latex_table.R")
source("../color_latex_table.R")


test_that("useless input creates an error", {
  expect_error(color_latex_table("Blob", c(1,1), "banana"))
})
          

tab_sizes <- matrix(c(3, 3, 19, 5, 5, 9, 3, 3), 
                    nrow = 4 , ncol = 2, byrow = TRUE)

for (i in 1:nrow(tab_sizes)) {
  cells <- expand.grid(
    row = seq(tab_sizes[i, 1]),
    col = seq(tab_sizes[i, 2])
  )
  
  # omit duplicate multirow/multicolumn cells
  if (i == 1) cells <- cells %>% filter(
    !(row == 1 & col == 3),
    !(row == 2 & col == 1)
  )
  if (i == 2) cells <- cells %>% filter(
    !(row == 1 & col %in% 3:5)
  )
  
  color <- sample(c("red", "green", "blue"), 
                  nrow(cells), replace = TRUE)
  
  test_that(
    sprintf("Processing test case %s returns character data", 
            basename(test_files[i])), 
    {
      expect_true(
        is.character(color_latex_table(test_cases[i], cells, color))
      )
    }
  )
  
  test_that(
    sprintf(
      "Latex code of test case %s can be parsed into a PDF", 
      basename(test_files[i])), 
    {
      tex_file <- file.path(temp_dir, sprintf("%d.tex", i))
      pdf_file <- file.path(temp_dir, sprintf("%d.pdf", i))
      write_file(color_latex_table(test_cases[i], cells, color), tex_file)
      pdflatex(tex_file, pdf_file = pdf_file)
      expect_true(file.exists(pdf_file))
      
      # All the things below do not work across platforms as 
      # testthat uses a bare non RStudio envrironment
      # system2('open', args = pdf_file, wait = TRUE)
      # getOpt("viewer")(pdf_file)
      # system2(Sys.getenv("R_PDFVIEWER"), args = pdf_file, wait = TRUE)
    }
  )
}

test_that("Coloring a cell that does not exist throws an error", {
  expect_error(color_latex_table(test_cases[4], c(4,4), "green"))  
})

test_that(paste("Coloring two cells that are in the same multi envronment",
                "produces a warning"), {
  expect_warning(
    color_latex_table(test_cases[1], matrix(c(1, 1, 2, 3), 2, 2), "green")
    )  
})

