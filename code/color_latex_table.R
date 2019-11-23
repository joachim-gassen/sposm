library(tidyverse)

#' @title Color Latex Table Cells
#'
#' @description A quick and dirty implementation to color the text in latex
#'   table cells. The function will be able to handle most but certainly not
#'   all latex tables and relies on somewhat well-behaved latex code.
#'
#' @param tab A character vector containing the latex code to generate the 
#'   table. Needs to contain one \code{tabular} environment.
#'   
#' @param cells A vector of length two or a two column array or dataframe 
#'   (row, column) containing the point(s) to be colored.
#'   
#' @param colors A column vector containing the colors to use
#   for each cell.
#'
#' @param quiet If \code{FALSE}, the code will send a message to the console
#'  displaying the row and column count.
#'
#' @details
#'
#' If called with cells = NULL and color = NULL it returns
#' a vector containing the row and column count of the parsed table.
#' 
#' This implementation is incomplete as it does not parse latex code
#' properly. Try to break it by feeding latex code that uses non-standard
#' formatting and content.

color_latex_table <- function(tab, cells = NULL, color = NULL, quiet = FALSE) {

  # Isolate the tabular environment
  
  p_tab_begin <- "\\\\begin\\{tabular\\}(\\{[^\\}]*\\}|\\[[^\\]]*\\])*"
  p_tab_end <- "\\\\end\\{tabular\\}"
  
  cut_begin <- str_locate_all(tab, p_tab_begin)[[1]]
  if (nrow(cut_begin) == 0) 
    stop ("Found no begin for 'tabular' environment")
  if (nrow(cut_begin) > 1) 
    stop ("Found multiple begins for 'tabular' environments")
  
  cut_end <- str_locate_all(tab, p_tab_end)[[1]]
  if (nrow(cut_end) == 0) 
    stop ("Found no end for 'tabular' environment")
  if (nrow(cut_end) > 1) 
    stop ("Found multiple ends for 'tabular' environments")
  
  if (cut_end[1,1] < cut_begin[1,1])
    stop ("Encountered end of 'tabular' environment before begin")
  
  tab_tokens <- 
    tibble(
      type = c("pre", "begin", "end", "post"),
      start = c(1, cut_begin[1,1], cut_end[1,1], cut_end[1,2] + 1), 
      end = c(cut_begin[1,1] - 1, cut_begin[1,2], cut_end[1,2], str_length(tab)) 
    )

  tab_env <- str_sub(tab, cut_begin[1,2] + 1, cut_end[1,1] - 1)
  
  
  # Indentify all relevant latex tokens in table
  
  p_row_sep <- "\\\\\\\\(\\[[^\\]]*\\])?"
  p_col_sep <- "(?<!\\\\)&"
  p_hline <- "\\\\hline"
  p_cline <- "\\\\cline\\{\\d{1,2}-\\d{1,2}\\}"
  
  add_tokens <- function(tokens, name, pattern) {
    tokens %>%
      bind_rows(
        tibble(
          type = name,
          start = str_locate_all(tab_env, pattern)[[1]][, 1] + cut_begin[1,2],
          end   = str_locate_all(tab_env, pattern)[[1]][, 2] + cut_begin[1,2],
        )
      )
  }
  
  tab_tokens <-
    tab_tokens %>%
    add_tokens("row_sep", p_row_sep) %>%
    add_tokens("col_sep", p_col_sep) %>%
    add_tokens("hline", p_hline) %>%
    add_tokens("cline", p_cline) %>%
    arrange(start)

  
  # Identify number of columns in table
  
  col_count <- 0
  nr_cols <- 0
  for (i in 1:nrow(tab_tokens)) {
    if (tab_tokens$type[i] == "col_sep") {
      col_count <- col_count + 1
      if (col_count > nr_cols) nr_cols <- col_count
    }
    if (tab_tokens$type[i] == "row_sep") col_count <- 0
  }
  nr_cols = nr_cols + 1
  
  if (nr_cols == 0) stop("No column separators found in tabular environment")
  if (!quiet) 
    message(sprintf("Found table with %s columns ", nr_cols), appendLF = FALSE)

  
  # Identify cell content in between tokens
  
  p_multicol_cols <- "(?<=\\\\multicolumn\\s{0,100}\\{)\\s*\\d+\\s*(?=\\})"
  p_multirow_rows <- "(?<=\\\\multirow\\s{0,100}\\{)\\s*\\d+\\s*(?=\\})"
  p_multicol_cell <- paste0("(?<=\\\\multicolumn\\s{0,100}\\{[^\\}]{1,100}\\}",
                            "\\s{0,100}\\{[^\\}]{1,100}\\}\\s{0,100}\\{)",
                            "[^\\}]*(?=\\})")
  p_multirow_cell <- str_replace(p_multicol_cell, "multicolumn", "multirow")

  covered_by_multirow <- function(row, col) {
    if (all(is.na(tab_tokens$multirow[tab_tokens$column == col]))) 
      return(FALSE)
    return(
      max(tab_tokens$multirow[tab_tokens$column == col], na.rm = TRUE) >= row
    ) 
  }

  tab_tokens$row <- NA
  tab_tokens$column <- NA
  tab_tokens$multicol <- NA
  tab_tokens$multirow <- NA
  row <- 0
  column <- 0
  for (i in 1:nrow(tab_tokens)) {
     if (tab_tokens$type[i] %in% c("col_sep", "row_sep", "end")) {
      pc <- str_sub(tab, tab_tokens$end[i-1] + 1, tab_tokens$start[i] - 1)
      mc <- as.numeric(str_extract(pc, p_multicol_cols))
      mr <- as.numeric(str_extract(pc, p_multirow_rows))
      if (!is.na(mc)) {
        column <- column + mc 
        tab_tokens$multicol[i] <- column + mc - 1
        new_es <- str_locate(pc, p_multicol_cell)
        tab_tokens$start[i] <- tab_tokens$end[i-1] + new_es[2] + 1
        tab_tokens$end[i-1] <- tab_tokens$end[i-1] + new_es[1] - 1
      } else {
        column <- column + 1
        mc <- 1
      }
      if (!is.na(mr)) {
        tab_tokens$multirow[i] <- row + mr - 1
        new_es <- str_locate(pc, p_multirow_cell)
        tab_tokens$start[i] <- tab_tokens$end[i-1] + new_es[2] + 1
        tab_tokens$end[i-1] <- tab_tokens$end[i-1] + new_es[1] - 1
      }
      if (tab_tokens$type[i] == "col_sep") {
        if (row == 0) row = 1
        if(!covered_by_multirow(row, column)) {
          tab_tokens$row[i] <- row
          tab_tokens$column[i] <- column - mc + 1
        }
      }

      if (tab_tokens$type[i] %in% c("row_sep", "end")) {
        if (column < nr_cols) {
          if (column != 1) stop(
            sprintf("Inconsistent cell count (%d) in row %d", column, row)
          )
          if(str_detect(pc, "[^\\s]")) stop (
            sprintf("Found non-whitespace characters in empty row %d", row)
          )
          column <- 0
        } else {
          if(!covered_by_multirow(row, column)) {
            tab_tokens$row[i] <- row
            tab_tokens$column[i] <- column - mc + 1
          }
          column <- 0
          row <- row + 1
        }
      }          
    }
  }
  
  nr_rows <- max(c(tab_tokens$row, tab_tokens$multirow), na.rm = TRUE)
  if (!quiet) 
    message(sprintf("and %s rows", nr_rows))
  
  if (is.null(cells) && is.null(color)) return(c(nr_rows, nr_cols))
  
  
  # Prepare and return colored table
  
  cell_lookup <- expand_grid(row = seq(nr_rows), col = seq(nr_cols)) %>%
    mutate(start = NA, end = NA)
  
  pos <- apply(cell_lookup, 1, function(x) {
    token <- max(which(tab_tokens$row <= x[1] & tab_tokens$column <= x[2]))
    c(tab_tokens$end[token - 1] + 1, tab_tokens$start[token] - 1)
  })
  
  cell_lookup$start <- pos[1, ]
  cell_lookup$end <- pos[2, ]
  
  if (is.vector(cells) && length(cells) == 2) cells <- matrix(cells, 1, 2)
  cell_df <- as.data.frame(cells)
  rownames(cell_df) <- NULL
  names(cell_df) <- c("row", "col")
  cell_df$color <- color

  colored_cells <- cell_df %>%
    inner_join(cell_lookup, by = c("row", "col")) %>%
    mutate(text = sprintf(
      " \\leavevmode\\color{%s} %s \\color{black} ", 
      color, str_sub(tab, start, end))
    ) 
  
  if (nrow(colored_cells) < nrow(cell_df)) {
    stop("You are trying to color cells that are not found in the table")
  }
  
  unique_colored_cells <- colored_cells %>%
    distinct(start, .keep_all = TRUE) %>%
    arrange(start, end, -col, -row) 

  if (nrow(unique_colored_cells) < nrow(colored_cells)) {
    warning(
      "You are trying to color cells that are covered by multicolumn or mutlirow"
    )
  }
  
  colored_cells <- unique_colored_cells
  
  tab_out <- str_sub(tab, 1, colored_cells$start[1] - 1)
  for (i in 1:nrow(colored_cells)) {
    tab_out <- paste0(tab_out, colored_cells$text[i])
    if (i < nrow(colored_cells)) 
      tab_out <- paste0(
        tab_out, 
        str_sub(tab, colored_cells$end[i] + 1, colored_cells$start[i + 1] - 1)
      )    
  }
  tab_out <- paste0(tab_out, str_sub(tab, colored_cells$end[i] + 1))

  return(tab_out)
}
