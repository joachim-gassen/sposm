library(testthat)

# desc_table returns selected descriptive statistics for each numerical and 
# logical variable in a data frame. It uses a reasonable default of statistics 
# but the user can provide a vector containing statistical function to apply.
# desc_table purges missing values prior to the analysis.


setwd("../..")

context("test parameters of function desc_table()")

test_that("test whether `code/desc_table.R` is present",{
  expect_true(file.exists("code/desc_table.R"))
})

source("code/desc_table.R")

test_that("desc_table() is a function", {
  expect_type(desc_table, "closure")
})

test_that("desc_table() requires a data frame as first parameter", {
  expect_error(desc_table())
  expect_error(desc_table(1L))
  expect_error(desc_table(list(a = 1:10, b = 1:10)))
  expect_error(desc_table(data.frame(a = 1:10, b = 1:10)), NA)
})

# Prepare a test data frame  
mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
colnames(mat) <- paste0("numeric", 1:10)
df <- as.data.frame(mat)
df$factor1 <- as.factor(sample(LETTERS[1:5], 100, replace = TRUE))
df$factor2 <- as.factor(sample(letters[6:10], 100, replace = TRUE))
df$logical <- sample(c(TRUE, FALSE), 100, replace = TRUE) 
df$character <- sample(c("BLUB", "BLOBB", "BLABB"), 100, replace = TRUE) 

test_that("desc_table() accepts a vector of functions as a second parameter", {
  expect_error(desc_table(df, 1L))
  expect_error(desc_table(df, 1:10))
  
  funcs <- c(function(x) sum(is.finite(x)), mean, sd, min, 
             function(x) quantile(x, 0.25), median, 
             function(x) quantile(x, 0.75), max)
  expect_error(desc_table(df, funcs), NA)
})


context("test return value of desc_table()")

test_that("desc_table() returns a data frame", {
  expect_true(is.data.frame(desc_table(df)))
})

test_that("desc_table() returns a data frame with correct rows", {
  expected_rows <- sum(sapply(df, is.numeric)) + sum(sapply(df, is.logical))
  rv <- desc_table(df)
  expect_true(is.data.frame(rv))
  expect_equal(nrow(rv), expected_rows)
  expect_equal(rownames(rv), names(df)[sapply(df, is.numeric) | sapply(df, is.logical)])
})

test_that("desc_table() returns a data frame with correct columns", {
  funcs <- c(n = function(x) sum(is.finite(x)), mean = mean, sd = sd, 
                 min = min, 
                 q25 = function(x) quantile(x, 0.25), median = median, 
                 q75 = function(x) quantile(x, 0.75), max = max)

  rv <- desc_table(df, funcs)
  expect_true(is.data.frame(rv))
  expect_equal(ncol(rv), length(funcs))
  expect_equal(colnames(rv), c("n", "mean", "sd", "min", "q25", "median", "q75", "max"))
})


context("test whether statisitcs provided by desc_table() are correct")

test_that("statisitcs provided by desc_table() are correct", {
  funcs <- c(n = function(x) sum(is.finite(x)), mean = mean, sd = sd, 
             min = min, 
             q25 = function(x) quantile(x, 0.25), median = median, 
             q75 = function(x) quantile(x, 0.75), max = max)
  
  rv <- desc_table(df, funcs)
  expect_equal(sum(is.finite(df$numeric1)), rv["numeric1", "n"]) 
  expect_equal(mean(df$logical, na.rm = TRUE), rv["logical", "mean"]) 
  expect_equal(sd(df$numeric2, na.rm = TRUE), rv["numeric2", "sd"]) 
  expect_equal(min(df$numeric3, na.rm = TRUE), rv["numeric3", "min"]) 
  expect_equal(quantile(df$numeric4, 0.25, na.rm = TRUE), rv["numeric4", "q25"], check.names = FALSE) 
  expect_equal(median(df$numeric5, na.rm = TRUE), rv["numeric5", "median"]) 
  expect_equal(quantile(df$numeric6, 0.75, na.rm = TRUE), rv["numeric6", "q75"], check.names = FALSE) 
  expect_equal(max(df$numeric7, na.rm = TRUE), rv["numeric7", "max"]) 
})


context("test for robustness with regards to missing values")

df_na <- df

for (c in 1:14) {
  rn <- runif(100)
  df_na[rn < 0.1, c] <- NA
}

test_that("statisitcs provided by desc_table() are correct", {
  funcs <- c(n = function(x) sum(is.finite(x)), mean = mean, sd = sd, 
             min = min, 
             q25 = function(x) quantile(x, 0.25), median = median, 
             q75 = function(x) quantile(x, 0.75), max = max)
  
  rv <- desc_table(df_na, funcs)
  expect_equal(sum(is.finite(df_na$numeric1)), rv["numeric1", "n"]) 
  expect_equal(mean(df_na$logical, na.rm = TRUE), rv["logical", "mean"]) 
  expect_equal(sd(df_na$numeric2, na.rm = TRUE), rv["numeric2", "sd"]) 
  expect_equal(min(df_na$numeric3, na.rm = TRUE), rv["numeric3", "min"]) 
  expect_equal(quantile(df_na$numeric4, 0.25, na.rm = TRUE), rv["numeric4", "q25"], check.names = FALSE) 
  expect_equal(median(df_na$numeric5, na.rm = TRUE), rv["numeric5", "median"]) 
  expect_equal(quantile(df_na$numeric6, 0.75, na.rm = TRUE), rv["numeric6", "q75"], check.names = FALSE) 
  expect_equal(max(df_na$numeric7, na.rm = TRUE), rv["numeric7", "max"]) 
})



