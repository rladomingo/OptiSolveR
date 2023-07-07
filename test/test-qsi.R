# Load R packages
if (!require(testthat)) install.packages('testthat')

# Source qsi function
source("../src/qsi.R")

test_that(desc = "QSI UI", code = {
  text <- "1.6 2\n2 8\n2.5 14\n19 124\n-9 4\n3.2 7"
  result <- poly.qsi(GetValues(text), 4)
  expect_that(object = result$y, condition = equals(-3.1897))
  
})

test_that(desc = "QSI Function", code = {
  data <- list(c(1.6, 2, 2.5), c(2, 8, 14))
  result <- poly.qsi(data, 2.2)
  expect_that(object = result$y, condition = equals(10.76))
  
  data <- list(c(3.0, 4.5, 7.0, 9.0), c(2.5, 1.0, 2.5, 0.5))
  result <- poly.qsi(data, 5)
  expect_that(object = result$y, condition = equals(0.66))
  
  result <- poly.qsi(data, 8)
  expect_that(object = result$y, condition = equals(3.1))
  
  result <- poly.qsi(data, 8.5)
  expect_that(object = result$y, condition = equals(2.2))
  
  result <- poly.qsi(data, 20)
  expect_that(object = result, condition = equals(NA))
  
  result <- poly.qsi(data, 3.0)
  expect_that(object = result$y, condition = equals(2.5))
})

