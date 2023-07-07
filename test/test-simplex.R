# Load R packages
if (!require(testthat)) install.packages('testthat')

# Source simplex function
source("../src/simplex.R")

test_that(desc = "Maximization", code = {
  x <- list("x1" = c(7, 10, 1, 0, -150),
            "x2" = c(11, 8, 0, 1, -175),
            "S1" = c(1, 0, 0, 0, 0),
            "S2" = c(0, 1, 0, 0, 0),
            "S3" = c(0, 0, 1, 0, 0),
            "S4" = c(0, 0, 0, 1, 0),
            "Z" = c(0, 0, 0, 0, 1),
            "Solution" = c(77, 80, 9, 6, 0))
  tableau <- matrix(unlist(x), ncol = length(x), byrow = FALSE, dimnames = list(NULL, c(names(x))))
  result <- simplex(tableau, TRUE, FALSE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(1413.881))
})

test_that(desc = "Minimization", code = {
  x <- list("S1" = c(1, 2, -4),
            "S2" = c(7, 6, -20),
            "x1" = c(1, 0, 0),
            "x2" = c(0, 1, 0),
            "Z" = c(0, 0, 1),
            "Solution" = c(14, 20, 0))
  tableau <- matrix(unlist(x), ncol = length(x), byrow = FALSE, dimnames = list(NULL, c(names(x))))
  result <- simplex(tableau, FALSE, FALSE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(47.9958))
})

test_that(desc = "Shipping Costs Minimization", code = {
  x <- list("S1" = c(1, 2, -4),
            "S2" = c(7, 6, -20),
            "x1" = c(1, 0, 0),
            "x2" = c(0, 1, 0),
            "Z" = c(0, 0, 1),
            "Solution" = c(14, 20, 0))
  tableau <- matrix(unlist(x), ncol = length(x), byrow = FALSE, dimnames = list(NULL, c(names(x))))
  result <- simplex(tableau, FALSE, FALSE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(47.9958))
  
  x <- list("Demand" = c(NA, 180, 80, 200, 160, 220),
           "DEN" = c(310, 10, 8, 6, 5, 4),
           "PHO" = c(260, 6, 5, 4, 3, 6),
           "DAL" = c(280, 3, 4, 5, 5, 9))
  data <- matrix(unlist(x), nrow = length(x), byrow = TRUE, dimnames = list(c(names(x)), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC")))
  result <- simplex(ProblemInitialTableau((data)), FALSE, TRUE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(3200))
  
  x <- list("Demand" = c(NA, 100, 100, 100, 100, 100),
            "DEN" = c(200, 6, 6, 7, 8, 9),
            "PHO" = c(200, 6, 7, 8, 9, 10),
            "DAL" = c(200, 3, 5, 7, 11, 13))
  data <- matrix(unlist(x), nrow = length(x), byrow = TRUE, dimnames = list(c(names(x)), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC")))
  result <- simplex(ProblemInitialTableau((data)), FALSE, TRUE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(3300))
  
  x <- list("Demand" = c(NA, 431, 332, 350, 450, 400),
            "DEN" = c(1400, 30, 29, 31, 35, 33),
            "PHO" = c(400, 26, 24, 23, 25, 27),
            "DAL" = c(200, 11, 13, 15, 20, 17))
  data <- matrix(unlist(x), nrow = length(x), byrow = TRUE, dimnames = list(c(names(x)), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC")))
  result <- simplex(ProblemInitialTableau((data)), FALSE, TRUE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(54558))
  
  x <- list("Demand" = c(NA, 20, 20, 20, 20, 20),
            "DEN" = c(100, 5, 5, 5, 5, 5),
            "PHO" = c(100, 5, 5, 5, 5, 5),
            "DAL" = c(100, 5, 5, 5, 5, 5))
  data <- matrix(unlist(x), nrow = length(x), byrow = TRUE, dimnames = list(c(names(x)), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC")))
  result <- simplex(ProblemInitialTableau((data)), FALSE, TRUE)
  expect_that(object = as.numeric(result$opt.val), condition = equals(500))
  
  x <- list("Demand" = c(NA, 20, 25, 90, 60, 70),
            "DEN" = c(50, 30, 29, 31, 35, 33),
            "PHO" = c(50, 26, 24, 23, 25, 27),
            "DAL" = c(50, 11, 13, 15, 20, 17))
  data <- matrix(unlist(x), nrow = length(x), byrow = TRUE, dimnames = list(c(names(x)), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC")))
  result <- simplex(ProblemInitialTableau((data)), FALSE, TRUE)
  expect_that(object = result, condition = equals("No feasible solution"))
})

