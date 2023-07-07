simplex <- function(tableau, isMax, problem) {
  result <- SimplexGaussJordanElimination(tableau)
  
  # If the Gauss-Jordan function returns NA, then there is no feasible solution
  suppressWarnings({
    if(any(is.na(result))) {
      return("No feasible solution")
    }
  })
  
  
  if (isMax == TRUE) { # Maximization
    # Basic Solution
    basic.solution <- matrix(nrow = 1, ncol = ncol(result) - 1, dimnames = list(NULL, colnames(result)[-length(colnames(result))]))
    for (i in 1:(ncol(result) - 1)) {
      r <- which(result[, i] == 1, arr.ind = TRUE)
      
      if (length(r) == 1) basic.solution[, i] <- result[r, "Solution"]
      else basic.solution[ , i] <- 0
    }
    
    retList <- list("final.tableau" = result, "basic.solution" = basic.solution, "opt.val" = basic.solution[, "Z"])
  } else { # Minimization
    if (problem == TRUE) { # For the shipping problem
      # The number of items shipped is given by the decision variables in the basic solution
      shipping.num <- matrix(data = result[nrow(result), 9:23], nrow = 3, ncol = 5, byrow = TRUE,
                             dimnames = list(c("DEN", "PHO", "DAL"), c("SAC", "SL", "ALB", "CHI", "NYC")))
      
      retList <- list("final.tableau" = result, "basic.solution" = result[nrow(result), ], "opt.val" = result[nrow(result), "Solution"], "shipping.num" = shipping.num)
    } else {
      retList <- list("final.tableau" = result, "basic.solution" = result[nrow(result), ], "opt.val" = result[nrow(result), "Solution"])
    }
  }
  return(retList)
}

# A modified Gauss-Jordan Elimination function from Exercise 4
SimplexGaussJordanElimination <- function(a) {
  # While there are still negative numbers in the bottom row excluding the solution column
  while (!all(a[nrow(a), 1:(ncol(a) - 1)] >= 0)) {
    # Find pivot column
    p.col <- 0
    for (i in 1:(ncol(a) - 1)) { # For every negative number in the bottom row excluding the solution column
      if (a[nrow(a), i] < 0) {
        if (p.col == 0 || abs(a[nrow(a), i]) > abs(a[nrow(a), p.col])) {
          p.col <- i
        }
      }
    }
    
    # Get test ratios
    test.ratios <- vector()
    for (i in 1:(nrow(a) - 1)) { # For every row excluding the last row
      test.ratios <- c(test.ratios, round(a[i, ncol(a)] / a[i, p.col], 4)) # TR is given by solution column/pivot column
    }
    
    # Find pivot row
    p.row <- 0
    for (i in 1:length(test.ratios)) {
      if (test.ratios[i] > 0 && is.finite(test.ratios[i])) {
        if (p.row == 0 || test.ratios[i] < test.ratios[p.row]) {
          p.row <- i
        }
      }
    }
    
    # Pivot element
    p.ele <- a[p.row, p.col]
    
    if (length(p.ele) == 0) return(NA) # If there is no pivot element, meaning that the test ratios don't have a positive number, return NA
    
    # Normalization
    a[p.row, ] <- round(a[p.row, ] / p.ele, 4)
    
    # Elimination
    for (i in 1:nrow(a)) {
      if (i == p.row) next;
      
      c <- a[i, p.col] # Multiplier (element to eliminate)
      a[i, ] <- round(a[i, ] - (a[p.row, ] * c), 4)
    }
  }
  
  return(a)
}

# To create the initial tableau for the shipping problem
ProblemInitialTableau <- function(data) {
  # To get the solution column
  sol.v <- vector()
  for (i in 2:nrow(data)) {
    sol.v <- c(sol.v, data[i, -1])
  }
  
  m <- matrix(nrow = 9, ncol = 16)
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      supply.constraints <- (i == 1 && j >= 1 && j <= 5) ||
        (i == 2 && j >= 6 && j <= 10) ||
        (i == 3 && j >= 11 && j <= 15)
      
      demand.constraints <- (i == 4 && j %in% c(1, 6, 11)) ||
        (i == 5 && j %in% c(2, 7, 12)) ||
        (i == 6 && j %in% c(3, 8, 13)) ||
        (i == 7 && j %in% c(4, 9, 14)) ||
        (i == 8 && j %in% c(5, 10, 15))
      
      if (supply.constraints) m[i, j] <- -1
      else if (demand.constraints || (i == 9 && j == 16)) m[i, j] <- 1
      else if (i == 9) m[i, 1:(ncol(m) - 1)] <- sol.v
      else if (j == 16) ifelse(i %in% c(1:3), m[i, j] <- data[i + 1, 1], m[i, j] <- data[1, i-2] * -1)
      else m[i, j] <- 0
    }
  }
  
  # Transpose the matrix
  m <- t(m)
  
  # Set up the initial tableau
  initial.tableau <- matrix(nrow = nrow(m), ncol = 25, dimnames = list(NULL, c(paste("S", 1:8, sep = ""), paste("x", 1:15, sep = ""), "Z", "Solution")))
  initial.tableau[1:15, 1:8] <- m[1:15, 1:8]
  initial.tableau[, 1:8] <- m[, 1:8]
  initial.tableau[16, "Z"] <- m[16, 9]
  initial.tableau[, "Solution"] <- m[, 9]
  initial.tableau[16, "Solution"] <- 0
  
  for (i in 1:16) {
    for (j in 9:24) {
      initial.tableau[i, j] <- ifelse(grepl(paste("\\bx", i, "\\b", sep = ""), colnames(initial.tableau)[j]) || (i == 16 && j == 24), 1, 0)
    }
  }
  
  return(initial.tableau)
}

# To create the initial tableau for the generic simplex solver in OptiSolveR
GenericInitialTableau <- function(r, c, z.coeffs, z.rhs, const.coeffs, const.rhs, mode) {
  var.num <- c - 2
  const.num <- r - 1
  
  if (mode == "max") {
    m <- matrix(
      nrow = const.num,
      ncol = var.num,
      data = const.coeffs,
      byrow = FALSE,
      dimnames = list(NULL, c(paste("x", 1:var.num, sep = "")))
    )
    
    slack <- matrix(
      nrow = const.num,
      ncol = const.num,
      dimnames = list(NULL, c(paste("S", 1:const.num, sep = "")))
    )
    
    for (i in 1:nrow(slack)) {
      for (j in 1:ncol(slack)) {
        slack[i, j] <- ifelse(i == j, 1, 0)
      }
    }
    
    z.col <- matrix(data = 0, nrow = const.num, ncol = 1, dimnames = list(NULL, "Z"))
    sol.col <- matrix(data = const.rhs, nrow = const.num, ncol = 1, dimnames = list(NULL, "Solution"))
    
    initial.tableau <- cbind(m, slack, z.col, sol.col)
    
    z.row <- matrix(nrow = 1, ncol = ncol(initial.tableau))
    z.row[1, 1:var.num] <- z.coeffs * -1
    z.row[1, (var.num + 1):(ncol(z.row) - 2)] <- 0
    z.row[1, ncol(z.row) - 1] <- z.rhs
    z.row[1, ncol(z.row)] <- 0
    
    initial.tableau <- rbind(initial.tableau, z.row)
  } else {
    m <- matrix(
      data = c(const.coeffs, const.rhs),
      nrow = const.num,
      ncol = var.num + 1,
      byrow = FALSE
    )
    
    z.row <- matrix(data = c(z.coeffs, z.rhs), nrow = 1, ncol = ncol(m))
    
    m <- rbind(m, z.row)
    
    t.m <- t(m)
    
    initial.tableau <- t.m[ ,-ncol(t.m)]
    colnames(initial.tableau) <- c(paste("S", 1:ncol(initial.tableau), sep = ""))
    initial.tableau[nrow(initial.tableau), ] <-  initial.tableau[nrow(initial.tableau), ] * -1
    
    xz.cols <- matrix(
      nrow = nrow(initial.tableau),
      ncol = nrow(initial.tableau),
      dimnames = list(NULL, c(paste("x", 1:(nrow(initial.tableau) - 1), sep = ""), "Z"))
    )
    for (i in 1:nrow(xz.cols)) {
      for (j in 1:ncol(xz.cols)) {
        xz.cols[i, j] <- ifelse(i == j, 1, 0)
      }
    }
    
    sol.col <- matrix(data = t.m[, ncol(t.m)], nrow = nrow(initial.tableau), ncol = 1, dimnames = list(NULL, "Solution"))
    sol.col[nrow(sol.col), 1] <- 0
    
    initial.tableau <- cbind(initial.tableau, xz.cols, sol.col)
  }
  
  return(initial.tableau)
}

