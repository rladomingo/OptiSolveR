poly.qsi <- function(data, x) {
  x.values <- data[[1]]
  y.values <- data[[2]]
  
  # Check if the values aren't enough, or if the number of values are not the same
  if (length(x.values) < 3 || length(y.values) < 3 || length(x.values) != length(y.values)) {
    return(NA)
  }
  
  # If the x-values are not in ascending order, sort them
  if (is.unsorted(x.values)) {
    for (i in 1:(length(x.values)-1)) {
      for (j in 1:(length(x.values)-i)) {
        if (x.values[j] > x.values[j+1]) {
          x.values[c(j, j+1)] = x.values[c(j+1, j)]
          y.values[c(j, j+1)] = y.values[c(j+1, j)]
        }
      }
    }
  }
  
  # Check if x is outside the given interval
  if (x < x.values[1] || x > x.values[length(x.values)]) {
    return(NA)
  }
  
  n <- length(x.values) - 1 # n+1 data points, n intervals
  
  # 3n unknowns
  unknowns <- vector()
  for (i in 1:n) {
    unknowns <- c(unknowns, paste(letters[1:3], i, sep = ""))
  }
  
  # 3n equations, thus there are 3n rows and 3n+1 columns
  m <- matrix(nrow = 3*n, ncol = 3*n+1, dimnames = list(NULL, c(unknowns, "RHS")))
  
  rhs.vec <- vector()
  
  # Condition 1: Interior Knots
  in.knots <- vector()
  for (i in 2:n) {
    in.knots <- c(in.knots, x.values[i] ^ 2)
    in.knots <- c(in.knots, x.values[i])
    in.knots <- c(in.knots, 1)
    rhs.vec <- c(rhs.vec, y.values[i])
    rhs.vec <- c(rhs.vec, y.values[i])
  }
  
  # 2n-2 equations generated; Loop goes from 2 to 2n-1 (2n-2-1)
  col.num <- 1
  vec.index <- 1
  for (i in 2:(2*n-1)) {
    if (i %% 2 == 1) { # For rows 3, 5, ..., same set of data as the former row
      m[i, (col.num + 3):(col.num + 5)] <- in.knots[vec.index:(vec.index + 2)]
      col.num <- col.num + 3
      vec.index <- vec.index + 3
    } else { # Else, new set of data to use
      m[i, col.num:(col.num + 2)] <- in.knots[vec.index:(vec.index + 2)]
      
    }
  }
  
  # Condition 2: Exterior knots
  ex.knots <- vector()
  for (i in c(1, n+1)) {
    ex.knots <- c(ex.knots, x.values[i] ^ 2)
    ex.knots <- c(ex.knots, x.values[i])
    ex.knots <- c(ex.knots, 1)
    rhs.vec <- c(rhs.vec, y.values[i])
  }
  
  # Additional two equations generated; Start at the 2n-th (2n-1+1) row to 2n+1
  col.num <- 1
  for (i in (2*n-1+1):(2*n+1)) {
    if (i == 2*n-1+1) {
      m[i, col.num:(col.num + 2)] <- ex.knots[1:3]
    } else {
      col.num <- ncol(m) - 3
      m[i, col.num:(col.num + 2)] <- ex.knots[4:6]
    }
  }
  
  # Condition 3: First derivative of the interior knots must be equal
  in.knots.deriv <- vector()
  for (i in 2:n) {
    in.knots.deriv <- c(in.knots.deriv, x.values[i] * 2)
    in.knots.deriv <- c(in.knots.deriv, 1)
    in.knots.deriv <- c(in.knots.deriv, 0)
    in.knots.deriv <- c(in.knots.deriv, -x.values[i] * 2)
    in.knots.deriv <- c(in.knots.deriv, -1)
    in.knots.deriv <- c(in.knots.deriv, 0)
    rhs.vec <- c(rhs.vec, 0)
  }
  
  # Additional n-1 equations; Start at the 2n+2-th (2n+1+1) row to the second to the last row
  col.num <- 1
  vec.index <- 1
  for (i in (2*n+2):(nrow(m))) {
    m[i, col.num:(col.num + 5)] <- in.knots.deriv[vec.index:(vec.index + 5)]
    col.num <- col.num + 3
    vec.index <- vec.index + 6
  }
  
  # Condition 4: a1 = 0
  m[1, 1] <- 1 # Since 1*a1 = 0
  rhs.vec <- c(0, rhs.vec)
  m[2:nrow(m), 1] <- 0
  
  # Fill in the rest of the matrix
  m[is.na(m)] <- 0
  m[, "RHS"] <- rhs.vec
  
  # result <- QSIGaussianElimination(m)
  result <- QSIGaussJordanElimination(m)
  # print(result)
  
  # If there is no solution set, return NA
  # suppressWarnings({
  if(any(is.na(result$solutionSet))) {
    return(NA)
  }
  # })
  
  # Substitute the values to the original quadratic equation
  fxns <- list()
  sol.set.num <- 1
  for (i in 1:n) {
    fxns <- append(fxns, eval(parse(text = paste(paste("function(x) ", result$solutionSet[sol.set.num], " * x^", 2, sep=""),
                                                 paste(result$solutionSet[sol.set.num + 1], " * x", sep = ""),
                                                 paste(result$solutionSet[sol.set.num + 2], sep = ""), sep = " + "))))
    sol.set.num <- sol.set.num + 3
  }
  
  
  # Find which function to use
  for (i in 1:length(x.values)) {
    if (x >= x.values[i] && x <= x.values[i + 1]) {
      fxn.num <- i
    }
  }
  
  # for (i in 1:length(fxns)) {
  #   f <- unlist(deparse(fxns[[i]]))
  #   cat(paste(f, collapse = ""), "\t", data$x[i], "<= x <=", data$x[i + 1], "\n")
  # }
  
  fxn <- fxns[[fxn.num]]
  retList <- list("qsi.fxns" = fxns, "y" = round(fxn(x), 4))
  
  return(retList)
}

# Modified Gaussian and Gauss-Jordan Elimination functions from Exercise 4
QSIGaussianElimination <- function(a) {
  n <- nrow(a)
  solutionSet <- vector()
  variables <- head(dimnames(a)[[2]], -1) # The variable names are the column names except the RHS
  
  # Forward Elimination
  for (i in 1:(n-1)) {
    # Find pivot row
    for (r in i:n) {
      if (a[r, i] ==  max(abs(a[i:n,i])) || a[r, i] ==  max(abs(a[i:n,i])) * -1) {
        pRow <- r
      }
    }
    
    # No solution exists so we stop and have solutionSet return NA
    if (a[pRow, i] == 0) {
      solutionSet <- NA
      break
    }
    
    # Partial pivoting
    temp <- a[i,]
    a[i,] <- a[pRow,]
    a[pRow, ] <- temp
    
    for (j in (i+1):n) {
      # Pivot element
      pElement <- a[i,i]
      
      # Multiplier
      multiplier <- a[j,i]/pElement
      
      # Elimination
      a[j, ] <- a[j, ] - (a[i, ] * multiplier)
    }
  }
  
  if (length(solutionSet) == 1) { # Length of NA is 1
    retList <- list("solutionSet" = solutionSet, "variables" = variables, "matrix" = a)
  } else {
    # Backward Substitution
    solutionSet[n] <- a[n, "RHS"] / a[n,n]
    for (i in (n-1):1) {
      solutionSet[i] = (a[i,n+1] - sum(a[i, (i+1):n] * solutionSet[(i+1):n])) / a[i,i]
    }
    
    retList <- list("solutionSet" = solutionSet, "variables" = variables, "matrix" = a)
  }
  
  return(retList)
}

QSIGaussJordanElimination <- function(a) {
  n <- nrow(a)
  solutionSet <- vector()
  variables <- head(dimnames(a)[[2]], -1) # The variable names are the column names except the RHS
  
  for (i in 1:n) {
    if (i != n) {
      # Find pivot row
      for (r in i:n) {
        if (a[r, i] ==  max(abs(a[i:n,i])) || a[r, i] ==  max(abs(a[i:n,i])) * -1) {
          pRow <- r
        }
      }
      
      # No solution exists so we stop and have solutionSet return NA
      if (a[pRow, i] == 0) {
        solutionSet <- NA
        break
      }
      
      # Partial pivoting
      temp <- a[i,]
      a[i,] <- a[pRow,]
      a[pRow, ] <- temp
    }
    
    # Normalization
    pElement <- a[i,i]
    a[i, ] <- a[i, ] / pElement
    
    for (j in 1:n) {
      if (i == j) next;
      # Multiplier
      multiplier <- a[j,i]
      
      # Elimination
      a[j, ] <- a[j, ] - (a[i, ] * multiplier)
    }
  }
  
  if (length(solutionSet) == 1) { # Length of NA is 1
    retList <- list("solutionSet" = solutionSet, "variables" = variables, "matrix" = a)
  } else {
    solutionSet <- c(as.vector(a[,"RHS"]))
    retList <- list("solutionSet" = solutionSet, "variables" = variables, "matrix" = a)
  }
  
  return(retList)
}

# For OptiSolveR
GetValues <- function(text) {
  values <- unlist(strsplit(text, "\n")) # Every line, e.g. "-5 7"
  
  points <- vector() # The x- and y-values separated, but with empty strings
  for (i in 1:length(values)) {
    points <- c(points, unlist(strsplit(values[i], " ")))
  }
  
  data.points <- points[points != ""] # The points with the empty strings removed
  
  suppressWarnings({
    if (any(is.na(as.numeric(data.points)))) { # Check if user inputted non-numeric values
      return(NA)
    }
  })
  
  x <- vector()
  y <- vector()
  
  for (i in 1:length(data.points)) {
    ifelse(i %% 2, x <- c(x, as.numeric(data.points[i])), y <- c(y, as.numeric(data.points[i]))) # Every odd index is the x-value
  }
  
  if (length(x) < 3 || length(y) < 3 || length(x) != length(y)) { # Insufficient data given
    return(NA)
  }
  
  retList <- list("x" = x, "y" = y)
  return(retList)
}
