# functions to preprocess and clean data before modeling

library(tidyverse)


basicDataSummary <- function(X) {
  ####### Function Description ########
  # function to summarize basic data oddities (e.g., NAs, constant columns,
  # empty factors)
  # 
  # inputs:
  # - X = data matrix or data frame
  #   
  # output: NULL
  ####### 
  
  cat("Finding NAs in data...\n")
  
  col_nas <- apply(X, 2, function(x) sum(is.na(x)))
  if (all(col_nas == 0)) {
    cat("No NAs in data!\n")
  } else {
    print(table(col_nas, dnn = "Frequency Table: #NAs per Column"))
    cat("Columns with NAs: \n")
    if (!is.null(names(col_nas))) {
      cat(names(col_nas)[col_nas > 0], sep = "\n")
    } else {
      cat(which(col_nas > 0), sep = "\n")
    }
  }
  
  cat("Types of features in data...\n")
  col_classes <- sapply(X, class)
  print(table(col_classes, dnn = "Frequency Table: Types of Features"))
  
  if (any(col_classes == "factor")) {
    missing_levels <- sapply(X, nlevels)[col_classes == "factor"] != 
      sapply(X, function(x) length(unique(x)))[col_classes == "factor"]
    if (any(missing_levels)) {
      cat("There are missing factor levels in", 
          paste(names(which(missing_levels)), sep = ", "),
          "\n")
    } else {
      cat("There are no missing factor levels in the data.\n")
    }
  }
  
  cat("Looking at column variances...\n")
  if (any(col_classes == "numeric")) {
    if (sum(col_classes == "numeric") == 1) {
      col_vars <- as.data.frame(X) %>%
        select(names(which(col_classes == "numeric"))) %>%
        apply(., 2, var, na.rm = T)
    } else {
      col_vars <- apply(X[, col_classes == "numeric"], 2, var, na.rm = T)
    }
    
    cat("Constant columns: \n")
    col_const <- col_vars == 0
    if (any(col_const)) {
      if (!is.null(names(col_vars))) {
        cat(paste(names(col_vars)[col_const], X[1, col_const],
                  sep = " with value "), sep = "\n")
      } else {
        cat(paste(which(col_const), X[1, col_const],
                  sep = " with value "), sep = "\n")
      }
    } else {
      cat("There are no constant columns in the data.")
    }
    hist(col_vars, main = "Histogram of Column Variances", xlab = "Variance")
  }
}


removeNACols <- function(X, verbose = 0) {
  ####### Function Description ########
  # function to remove all columns with at least one NA value
  # 
  # inputs:
  # - X = data matrix or data frame
  # - verbose = integer (0-2); level of written output
  #   
  # output: cleaned data matrix or frame without NAs
  ####### 
  
  col_nas <- apply(X, 2, function(x) sum(is.na(x)))
  
  if (verbose >= 1) {
    print(table(col_nas, dnn = "Frequency Table: #NAs per Column"))
  }
  if (verbose >= 2) {
    cat("Columns with NAs: \n")
    if (!is.null(names(col_nas))) {
      cat(names(col_nas)[col_nas > 0], sep = "\n")
    } else {
      cat(which(col_nas > 0), sep = "\n")
    }
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(col_nas > 0), "features\n"))
  }
  
  X_cleaned <- X[, col_nas == 0]
  
  return(X_cleaned)
}


removeConstantCols <- function(X, verbose = 0) {
  ####### Function Description ########
  # function to remove all columns that are constant (i.e., have 0 variance)
  # 
  # inputs:
  # - X = data matrix or data frame
  # - verbose = integer (0-2); level of written output
  #   
  # output: cleaned data matrix or frame without constant columns
  #######
  
  col_vars <- apply(X, 2, var, na.rm = T)
  
  if (verbose >= 2) {
    cat("Constant columns: \n")
    if (!is.null(names(col_vars))) {
      cat(paste(names(col_vars)[col_vars == 0], X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    } else {
      cat(paste(which(col_vars == 0), X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    }
    hist(col_vars, main = "Histogram of Column Variances", xlab = "Variance")
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(col_vars == 0), "features"))
  }
  
  X_cleaned <- X[, col_vars > 0]

  return(X_cleaned)
}
