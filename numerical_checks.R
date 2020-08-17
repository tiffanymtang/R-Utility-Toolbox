# functions to complete common numerical checks

library(tidyverse)

isEqual <- function(A, B, tol = 1e-7) {
  ####### Function Description ########
  # function to check for equality between two matrices A and B up to given
  # numerical tolerance
  # 
  # inputs:
  # - A = matrix
  # - B = matrix
  # - tol = numerical tolerance
  #
  # output: displays message (either "Equal" or "Not Equal")
  ####### 
  
  if (max(abs(A - B)) > tol) {
    cat("Not Equal.\n")
  } else {
    cat("Equal.\n")
  }
}


isOrthonormal <- function(A, verbose = FALSE, tol = 1e-7) {
  ####### Function Description ########
  # function to check if matrix is orthonormal up to given numerical tolerance
  # 
  # inputs:
  # - A = matrix
  # - verbose = binary; level of written output
  # - tol = numerical tolerance
  #
  # output: displays messages
  ####### 
  
  if (nrow(A) != ncol(A)) {
    stop("Matrix is not square.")
  }
  
  n <- nrow(A)
  
  # check AA'
  A_tA <- A %*% t(A)
  A_tA_off <- A_tA
  diag(A_tA_off) <- 0
  if (verbose) {
    cat("Max diagonal of AA' is", max(diag(A_tA)), "\n")
    cat("Min diagonal of AA' is", min(diag(A_tA)), "\n")
    cat("Max off-diagonal of AA' is", max(A_tA_off), "\n")
    cat("Min off-diagonal of AA' is", min(A_tA_off), "\n")
  }
  if ((max(abs(A_tA_off)) > tol) | (max(abs(diag(A_tA) - rep(1, n))) > tol)) {
    cat("AA' != Id. \n")
  }
  
  # check A'A
  tA_A <- t(A) %*% A
  tA_A_off <- tA_A
  diag(tA_A_off) <- 0
  if (verbose) {
    cat("Max diagonal of A'A is", max(diag(tA_A)), "\n")
    cat("Min diagonal of A'A is", min(diag(tA_A)), "\n")
    cat("Max off-diagonal of A'A is", max(tA_A_off), "\n")
    cat("Min off-diagonal of A'A is", min(tA_A_off), "\n")
  }
  if ((max(abs(tA_A_off)) > tol) | (max(abs(diag(tA_A) - rep(1, n))) > tol)) {
    cat("A'A != Id. \n")
  }
}