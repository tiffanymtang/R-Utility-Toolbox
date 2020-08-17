# functions to reduce number of features in data before modeling

library(tidyverse)
library(glmnet)
library(ranger)

filterByAssoc <- function(X, y, max.pval, max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # highest univariate association with y (measured via anova)
  # 
  # inputs: (must specify either max.pval or max.p)
  # - X = n x p data matrix or data frame
  # - y = n x 1 response vector
  # - max.pval = anova pvalue threshold
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  pvals <- apply(X, 2, FUN = function(x) {
    aov_fit <- aov(x ~ y, data = data.frame(x = x, y = y))
    return(summary(aov_fit)[[1]]$`Pr(>F)`[1])
  })
  
  if (!missing(max.pval)) {
    fdat <- X[, pvals <= max.pval]
  } else if (!missing(max.p)) {
    pval_cutoff <- sort(pvals, decreasing = F)[max.p]
    fdat <- X[, pvals <= pval_cutoff]
  } else {
    stop("Must input either max.pval or max.p.")
  }
  
  return(fdat)
}


filterByCor <- function(X, y, method = "pearson", min.cor, max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # highest correlation with y
  # 
  # inputs: (must specify either max.pval or max.p)
  # - X = n x p data matrix or data frame
  # - y = n x 1 response vector
  # - method = character string indicating type of correlation coefficient; one
  #   of "pearson" (default), "kendall", or "spearman"
  # - min.cor = minimum threshold for correlation
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  cors <- apply(X, 2, cor, 
                y = y, method = method, use = "pairwise.complete.obs")
  
  if (!missing(min.cor)) {
    fdat <- X[, cors >= min.cor]
  } else if (!missing(max.p)) {
    cor_cutoff <- sort(cors, decreasing = T)[max.p]
    fdat <- X[, cors >= cor_cutoff]
  } else {
    stop("Must input either min.cor or max.p.")
  }
  
  return(fdat)
}


filterByCorBootstrap <- function(X, y, 
                                 method = "pearson", min.cor, max.p, B = 100) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # highest correlation with y, averaged across multiple bootstrap samples
  # 
  # inputs: (must specify either max.pval or max.p)
  # - X = n x p data matrix or data frame
  # - y = n x 1 response vector
  # - method = character string indicating type of correlation coefficient; one
  #   of "pearson" (default), "kendall", or "spearman"
  # - min.cor = minimum threshold for correlation
  # - max.p = maximum number of features to keep
  # - B = number of bootstrap samples
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  n <- nrow(X)
  boot_cors <- replicate(B, {
    bs_idx <- sample(1:n, n, replace = T)
    X_bs <- X[bs_idx, ]
    y_bs <- y[bs_idx]
    return(apply(X_bs, 2, cor, 
                 y = y_bs, method = method, use = "pairwise.complete.obs"))
  })
  
  boot_cors[is.na(boot_cors)] <- 0
  cors <- rowMeans(boot_cors)
  
  if (!missing(min.cor)) {
    fdat <- X[, cors >= min.cor]
  } else if (!missing(max.p)) {
    cor_cutoff <- sort(cors, decreasing = T)[max.p]
    fdat <- X[, cors >= cor_cutoff]
  } else {
    stop("Must input either min.cor or max.p.")
  }
  
  return(fdat)
}


filterByLasso <- function(X, y, family = "gaussian", max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # highest feature importance determined by magnitude of Lasso coefficient
  # 
  # inputs:
  # - X = n x p data matrix or data frame
  # - y = n x 1 response vector
  # - family = name of glm family for glmnet fit
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  # get default lambda grid to search over
  lasso_fit <- glmnet(x = as.matrix(X), y = y, family = family, alpha = 1)
  lambdas <- lasso_fit$lambda
  
  ptr <- T
  while (ptr) {  # search over regularization path
    lasso_fit <- glmnet(x = as.matrix(X), y = y, family = family, alpha = 1,
                        lambda = lambdas)
    if (length(which(lasso_fit$df > max.p)) >= 1) {
      idx <- which(lasso_fit$df > max.p)[1]
      betas <- abs(lasso_fit$beta[, idx])
      beta_cutoff <- sort(betas, decreasing = T)[max.p]
      fdat <- X[, betas >= beta_cutoff]
      break
    } else {  # try different lambda grid
      lambdas <- lasso_fit$lambda * 1e-1
    }
  }
  
  return(fdat)
}


filterByRF <- function(X, y, 
                       mtry = round(ifelse(is.factor(y), 
                                           sqrt(ncol(X)), ncol(X) / 3)),
                       ntrees = 500, max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # highest feature importance determined by RF
  # 
  # inputs:
  # - X = n x p data matrix or data frame
  # - y = n x 1 response vector
  # - mtry = number of variables to possibly split at in each node; default is 
  #   sqrt(p) if y is a factor and p / 3 otherwise
  # - ntrees = number of trees in rf fit
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  rf_df <- data.frame(y = y, X)
  rf_fit <- ranger(formula = y ~., 
                   data = rf_df,
                   num.trees = ntrees,
                   mtry = mtry,
                   importance = "impurity", 
                   num.threads = 1)
  
  imp <- rf_fit$variable.importance
  imp_cutoff <- sort(imp, decreasing = T)[max.p]
  fdat <- X[, imp >= imp_cutoff]
  
  return(fdat)
}


filterByVar <- function(X, min.var, max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # largest variance
  # 
  # inputs:
  # - X = n x p data matrix or data frame
  # - min.var = minimum threshold for variance
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  vars <- apply(X, 2, var, na.rm = T)
  
  if (!missing(min.var)) {
    fdat <- X[, vars >= min.var]
  } else if (!missing(max.p)) {
    var_cutoff <- sort(vars, decreasing = T)[max.p]
    fdat <- X[, vars >= var_cutoff]
  } else {
    stop("Must input either min.var or max.p.")
  }
  
  return(fdat)
}
