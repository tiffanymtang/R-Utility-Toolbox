# script with functions to fit various models given (X, y) data
# 
# only handles continuous and binary responses for now

library(tidyverse)
library(glmnet)
library(ranger)
library(caret)
library(xgboost)
library(e1071)

fitLM <- function(X, y, Xts = NULL, p_cut = 0.05) {
  #### Function Description ####
  # fit (generalized) linear model
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame 
  #   - p_cut = p-value cutoff for determining "support"
  # 
  # returns: list of 5
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = model fit; output of lm() or glm()
  #   - imp = importance df, as measured by magnitude of t value
  #   - sest = estimated support
  ##############
  
  # check if binary
  binary <- FALSE
  if (is.factor(y)) {
    if (nlevels(y) == 2) {
      binary <- TRUE
    } else {
      stop("Multi-level factor responses has not been implemented.")
    }
  }
  
  # fit model
  tr_data <- cbind(as.data.frame(X), y = y)
  if (binary) {
    fit <- glm(y ~ ., data = tr_data, family = "binomial")
  } else {
    fit <- lm(y ~ ., data = tr_data)
  }
  
  # make predictions
  yhat_tr <- predict(fit, as.data.frame(X), type = "response")
  if (!is.null(Xts)) {
    yhat_ts <- predict(fit, as.data.frame(Xts), type = "response")
  } else {
    yhat_ts <- NULL
  }
  
  # get support
  imp <- as.data.frame(abs(summary(fit)$coefficients[-1, 3])) %>%  # t value
    setNames("imp") %>%
    rownames_to_column("var")
  sest <- which(summary(fit)$coefficients[-1, 4] < p_cut)
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts,
              fit = fit, imp = imp, sest = sest))
}

fitLasso <- function(X, y, Xts = NULL, nfolds = 10, foldid = NULL,
                     lambda = NULL, cv_measure = "deviance", ...) {
  #### Function Description ####
  # fit Lasso
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame
  #   - nfolds = number of folds for CV to choose optimal lambda
  #   - foldid = fold ids for CV; optional
  #   - lambda = lambda for cv.glmnet()
  #   - cv_measure = loss to use for CV; see cv.glmnet()
  #   - ... = other arguments to feed into glmnet()
  # 
  # returns: list of 7
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = lasso model fit; output of glmnet()
  #   - cv_fit = lasso model cv fit; output of cv.glmnet()
  #   - best_lam = best lambda selected by cv
  #   - imp = importance df, as measured by magnitude of lasso coefficient
  #   - sest = estimated support
  ##############
  
  # check if binary
  binary <- FALSE
  if (is.factor(y)) {
    if (nlevels(y) == 2) {
      binary <- TRUE
    } else {
      stop("Multi-level factor responses for Lasso has not been implemented.")
    }
  }
  
  if (binary) {
    family <- "binomial"
    response <- "response"
  } else {
    family <- "gaussian"
    response <- "link"
  }
  
  # create model matrix (to deal with categorical features)
  X_all <- model.matrix(~., as.data.frame(rbind(X, Xts)))[, -1]  # rm intercept
  X <- X_all[1:nrow(X), ]
  if (!is.null(Xts)) {
    Xts <- X_all[(nrow(X) + 1):nrow(X_all), ]
  }
  
  # fit model
  cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1,
                      family = family, type.measure = cv_measure,
                      nfolds = nfolds, foldid = foldid, lambda = lambda, ...)
  fit <- glmnet(x = as.matrix(X), y = y, alpha = 1, 
                lambda = cv_fit$lambda.min, family = family, ...)
  cat(paste0("Best lambda: ", cv_fit$lambda.min, "\n"))
  
  # make predictions
  yhat_tr <- c(predict(fit, as.matrix(X), type = response))
  if (!is.null(Xts)) {
    yhat_ts <- c(predict(fit, as.matrix(Xts), type = response))
  } else {
    yhat_ts <- NULL
  }
  
  # get support
  imp <- as.data.frame(as.matrix(abs(fit$beta))) %>%
    setNames("imp") %>%
    rownames_to_column("var")
  sest <- which(imp$imp > 0)
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts,
              fit = fit, cv_fit = cv_fit, best_lam = cv_fit$lambda.min, 
              imp = imp, sest = sest))
}

fitRidge <- function(X, y, Xts = NULL, nfolds = 10, foldid = NULL, 
                     lambda = NULL, cv_measure = "deviance", ...) {
  #### Function Description ####
  # fit ridge
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame
  #   - nfolds = number of folds for CV to choose optimal lambda
  #   - foldid = fold ids for CV; optional
  #   - lambda = lambda for cv.glmnet()
  #   - cv_measure = loss to use for CV; see cv.glmnet()
  #   - ... = other arguments to feed into glmnet()
  # 
  # returns: list of 7
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = ridge model fit; output of glmnet()
  #   - cv_fit = ridge model cv fit; output of cv.glmnet()
  #   - best_lam = best lambda selected by cv
  #   - imp = importance df, as measured by magnitude of ridge coefficient
  #   - sest = estimated support
  ##############
  
  # check if binary
  binary <- FALSE
  if (is.factor(y)) {
    if (nlevels(y) == 2) {
      binary <- TRUE
    } else {
      stop("Multi-level factor responses for Lasso has not been implemented.")
    }
  }
  
  if (binary) {
    family <- "binomial"
    response <- "response"
  } else {
    family <- "gaussian"
    response <- "link"
  }
  
  # create model matrix (to deal with categorical features)
  X_all <- model.matrix(~., as.data.frame(rbind(X, Xts)))[, -1]  # rm intercept
  X <- X_all[1:nrow(X), ]
  if (!is.null(Xts)) {
    Xts <- X_all[(nrow(X) + 1):nrow(X_all), ]
  }
  
  # fit model
  cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0,
                      family = family, type.measure = cv_measure,
                      nfolds = nfolds, foldid = foldid, lambda = lambda, ...)
  fit <- glmnet(x = as.matrix(X), y = y, alpha = 0, 
                lambda = cv_fit$lambda.min, family = family, ...)
  cat(paste0("Best lambda: ", cv_fit$lambda.min, "\n"))
  
  # make predictions
  yhat_tr <- c(predict(fit, as.matrix(X), type = response))
  if (!is.null(Xts)) {
    yhat_ts <- c(predict(fit, as.matrix(Xts), type = response))
  } else {
    yhat_ts <- NULL
  }
  
  # get support
  imp <- as.data.frame(as.matrix(abs(fit$beta))) %>%
    setNames("imp") %>%
    rownames_to_column("var")
  sest <- which(imp$imp > mean(imp$imp))
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts,
              fit = fit, cv_fit = cv_fit, best_lam = cv_fit$lambda.min, 
              imp = imp, sest = sest))
}

fitElnet <- function(X, y, Xts = NULL, alpha = .5, nfolds = 10, foldid = NULL, 
                     lambda = NULL, cv_measure = "deviance", ...) {
  #### Function Description ####
  # fit elastic net
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame
  #   - alpha = elastic net tuning parameter
  #   - nfolds = number of folds for CV to choose optimal lambda
  #   - foldid = fold ids for CV; optional
  #   - lambda = lambda for cv.glmnet()
  #   - cv_measure = loss to use for CV; see cv.glmnet()
  #   - ... = other arguments to feed into glmnet()
  # 
  # returns: list of 7
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = elastic net model fit; output of glmnet()
  #   - cv_fit = elastic net model cv fit; output of cv.glmnet()
  #   - best_lam = best lambda selected by cv
  #   - imp = importance df, as measured by magnitude of ridge coefficient
  #   - sest = estimated support
  ##############
  
  # check if binary
  binary <- FALSE
  if (is.factor(y)) {
    if (nlevels(y) == 2) {
      binary <- TRUE
    } else {
      stop("Multi-level factor responses for Lasso has not been implemented.")
    }
  }
  
  if (binary) {
    family <- "binomial"
    response <- "response"
  } else {
    family <- "gaussian"
    response <- "link"
  }
  
  # create model matrix (to deal with categorical features)
  X_all <- model.matrix(~., as.data.frame(rbind(X, Xts)))[, -1]  # rm intercept
  X <- X_all[1:nrow(X), ]
  if (!is.null(Xts)) {
    Xts <- X_all[(nrow(X) + 1):nrow(X_all), ]
  }
  
  # fit model
  cv_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = alpha,
                      family = family, type.measure = cv_measure,
                      nfolds = nfolds, foldid = foldid, lambda = lambda, ...)
  fit <- glmnet(x = as.matrix(X), y = y, alpha = alpha, 
                lambda = cv_fit$lambda.min, family = family, ...)
  cat(paste0("Best lambda: ", cv_fit$lambda.min, "\n"))
  
  # make predictions
  yhat_tr <- c(predict(fit, as.matrix(X), type = response))
  if (!is.null(Xts)) {
    yhat_ts <- c(predict(fit, as.matrix(Xts), type = response))
  } else {
    yhat_ts <- NULL
  }
  
  # get support
  imp <- as.data.frame(as.matrix(abs(fit$beta))) %>%
    setNames("imp") %>%
    rownames_to_column("var")
  sest <- which(imp$imp > 0)
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts,
              fit = fit, cv_fit = cv_fit, best_lam = cv_fit$lambda.min, 
              imp = imp, sest = sest))
}

fitRF <- function(X, y, Xts = NULL, nfolds = 10, foldid = NULL, caret = FALSE,
                  tune_grid = NULL, caret_params = NULL, ...) {
  #### Function Description ####
  # fit RF via caret and ranger
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame
  #   - nfolds = number of folds for CV
  #   - foldid = fold ids for CV; optional
  #   - caret = logical; whether or not to tune mtry with caret
  #   - tune_grid = grid of tuning parameters to search over; used only if
  #     caret = T
  #   - caret_params = list of named arguments for training caret; used
  #     only if caret = T; possible named arguments are summaryFunction, 
  #     classProbs, split_rule, min_node_size, metric, response
  #   - ... = other arguments to feed into ranger()
  # 
  # returns: list of 4
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = rf model fit; output of ranger()
  #   - imp = importance df, as measured by RF impurity score
  #   - sest = estimated support, defined as impurity > mean(impurity)
  ##############
  
  if (!caret) {
    df <- as.data.frame(X) %>%
      cbind(y = y)
    
    if (is.factor(y)) {
      mtry <- round(sqrt(ncol(X)))
    } else {
      mtry <- round(ncol(X) / 3)
    }
    
    # fit model
    fit <- ranger(data = df,
                  dependent.variable.name = "y",
                  importance = "impurity",
                  mtry = mtry,
                  num.threads = 1,
                  ...)
    
    # make predictions
    yhat_ts <- NULL
    if (is.factor(y)) {
      yhat_tr <- predict(fit, as.data.frame(X), predict.all = TRUE,
                         num.threads = 1)$predictions
      yhat_tr <- rowMeans(yhat_tr) - 1
      if (!is.null(Xts)) {
        yhat_ts <- predict(fit, as.data.frame(Xts), predict.all = TRUE,
                           num.threads = 1)$predictions
        yhat_ts <- rowMeans(yhat_ts) - 1
      }
    } else {
      yhat_tr <- predict(fit, as.data.frame(X), num.threads = 1)$predictions
      if (!is.null(Xts)) {
        yhat_ts <- predict(fit, as.data.frame(Xts), num.threads = 1)$predictions
      }
    }
    
    # get support
    imp <- as.data.frame(fit$variable.importance) %>%
      setNames("imp") %>%
      rownames_to_column("var")
    sest <- which(imp$imp >= mean(imp$imp))
    
  } else {
    if (!is.null(foldid)) {  # split by study
      foldid_ls <- lapply(unique(foldid), FUN = function(i) {
        return(c(1:nrow(X))[foldid != i])
      })
    } else {  # do normal Cv
      foldid_ls <- NULL
    }
    
    # default train control parameters
    if (is.factor(y)) {
      summaryFunction <- defaultSummary
      classProbs <- TRUE
      split_rule <- "gini"
      min_node_size <- 1
      metric <- "Accuracy"
      response <- "prob"
    } else {
      summaryFunction <- defaultSummary
      classProbs <- FALSE
      split_rule <- "variance"
      min_node_size <- 5
      metric <- "RMSE"
      response <- "raw"
    }
    
    if (!is.null(caret_params)) {
      if (!is.list(caret_params) | 
          is.null(names(caret_params))) {
        stop("caret_params must be a named list of arguments.")
      } else {
        if ("summaryFunction" %in% names(caret_params)) {
          summaryFunction <- caret_params$summaryFunction
        }
        if ("classProbs" %in% names(caret_params)) {
          classProbs <- caret_params$classProbs
        }
        if ("split_rule" %in% names(caret_params)) {
          split_rule <- caret_params$split_rule
        }
        if ("min_node_size" %in% names(caret_params)) {
          min_node_size <- caret_params$min_node_size
        }
        if ("metric" %in% names(caret_params)) {
          metric <- caret_params$metric
        }
        if ("response" %in% names(caret_params)) {
          response <- caret_params$response
        }
      }
    }
    
    # how to do splitting
    trcontrol <- trainControl(
      method = "cv",
      number = nfolds,
      index = foldid_ls,
      classProbs = classProbs,
      summaryFunction = summaryFunction,
      allowParallel = FALSE,
      verboseIter = FALSE
    )
    
    # tuning grid
    if (is.null(tune_grid)) {
      tune_grid <- expand.grid(mtry = seq(sqrt(ncol(X)), ncol(X) / 3,
                                          length.out = 3),
                               splitrule = split_rule,
                               min.node.size = min_node_size)
    }
    
    # fit model
    fit <- train(x = as.data.frame(X), y = y,
                 trControl = trcontrol,
                 tuneGrid = tune_grid,
                 method = "ranger",
                 metric = metric,
                 importance = "impurity",
                 num.threads = 1, ...)
    cat("Best rf hyperparameters:\n")
    print(fit$bestTune)
    
    # make predictions
    yhat_tr <- predict(fit, as.data.frame(X), type = response,
                       num.threads = 1)
    if (!is.null(Xts)) {
      yhat_ts <- predict(fit, as.data.frame(Xts), type = response,
                         num.threads = 1)
    } else {
      yhat_ts <- NULL
    }
    if (is.factor(y)) {
      yhat_tr <- c(yhat_tr[, 2])
      if (!is.null(Xts)) {
        yhat_ts <- c(yhat_ts[, 2])
      }
    }
    
    # get support
    imp <- as.data.frame(fit$finalModel$variable.importance) %>%
      setNames("imp") %>%
      rownames_to_column("var")
    sest <- which(imp$imp >= mean(imp$imp))
  }
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts, fit = fit, 
              imp = imp, sest = sest))
}

fitXGB <- function(X, y, Xts = NULL, nfolds = 10, foldid = NULL,
                   tune_grid = NULL, caret_params = NULL, ...) {
  #### Function Description ####
  # fit xgboost via caret() and xgboost()
  # 
  # input:
  #   - X = training data matrix or data frame
  #   - y = response vector
  #   - Xts = (optional) test data matrix or test data frame
  #   - nfolds = number of folds for CV
  #   - foldid = fold ids for CV; optional
  #   - tune_grid = grid of tuning parameters to search over
  #   - caret_params = list of named arguments for training caret; possible
  #     named arguments are summaryFunction, classProbs, metric, response
  #   - ... = other arguments to feed into xgboost()
  # 
  # returns: list of 4
  #   - yhat_tr = vector of predicted responses using training data
  #   - yhat_ts = vector of predicted responses using test data
  #   - fit = xgb model fit; output of ranger()
  #   - imp = importance df, as measured by xgb gain score (see xgb.importance)
  #   - sest = estimated support, defined as gain > mean(gain)
  ##############
  
  if (!is.null(foldid)) {  # split by study
    foldid_ls <- lapply(unique(foldid), FUN = function(i) {
      return(c(1:nrow(X))[foldid != i])
    })
  } else {  # do normal CV
    foldid_ls <- NULL
  }
  
  # default train control parameters
  if (is.factor(y)) {
    summaryFunction <- defaultSummary
    classProbs <- TRUE
    metric <- "Accuracy"
    response <- "prob"
    if (nlevels(y) == 2) {
      objective <- "binary:logistic"
    } else {
      objective <- "multi:softmax"
    }
  } else {
    summaryFunction <- defaultSummary
    classProbs <- FALSE
    metric <- "RMSE"
    response <- "raw"
    objective <- "reg:squarederror"
  }
  
  if (!is.null(caret_params)) {
    if (!is.list(caret_params) | is.null(names(caret_params))) {
      stop("caret_params must be a named list of arguments.")
    } else {
      if ("summaryFunction" %in% names(caret_params)) {
        summaryFunction <- caret_params$summaryFunction
      }
      if ("classProbs" %in% names(caret_params)) {
        classProbs <- caret_params$classProbs
      }
      if ("metric" %in% names(caret_params)) {
        metric <- caret_params$metric
      }
      if ("response" %in% names(caret_params)) {
        response <- caret_params$response
      }
    }
  }
  
  # how to do splitting
  trcontrol <- trainControl(
    method = "cv",
    number = nfolds,
    index = foldid_ls,
    classProbs = classProbs,
    summaryFunction = summaryFunction,
    allowParallel = FALSE,
    verboseIter = FALSE
  )
  
  # tuning grid
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(nrounds = c(10, 25, 50, 100, 150),
                             max_depth = c(3, 6),
                             colsample_bytree = 0.33,
                             eta = c(0.1, 0.3),
                             gamma = 0,
                             min_child_weight = 1,
                             subsample = 0.6)
  }
  
  # create model matrix (to deal with categorical features)
  X_all <- model.matrix(~., as.data.frame(rbind(X, Xts)))[, -1]  # rm intercept
  X <- X_all[1:nrow(X), ]
  if (!is.null(Xts)) {
    Xts <- X_all[(nrow(X) + 1):nrow(X_all), ]
  }
  
  # fit model
  fit <- train(x = as.data.frame(X), y = y,
               trControl = trcontrol,
               tuneGrid = tune_grid,
               method = "xgbTree",
               metric = metric,
               objective = objective,
               nthread = 1, ...)
  cat("Best xgb hyperparameters:\n")
  print(fit$bestTune)
  
  # make predictions
  yhat_tr <- predict(fit, as.data.frame(X), type = response, nthread = 1)
  if (!is.null(Xts)) {
    yhat_ts <- predict(fit, as.data.frame(Xts), type = response, nthread = 1)
  } else {
    yhat_ts <- NULL
  }
  if (is.factor(y)) {
    yhat_tr <- c(yhat_tr[, 2])
    if (!is.null(Xts)) {
      yhat_ts <- c(yhat_ts[, 2])
    }
  }
  
  # get support
  imp <- xgb.importance(model = fit$finalModel)  # only returns nonzero features
  all_vars <- data.frame(Feature = colnames(as.data.frame(X)), 
                         stringsAsFactors = F)
  imp <- left_join(all_vars, imp, by = "Feature") %>%
    replace(is.na(.), 0) %>%
    rename("var" = "Feature",
           "imp" = "Gain")
  sest <- which(imp$imp >= mean(imp$imp))
  
  return(list(yhat_tr = yhat_tr, yhat_ts = yhat_ts, fit = fit, 
              imp = imp, sest = sest))
}




