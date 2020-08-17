# functions to evaluate prediction models

library(tidyverse)
library(PRROC)

evalPreds <- function(y, yhat, metric, group = NULL, na.rm = F) {
  ####### Function Description ########
  # function to calculate prediction error between y and yhat, as measured by
  # the given metric
  # 
  # inputs:
  # - y = vector, matrix, or data.frame of the true response values
  # - yhat = vector, matrix, or data.frame of the true response values
  # - metric = character vector of prediction error metrics to compute; elements
  #   should be one of "MSE", "R2", "MAE", "Correlation", "Class", "AUC", "PR"
  # - group = (optional) vector of factors to group prediction errors by
  # - na.rm = logical; whether or not to remove NAs
  # 
  # output: a data frame with the following columns
  # - Group = (if group specified): name of group for grouped prediction errors
  # - Metric = prediction error metric
  # - Value = prediction error value
  ####### 
  
  # error checking
  isvec <- is.null(dim(y))
  if ((isvec & (length(y) != length(yhat))) |
      (!isvec & any(dim(y) != dim(yhat)))) {
    stop("y and yhat must be the same size.")
  }
  if (!all(metric %in% c("MSE", "R2", "MAE", 
                         "Correlation", "Class", "AUC", "PR"))) {
    stop("metric has not been implemented.")
  }
  if (("AUC" %in% metric) | ("PR" %in% metric)) {
    if (length(unique(y)) != 2) {
      stop("y must be binary to evaluate AUC and PR metrics.")
    }
    if ((min(yhat) < 0) | (max(yhat) > 1)) {
      stop("yhat must give the class proportions.")
    }
    ylevels <- levels(as.factor(y))
    Y0 <- ylevels[1]
    Y1 <- ylevels[2]
  }
  
  # create (long) grouped prediction data frame with groups, y, and yhat
  if (isvec) {
    pred_df <- data.frame(Group = "all", y = y, yhat = yhat)
    if (!is.null(group)) {
      pred_df <- rbind(pred_df, data.frame(Group = group, y = y, yhat = yhat))
    }
    pred_df <- pred_df %>%
      group_by(Group)
    
  } else {
    y_long <- data.frame(Group = "all", y) %>%
      gather(key = "column", value = "y", -Group)
    yhat_long <- data.frame(Group = "all", yhat) %>%
      gather(key = "column", value = "yhat", -Group)
    if (!is.null(group)) {
      y_long <- rbind(y_long,
                      data.frame(Group = group, y) %>%
                        gather(key = "column", value = "y", -Group))
      yhat_long <- rbind(yhat_long,
                         data.frame(Group = group, yhat) %>%
                           gather(key = "column", value = "yhat", -Group))
    }
    pred_df <- left_join(y_long, yhat_long, by = c("Group", "column")) %>%
      group_by(Group, column)
  }
  
  # compute error metrics between y and yhat
  err_out <- NULL
  for (m in metric) {
    if (m == "MSE") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = mean((y - yhat)^2, na.rm = na.rm))
    } else if (m == "R2") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = 1 - mean((y - yhat)^2, na.rm = na.rm) / 
                    mean((y - mean(y))^2, na.rm = na.rm))
    } else if (m == "MAE") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = mean(abs(y - yhat), na.rm = na.rm))
    } else if (m == "Correlation") {
      err <- pred_df %>%
        summarise(Metric = m,
                  cor(y, yhat, use = "pairwise.complete.obs"))
    } else if (m == "Class") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = mean(y != yhat, na.rm = na.rm))
    } else if (m == "AUC") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = roc.curve(yhat[(y == Y1) & !(is.na(y))],
                                    yhat[(y == Y0) & !(is.na(y))],
                                    curve = F)$auc)
    } else if (m == "PR") {
      err <- pred_df %>%
        summarise(Metric = m,
                  Value = pr.curve(yhat[(y == Y1) & !(is.na(y))],
                                   yhat[(y == Y0) & !(is.na(y))],
                                   curve = F)$auc.integral)
    } else {
      stop("Metric has not been implemented.")
    }
    err_out <- rbind(err_out, err)
  }
  
  # clean up output formatting
  if (!isvec) {
    err_out <- err_out %>%
      spread(key = "column", value = "Value") %>%
      select(Group, Metric, colnames(data.frame(y)))
  }
  if (is.null(group)) {
    err_out <- err_out %>% 
      ungroup() %>%
      select(-Group)
  }
  
  return(err_out)
}