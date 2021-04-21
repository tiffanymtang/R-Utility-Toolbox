# functions to create basic eda plots

library(tidyverse)
library(irlba)
library(GGally)
library(cowplot)
library(dendextend)
library(leaflet)
library(ggdendro)

source("./ggplot_themes.R")

plotPairs <- function(data, columns, color = NULL, color2 = NULL, 
                      color.label = "", color2.label = "",
                      manual.color = NULL, manual.color2 = NULL,
                      columnLabels = colnames(data[, columns]), title = "",
                      size = .5, alpha = .5, cor.text.size = 3.5, subsample = 1,
                      show.upper = T, drop = F, show.plot = F, ...) {
  ####### Function Description ########
  # function to plot nice pair plots of variables
  #
  # inputs:
  # - data = data frame or data matrix
  # - columns = vector of column indicies or names to plot in pair plots
  # - color = vector of class labels to use as colors for lower ggplot panels
  # - color2 = vector of class labels to use as colors for upper ggplot panels
  # - color.label = string for color legend title
  # - color2.label = string for color2 legend title
  # - manual.color = vector of manual colors, corresponding to color argument
  # - manual.color2 = vector of manual colors, corresponding to color2 argument
  # - columnLabels = label names to be displayed on strips
  # - size = point size for geom_point
  # - alpha = alpha for geom_point
  # - cor.text.size = size of correlation text
  # - show.upper = logical; whether or not to show plot in upper panels
  # - subsample = proportion of points to sample and plot
  # - title = string; title for ggplot
  # - drop = logical; whether or not to drop factors with no observations
  # - show.plot = logical; whether or not to show plot
  # ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggpairs object
  #
  # example usage:
  # plotPairs(data = iris, columns = 1:ncol(iris), color = iris$Species)
  ####### 
  
  # adding labels for colors
  plt_df <- as.data.frame(data)
  if (!is.null(color)) {
    plt_df <- plt_df %>%
      mutate(color = color)
  }
  if (!is.null(color2)) {
    plt_df <- plt_df %>%
      mutate(color2 = color2)
  }
  
  # subsample points
  if (subsample != 1) {
    plt_df <- sample_frac(plt_df, size = subsample, replace = F)
  }
  
  # check if show correlations in upper panel
  if (show.upper) {
    uplt <- GGally::wrap("cor", size = cor.text.size)
  } else {
    uplt <- "blank"
  }
  
  # get columns for plotting
  if (!is.numeric(columns)) {
    columns <- which(colnames(data) %in% columns)
  }
  
  if (is.null(color) & is.null(color2)) {  # no colors
    plt <- ggpairs(
      data = plt_df,
      columns = columns,
      diag = list(continuous = GGally::wrap("densityDiag", alpha = .5)),
      lower = list(continuous = GGally::wrap("points", 
                                             size = size, alpha = alpha)),
      upper = list(continuous = uplt),
      title = title,
      columnLabels = columnLabels
    ) + myGGplotTheme(...)
    
  } else if (is.null(color2)) {  # one color
    
    # grab subplot for legend
    if (length(columns) == 1) {
      if (is.factor(color)) {
        legend <- c(1, 1)
      } else {
        legend <- NULL  
      }
    } else {
      legend_plt_df <- data.frame(color = color, 
                                  legend_x = rep(1, length(color)))
      legend_plt <- ggplot(legend_plt_df) +
        aes(x = legend_x, y = legend_x, color = color) +
        geom_point() +
        labs(color = color.label)
      if (is.null(manual.color)) {
        legend_plt <- legend_plt +
          myGGplotColor(color = color, drop = drop)
      } else {
        legend_plt <- legend_plt +
          scale_color_manual(values = manual.color, drop = drop)
      }
      legend <- grab_legend(legend_plt)
    }
    
    if (is.factor(color)) {
      upper_ls <- list(continuous = uplt)
    } else {
      if (show.upper) {
        upper_ls <- list(continuous = GGally::wrap("points", 
                                                   size = size, alpha = alpha))
      } else {
        upper_ls <- list(continuous = "blank")
      }
    }
    
    plt <- ggpairs(
      data = plt_df,
      columns = columns,
      mapping = aes(color = color),
      diag = list(continuous = GGally::wrap("densityDiag", alpha = .5)),
      lower = list(continuous = GGally::wrap("points", 
                                             size = size, alpha = alpha)),
      upper = upper_ls,
      title = title,
      legend = legend,
      columnLabels = columnLabels
    ) 
    
    # change color palette for all panels
    for (i in 1:plt$nrow) {
      for (j in 1:plt$ncol) {
        if (is.null(manual.color)) {
          plt[i, j] <- plt[i, j] +
            myGGplotColor(color = color, drop = drop) +
            myGGplotFill(fill = color, drop = drop)
        } else {
          plt[i, j] <- plt[i, j] +
            scale_color_manual(values = manual.color, drop = drop) +
            scale_fill_manual(values = manual.color, drop = drop)
        }
      }
    }
    
    plt <- plt + 
      labs(color = color.label, fill = color.label) +
      myGGplotTheme(...)
    
  } else {
    # make lower scatter plots and color by color for the ggpairs plots
    lowerContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, y = y, color = color) +
        geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper scatter plots and color by color2 for the ggpairs plots
    upperContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, y = y, color = color2) +
        geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper boxplots and color by color2 for the ggpairs plots
    upperCombo <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      if (is.factor(data$x)) {
        p <- ggplot(data) +
          aes(x = x, y = y, fill = color2) +
          geom_boxplot()
      } else {
        p <- ggplot(data) +
          aes(x = y, y = x, fill = color2) +
          geom_boxplot() +
          coord_flip()
      }
      return(p)
    }
    
    # make upper bar plots and color by color2 for the ggpairs plots
    upperDiscrete <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      data <- data %>%
        rename(x = x_str, y = y_str)
      p <- ggplot(data) +
        aes(x = x, fill = color2) +
        facet_grid(y ~.) +
        geom_bar()
      return(p)
    }
    
    # number of color factors
    nfactors <- is.factor(color) + is.factor(color2)
    
    if (length(columns) == 1) {  # in this case, plot density
      if (nfactors == 0) {
        plt <- ggpairs(data = plt_df, columns = columns, 
                       title = title, legend = c(1, 1), 
                       columnLabels = columnLabels) + 
          labs(color = color.label, fill = color.label) +
          myGGplotTheme(...)
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color2
        }
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = plt_color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          title = title,
          legend = c(1, 1),
          columnLabels = columnLabels
        ) + 
          labs(color = color.label, fill = color.label) +
          myGGplotTheme(...)
        
        if (is.null(manual.color)) {
          plt[1, 1] <- plt[1, 1] +
            myGGplotColor(color = plt_df$plt_color, drop = drop) +
            myGGplotFill(fill = plt_df$plt_color, drop = drop)
        } else {
          plt[1, 1] <- plt[1, 1] +
            scale_color_manual(values = manual.color, drop = drop) +
            scale_fill_manual(values = manual.color, drop = drop)
        }
      }
      
    } else {
      
      # grab color and color2 legends
      legend_plt_df <- plt_df %>%
        mutate(legend_x = data[, columns[1]], legend_y = data[, columns[2]])
      legend_plt1 <- ggplot(legend_plt_df) +
        geom_point(aes(x = legend_x, y = legend_y, color = color)) +
        labs(color = color.label, fill = color.label) +
        theme(legend.position = "bottom")
      legend_plt2 <- ggplot(legend_plt_df) +
        geom_point(aes(x = legend_x, y = legend_y, color = color2)) +
        labs(color = color2.label, fill = color2.label) +
        theme(legend.position = "bottom")
      if (is.null(manual.color)) {
        legend_plt1 <- legend_plt1 +
          myGGplotColor(color = color, drop = drop)
      } else {
        legend_plt1 <- legend_plt1 +
          scale_color_manual(values = manual.color, drop = drop)
      }
      if (is.null(manual.color2)) {
        legend_plt2 <- legend_plt2 +
          myGGplotColor(color = color2, option = "C", viridis = T, drop = drop)
      } else {
        legend_plt2 <- legend_plt2 +
          scale_color_manual(values = manual.color2, drop = drop)
      }
      legend1 <- grab_legend(legend_plt1)
      legend2 <- grab_legend(legend_plt2)
      
      # make ggpairs
      if (nfactors == 0) {
        plt <- ggpairs(
          data = plt_df, 
          columns = columns,
          mapping = aes(color = color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = GGally::wrap("points", 
                                                 size = size, alpha = alpha)),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = columnLabels
        )
      } else if (nfactors == 2) {
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = GGally::wrap("points", 
                                                 size = size, alpha = alpha),
                       combo = "box_no_facet"),
          upper = list(continuous = upperContinuous,
                       combo = upperCombo,
                       discrete = upperDiscrete),
          title = title,
          columnLabels = columnLabels
        )
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color2
        }
        
        plt <- ggpairs(
          data = plt_df,
          columns = columns,
          mapping = aes(color = plt_color),
          diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.5)),
          lower = list(continuous = lowerContinuous),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = columnLabels
        )
      }
      
      # change color scheme in all panels
      for (i in 1:plt$nrow) {
        for (j in 1:plt$ncol) {
          plt_fill <- plt[i, j]$labels$fill
          plt_col <- plt[i, j]$labels$colour
          if (!is.null(plt_fill)) {
            if (plt_fill %in% names(plt_df)) {
              if (all(as.character(plt_df[, plt_fill]) == 
                      as.character(color))) {
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotFill(fill = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_fill_manual(values = manual.color, drop = drop)
                }
              } else {
                if (is.null(manual.color2)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotFill(fill = color2, option = "C", viridis = T,
                                 drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_fill_manual(values = manual.color2, drop = drop)
                }
              }
            }
          }
          
          if (!is.null(plt_col)) {
            if (plt_col %in% names(plt_df)) {
              ptr <- FALSE
              if (is.numeric(plt_df[, plt_col]) & is.numeric(color)) {
                ptr <- all(plt_df[, plt_col] == color)
              } else if (is.factor(plt_df[, plt_col]) & is.factor(color)) {
                ptr <- all(as.character(plt_df[, plt_col]) == 
                             as.character(color))
              }
              if (ptr) {
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotColor(color = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_color_manual(values = manual.color, drop = drop)
                }
              } else {
                if (is.null(manual.color2)) {
                  plt[i, j] <- plt[i, j] +
                    myGGplotColor(color = color2, option = "C", viridis = T,
                                  drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    scale_color_manual(values = manual.color2, drop = drop)
                }
              }
            }
          }
        }
      }
    }
    
    if (length(columns) != 1) {
      plt <- plot_grid(ggmatrix_gtable(plt + myGGplotTheme(...)),
                       legend1, legend2, 
                       nrow = 3, rel_heights = c(10, 1, 1))
    }
    
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotPCA <- function(X, pca.out, 
                    npcs, pcs, 
                    color = NULL, color2 = NULL, 
                    color.label = "", color2.label = "",
                    manual.color = NULL, manual.color2 = NULL,
                    size = .5, alpha = 1, subsample = 1,
                    show.var = T, center = T, scale = F,
                    title = "", show.plot = F, save = F, path = NULL, ...) {
  ####### Function Description ########
  # function to plot PCA pairs plot
  #
  # inputs: (must specify either npcs or pcs, and either X or pca.out)
  # - X = data matrix or data.frame (must specify either X or pca.out)
  # - pca.out = output of plotPCA(); to avoid computing svds (i.e. pca loadings
  #   and scores) again if computed previously
  # - npcs = number of top pcs to plot (must specify either npcs or pcs)
  # - pcs = vector of which pcs to show (optional; only needed if npcs missing)
  # - color = vector of class labels to use as colors for lower ggplot panels
  # - color2 = (optional) vector of secondary class labels to use as colors for
  #   upper ggplot panels
  # - color.label = string for color legend title
  # - color2.label = string for color2 legend title
  # - manual.color = vector of manual colors, corresponding to color argument
  # - manual.color2 = vector of manual colors, corresponding to color2 argument
  # - size = point size for geom_point
  # - alpha = alpha for geom_point
  # - subsample = proportion of points to sample and plot
  # - show.var = logical; show proportion of variance explained on axes labels
  # - center = logical; whether or not to center data for pca
  # - scale = logical; whether or not to scale data for pca
  # - title = string; title for ggplot
  # - show.plot = logical; whether or not to show plot
  # - save = logical; whether or not to save plot
  # - path = string ending in .rds; filename to save plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: list of 4
  # - plot = pca pair plots
  # - scores = PCA scores
  # - loadings = PCA loadings
  # - var.explained = proportion of variance explained
  # 
  # example usage:
  # out <- plotPCA(X = iris[, -5], npcs = 3, color = iris$Species)
  # out$plot
  # iris2 <- data.frame(iris, z = rep(letters[1:2], length.out = nrow(iris)))
  # out <- plotPCA(X = iris2[, -c(5, 6)], npcs = 3, 
  #                color = iris2$Species, color2 = iris2$z)
  # out$plot
  ####### 
  
  if (!missing(X)) {
    X <- scale(as.matrix(X), center = center, scale = scale)
    
    if (sum(is.na(X)) > 0) { # error checking
      stop("NAs found in X")
    }
  }
  
  if (!missing(npcs)) { # which pcs to plot
    pcs <- 1:npcs
  } else {
    npcs <- length(pcs)
  }
  
  max_pcs <- max(pcs) # maximum pc
  
  # compute svd of X
  if (!missing(X)) {
    if (max_pcs / min(nrow(X), ncol(X)) > .25) { # full svd
      X_svd <- svd(X)
    } else { # only compute top singular vectors
      X_svd <- X %>% irlba(nu = max_pcs, nv = max_pcs)
    }
    d = X_svd$d
  } else {
    X_svd <- list(
      u = pca.out$scores,
      v = pca.out$loadings,
      var_explained = pca.out$var.explained
    )
    d = NULL
  }
  
  # compute and show proportion of variance
  if (show.var) { 
    if (!missing(X)) {
      total_var <- norm(X, "F")^2
      var_explained <- X_svd$d^2 / total_var
    } else {
      var_explained <- X_svd$var_explained
    }
    var_explained_str <- paste0(" (", round(var_explained, 3), ")")
  } else {
    var_explained <- NA
    var_explained_str <- rep("", times = max_pcs)
  }
  
  # initializing data frame for plotting
  plt_df <- data.frame(X_svd$u)
  
  # adding labels for colors
  if (!is.null(color) & !is.null(color2)) {
    plt_df <- plt_df %>%
      mutate(
        color = color,
        color2 = color2
      )
  } else if (!is.null(color)) {
    plt_df <- plt_df %>%
      mutate(color = color)
  }
  
  # subsample points
  if (subsample != 1) {
    plt_df <- sample_frac(plt_df, size = subsample, replace = F)
  }
  
  if (npcs == 2) {  # ggplot object instead of ggpairs
    
    if (!is.null(color)) {  # plot with color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2, color = color) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          color = color.label, title = title
        ) +
        myGGplotTheme(...)
      
      if (is.null(manual.color)) {
        plt <- plt + myGGplotColor(color = plt_df$color)
      } else {
        plt <- plt + scale_color_manual(values = manual.color)
      }
    } else {  # plot without color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          title = title
        ) +
        myGGplotTheme(...)
    }
  } else {
    plt <- plotPairs(data = plt_df, columns = pcs, title = title, 
                     size = size, alpha = alpha, show.upper = F, drop = F, 
                     color = color, color2 = color2, 
                     color.label = color.label, color2.label = color2.label,
                     manual.color = manual.color, manual.color2 = manual.color2,
                     columnLabels = paste0("PC", pcs, var_explained_str[pcs]),
                     ...)
  }
  
  if (show.plot) {
    print(plt)
  }
  
  if (save) { # save figure to file
    if (!is.null(path)) {
      saveRDS(plt, paste0("./", path))
    } else {
      saveRDS(plt, paste0("./", title, ".rds"))
    }
  }
  
  return(list(
    plot = plt, scores = X_svd$u, loadings = X_svd$v, d = d,
    var.explained = var_explained
  ))
}

plotHeatmap <- function(X, y.labels = rownames(X), x.labels = colnames(X),
                        y.labels.num = FALSE, x.labels.num = FALSE,
                        y.label.colors = NULL, x.label.colors = NULL,
                        y.groups = NULL, x.groups = NULL,
                        center = FALSE, scale = FALSE, 
                        text.size = 0, theme = "default", position = "identity",
                        size = 0, viridis = T, option = "C", 
                        col_quantile = F, n_quantiles = 5,
                        manual.fill = NULL, show.plot = F, ...) {
  ####### Function Description ########
  # plot nice heatmap of X
  #
  # inputs:
  # - X = data matrix or data frame
  # - y.labels = row/y labels in heatmap
  # - x.labels = column/x labels in heatmap
  # - y.labels.num = logical; whether or not y labels are numeric/continuous
  # - x.labels.num = logical; whether or not x labels are numeric/continuous
  # - y.label.colors = vector to use for coloring y axis text; optional
  # - x.label.colors = vector to use for coloring x axis text; optional
  # - y.groups = vector of groups for rows/y
  # - x.groups = vector of groups for columns/x
  # - center = logical; whether or not to center columns of X
  # - scale = logical; whether or not to scale columns of X
  # - text.size = numeric; size of text on heatmap; no text if text.size = 0
  # - theme = "default" or "blank"
  # - position = "identity" or "ordered"
  # - size = size argument in geom_tile() to avoid white lines in continuous
  #     heatmap
  # - viridis = logical; whether or not to use viridis color scheme
  # - option = viridis option argument
  # - col_quantile = logical; whether or not to use quantile color scale
  # - n_quantiles = number of quantiles for color scheme; only used if
  #     col_quantile = T
  # - manual.fill = "temperature" or vector of colors for the color scale
  #     (optional)
  # - show.plot = logical; whether or not to print plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggplot object
  # 
  # example usage:
  # df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
  # plotHeatmap(df,  position = "identity")
  #######
  
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  
  y.labels <- y.labels
  x.labels <- x.labels
  y.label.colors <- y.label.colors
  x.label.colors <- x.label.colors

  if (any(duplicated(y.labels)) | any(duplicated(x.labels))) {
    stop("x.labels and y.labels cannot contain duplicates.")
  }
  
  # center/scale X if specified
  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }
  
  # convert to long df to plot
  X_long <- as.data.frame(X)
  if (!x.labels.num & !y.labels.num) {
    X_long <- as.data.frame(X) %>%
      setNames(x.labels) %>%
      mutate(y = fct_rev(fct_inorder(y.labels))) %>%
      gather(data = ., -y, key = "x", value = "fill") %>%
      mutate(x = factor(x, levels = x.labels))
  } else if (!x.labels.num) {
    X_long <- as.data.frame(X) %>%
      setNames(x.labels) %>%
      mutate(y = y.labels) %>%
      gather(data = ., -y, key = "x", value = "fill") %>%
      mutate(x = factor(x, levels = x.labels))
  } else if (!y.labels.num) {
    X_long <- as.data.frame(X) %>%
      setNames(x.labels) %>%
      mutate(y = fct_rev(fct_inorder(y.labels))) %>%
      gather(data = ., -y, key = "x", value = "fill") %>%
      mutate(x = as.numeric(x))
  } else {
    X_long <- as.data.frame(X) %>%
      setNames(x.labels) %>%
      mutate(y = as.numeric(y.labels)) %>%
      gather(data = ., -y, key = "x", value = "fill") %>%
      mutate(x = as.numeric(x))
  }
  
  if (!is.null(x.groups)) {
    X_long$x.group <- x.groups[X_long$x]
  }
  if (!is.null(y.groups)) {
    X_long$y.group <- y.groups[X_long$y]
  }
  
  # base plot
  if (x.labels.num | y.labels.num) {
    plt <- ggplot(X_long) +
      geom_tile(aes(x = x, y = y, fill = fill, color = fill), size = size) +
      guides(color = FALSE)
  } else {
    plt <- ggplot(X_long) +
      geom_tile(aes(x = x, y = y, fill = fill))
  }
  
  if (!is.null(x.groups) & !is.null(y.groups)) {
    plt <- plt +
      facet_grid(y.group ~ x.group, space = "free", scales = "free") +
      theme(panel.spacing = unit(0, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 1))
  } else if (!is.null(x.groups)) {
    plt <- plt +
      facet_grid(~ x.group, space = "free", scales = "free") +
      theme(panel.spacing = unit(0, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 1))
  } else if (!is.null(y.groups)) {
    plt <- plt +
      facet_grid(y.group ~., space = "free", scales = "free") +
      theme(panel.spacing = unit(0, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 1))
  }
  
  # add text if specified
  if (text.size > 0) {
    plt <- plt +
      geom_text(aes(x = x, y = y, label = fill), 
                color = "black", size = text.size)
  }
  
  # add theme
  if (identical(theme, "default")) {
    plt <- plt + myGGplotTheme(...)
  } else if (identical(theme, "blank")) {
    plt <- plt + myGGplotMapTheme()
  } else {
    stop("Unknown theme argument.")
  }
  
  # add color
  if (!col_quantile | is.factor(X_long$fill)) {
    if (is.null(manual.fill)) {
      plt <- plt +
        myGGplotFill(fill = fill, viridis = viridis, option = option) +
        myGGplotColor(color = fill, viridis = viridis, option = option)
    } else {
      if (manual.fill == "temperature") {
        plt <- plt + 
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                               midpoint = median(X_long$fill),
                               limit = c(min(X_long$fill), max(X_long$fill))) +
          scale_color_gradient2(low = "blue", high = "red", mid = "white",
                                midpoint = median(X_long$fill),
                                limit = c(min(X_long$fill), max(X_long$fill)))
      } else if (manual.fill == "cor_temperature") {
        plt <- plt + 
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                               midpoint = 0, limit = c(-1, 1)) +
          scale_color_gradient2(low = "blue", high = "red", mid = "white", 
                                midpoint = 0, limit = c(-1, 1))
      } else {
        plt <- plt +
          scale_fill_manual(values = manual.fill) +
          scale_color_manual(values = manual.fill)
      }
    }
  } else {
    if (!is.null(manual.fill)) {
      if (manual.fill == "temperature") {
        heat_pal <- c("blue", "white", "red")
      } else {
        heat_pal <- manual.fill
      }
    } else {
      heat_pal <- viridis(n = n_quantiles, option = option)
    }
    probs <- seq(0, 1, length = length(heat_pal))
    quantiles <- quantile(X_long$fill, probs, na.rm = T)
    heat_pal_values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
    plt <- plt + 
      scale_fill_gradientn(values = heat_pal_values, colors  = heat_pal) +
      scale_color_gradientn(values = heat_pal_values, colors = heat_pal)
  }
  
  # image position
  if (identical(position, "identity")) {
    if (!x.labels.num) {
      plt <- plt + 
        scale_x_discrete(expand = c(0, 0))
    } else {
      plt <- plt + 
        scale_x_continuous(expand = c(0, 0))
    }
    if (!y.labels.num) {
      plt <- plt + 
        scale_y_discrete(expand = c(0, 0))
    } else {
      plt <- plt +
        scale_y_continuous(expand = c(0, 0))
    }
  } else if (identical(position, "ordered")) {
    if (!x.labels.num) {
      plt <- plt + 
        scale_x_discrete(expand = c(0, 0))
    } else {
      plt <- plt + 
        scale_x_continuous(expand = c(0, 0))
    }
    if (!y.labels.num) {
      plt <- plt + 
        scale_y_discrete(expand = c(0, 0), limits = rev)
    } else {
      plt <- plt +
        scale_y_continuous(expand = c(0, 0))
    }
  } else {
    stop("Unknown position argument.")
  }
  
  # add color to x and y axis text
  if (!is.null(y.label.colors)) {
    ylab_df <- data.frame(ylab_x = 1, ylab_y = 1, ylab_color = y.label.colors)
    if (is.factor(y.label.colors)) {
      if (nlevels(y.label.colors) <= 8) {
        y_colors <- brewer.pal(n = 8, name = "Dark2")
        y_colors[2] <- y_colors[1]
        y_colors[1] <- "#FF9300"
        ylab_colors <- y_colors[y.label.colors]
      } else {
        y_colors <- colorFactor(palette = "viridis", 
                                domain = levels(y.label.colors))
        ylab_colors <- y_colors(y.label.colors)
      }
      plt <- plt +
        geom_point(aes(x = ylab_x, y = ylab_y, color = ylab_color), 
                   data = ylab_df, size = -1) +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        scale_color_manual(values = y_colors)
    } else {
      y_colors <- colorNumeric(palette = "viridis", 
                               domain = c(min(y.label.colors), 
                                          max(y.label.colors)))
      ylab_colors <- y_colors(y.label.colors)
      plt <- plt +
        geom_point(aes(x = ylab_x, y = ylab_y, color = ylab_color), 
                   data = ylab_df, size = -1) +
        scale_colour_viridis(discrete = F)
    }
    plt <- plt +
      theme(axis.text.y = element_text(color = ylab_colors))
  }
  if (!is.null(x.label.colors)) {
    xlab_df <- data.frame(xlab_x = 1, xlab_y = 1, xlab_color = x.label.colors)
    if (is.factor(x.label.colors)) {
      if (nlevels(x.label.colors) <= 8) {
        x_colors <- brewer.pal(n = 8, name = "Dark2")
        x_colors[2] <- x_colors[1]
        x_colors[1] <- "#FF9300"
        xlab_colors <- x_colors[x.label.colors]
      } else {
        x_colors <- colorFactor(palette = "viridis", 
                                domain = levels(x.label.colors))
        xlab_colors <- x_colors(x.label.colors)
      }
      plt <- plt +
        geom_point(aes(x = xlab_x, y = xlab_y, color = xlab_color), 
                   data = xlab_df, size = -1) +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        scale_color_manual(values = x_colors)
    } else {
      x_colors <- colorNumeric(palette = "viridis", 
                               domain = c(min(x.label.colors), 
                                          max(x.label.colors)))
      xlab_colors <- x_colors(x.label.colors)
      plt <- plt +
        geom_point(aes(x = xlab_x, y = xlab_y, color = xlab_color), 
                   data = xlab_df, size = -1) +
        scale_colour_viridis(discrete = F)
    }
    plt <- plt +
      theme(axis.text.x = element_text(color = xlab_colors))
  }
  
  if (show.plot) {
    print(plt)
  }
  
  return(plt)
}

plotHclustHeatmap <- function(X, 
                              y.labels = rownames(X), x.labels = colnames(X),
                              y.labels.num = FALSE, x.labels.num = FALSE,
                              y.label.colors = NULL, x.label.colors = NULL,
                              y.groups = NULL, x.groups = NULL,
                              clust.x = TRUE, clust.y = TRUE,
                              dist.metric.x = "euclidean",
                              dist.metric.y = "euclidean",
                              dist.mat.x = NULL, dist.mat.y = NULL,
                              linkage.x = "ward.D", linkage.y = "ward.D",
                              center = FALSE, scale = FALSE,
                              text.size = 0, theme = "default", size = 0,
                              viridis = T, option = "C", 
                              col_quantile = F, n_quantiles = 5,
                              manual.fill = NULL, show.plot = F, ...) {
  ####### Function Description ########
  # plot clustered heatmap of X
  #
  # inputs:
  # - X = data matrix or data frame
  # - y.labels = row/y labels in heatmap
  # - x.labels = column/x labels in heatmap
  # - y.labels.num = logical; whether or not y labels are numeric/continuous
  # - x.labels.num = logical; whether or not x labels are numeric/continuous
  # - y.label.colors = vector to use for coloring y axis text; optional
  # - x.label.colors = vector to use for coloring x axis text; optional
  # - clust.x = logical; whether or not to cluster columns
  # - clust.y = logical; whether or not to cluster rows
  # - dist.metric.x = distance metric for clustering columns (see stats::dist)
  # - dist.metric.y = distance metric for clustering rows (see stats::dist)
  # - dist.mat.x = distance matrix for clustering columns (optional); must 
  #     provide either dist.metric or dist.mat
  # - dist.mat.y = distance matrix for clustering rows (optional); must provide 
  #     either dist.metric or dist.mat
  # - linkage.x = type of linkage for clustering columns (see stats::hclust)
  # - linkage.y = type of linkage for clustering rows (see stats::hclust)
  # - center = logical; whether or not to center columns of X
  # - scale = logical; whether or not to scale columns of X
  # - text.size = numeric; size of text on heatmap; no text if text.size = 0
  # - theme = "default" or "blank"
  # - size = size argument in geom_tile() to avoid white lines in continuous
  #     heatmap
  # - viridis = logical; whether or not to use viridis color scheme
  # - option = viridis option argument
  # - col_quantile = logical; whether or not to use quantile color scale
  # - n_quantiles = number of quantiles for color scheme; only used if
  #     col_quantile = T
  # - manual.fill = "temperature" or vector of colors for the color scale
  #     (optional)
  # - show.plot = logical; whether or not to print plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggplot object
  # 
  # example usage:
  # df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
  # plotHclustHeatmap(df)
  #######
  
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  if (any(is.na(X))) {
    stop("NAs found in data. Please remove NAs.")
  }
  
  y.labels <- y.labels
  x.labels <- x.labels
  y.label.colors <- y.label.colors
  x.label.colors <- x.label.colors
  
  # center/scale X if specified
  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }
  
  # cluster columns
  if (clust.x) {
    if (is.null(dist.mat.x)) {
      Dmat.x <- dist(t(X), method = dist.metric.x)
    } else {
      if (!("dist" %in% class(dist.mat.x))) {
        Dmat.x <- as.dist(dist.mat.x)
      } else {
        Dmat.x <- dist.mat.x
      }
    }
    if (is.null(x.groups)) {
      hclust_out_x <- hclust(Dmat.x, method = linkage.x)
      order_x <- hclust_out_x$order
    } else {
      order_x <- c()
      for (group in unique(x.groups)) {
        group.idx <- x.groups == group
        Dmat.x.sub <- as.dist(as.matrix(Dmat.x)[group.idx, group.idx])
        hclust_out_x <- hclust(Dmat.x.sub, method = linkage.x)
        col_idx <- data.frame(idx = 1:sum(group.idx), col = which(group.idx))
        order_x <- c(order_x, col_idx$col[match(hclust_out_x$order, col_idx$idx)])
      }
      x.groups <- x.groups[order_x]
    }
    X <- X[, order_x]
    x.labels <- x.labels[order_x]
    if (!is.null(x.label.colors)) {
      x.label.colors <- x.label.colors[order_x]
    }
  }
  
  # cluster rows
  if (clust.y) {
    if (is.null(dist.mat.y)) {
      Dmat.y <- dist(X, method = dist.metric.y)
    } else {
      if (!("dist" %in% class(dist.mat.y))) {
        Dmat.y <- as.dist(dist.mat.y)
      } else {
        Dmat.y <- dist.mat.y
      }
    }
    if (is.null(y.groups)) {
      hclust_out_y <- hclust(Dmat.y, method = linkage.y)
      order_y <- hclust_out_y$order
    } else {
      order_y <- c()
      for (group in unique(y.groups)) {
        group.idx <- y.groups == group
        Dmat.y.sub <- as.dist(as.matrix(Dmat.y)[group.idx, group.idx])
        hclust_out_y <- hclust(Dmat.y.sub, method = linkage.y)
        col_idx <- data.frame(idx = 1:sum(group.idx), col = which(group.idx))
        order_y <- c(order_y, col_idx$col[match(hclust_out_y$order, col_idx$idx)])
      }
      y.groups <- y.groups[order_y]
    }
    X <- X[order_y, ]
    y.labels <- y.labels[order_y]
    if (!is.null(y.label.colors)) {
      y.label.colors <- y.label.colors[order_y]
    }
  } 
  
  plt <- plotHeatmap(X = X, y.labels = y.labels, x.labels = x.labels, 
                     y.labels.num = y.labels.num, x.labels.num = x.labels.num,
                     y.label.colors = y.label.colors, 
                     x.label.colors = x.label.colors,
                     y.groups = y.groups, x.groups = x.groups,
                     text.size = text.size, theme = theme, 
                     position = "identity", size = size,
                     viridis = viridis, option = option, 
                     col_quantile = col_quantile, n_quantiles = n_quantiles, 
                     manual.fill = manual.fill, show.plot = show.plot)
  return(plt)
}

plotCorHeatmap <- function(X, cor.type = "pearson",
                           axis.labels = colnames(X), axis.label.colors = NULL,
                           clust = TRUE, linkage = "ward.D",
                           text.size = 0, theme = "default",
                           viridis = T, option = "C", 
                           col_quantile = F, n_quantiles = 5,
                           manual.fill = "cor_temperature", show.plot = F, 
                           ...) {
  ####### Function Description ########
  # plot (clustered) correlation heatmap of X
  #
  # inputs:
  # - X = data matrix or data frame
  # - cor.type = correlation metric; one of "pearson", "kendall", or "spearman"
  # - axis.labels = axis text labels in heatmap
  # - axis.label.colors = vector to use for coloring axis text labels; optional
  # - clust = logical; whether or not to cluster columns and rows
  # - linkage = type of linkage for clustering (see stats::hclust)
  # - text.size = numeric; size of text on heatmap; no text if text.size = 0
  # - theme = "default" or "blank"
  # - position = "identity" or "ordered"
  # - viridis = logical; whether or not to use viridis color scheme
  # - option = viridis option argument
  # - col_quantile = logical; whether or not to use quantile color scale
  # - n_quantiles = number of quantiles for color scheme; only used if
  #     col_quantile = T
  # - manual.fill = "temperature" or vector of colors for the color scale
  #     (optional)
  # - show.plot = logical; whether or not to print plot
  # - ... = additional arguments to pass to myGGplotTheme()
  #
  # outputs: a ggplot object
  # 
  # example usage:
  # df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
  # plotCorHeatmap(df)
  #######
  
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  
  axis.labels = axis.labels
  axis.label.colors = axis.label.colors

  cor_mat <- cor(X, method = cor.type, use = "pairwise.complete.obs")
  cor_dist <- as.dist(1 - abs(cor_mat))
  
  # cluster
  if (clust) {
    hclust_out <- hclust(cor_dist, method = linkage)
    cor_mat <- cor_mat[hclust_out$order, hclust_out$order]
  }
  
  cor_mat <- round(cor_mat, 2)
  
  plt <- plotHeatmap(X = cor_mat, 
                     y.labels = axis.labels, x.labels = axis.labels, 
                     y.label.colors = axis.label.colors, 
                     x.label.colors = axis.label.colors,
                     text.size = text.size, theme = theme, 
                     position = "identity", viridis = viridis, option = option, 
                     col_quantile = col_quantile, n_quantiles = n_quantiles, 
                     manual.fill = manual.fill, show.plot = show.plot, ...)
  
  return(plt)
}

plotHclust <- function(data, 
                       leaf_labels = rownames(data), leaf_colors = NULL, 
                       dist.metric = "euclidean", dist.mat = NULL, 
                       linkage = "ward.D",
                       text.size = 10, show.plt = F,
                       title = NULL, manual.color = NULL, 
                       save = F, save.filename = NULL) {
  ####### Function Description ########
  # perform hierarchical clustering and plot resulting dendrogram
  # 
  # inputs:
  # - data = data matrix or data frame
  # - leaf_labels = labels for leaf nodes (e.g., class/outcome labels); optional
  # - leaf_colors = vector to use for coloring leaf nodes; optional
  # - dist.metric = distance metric (see stats::dist)
  # - dist.mat = distance matrix (optional); must provide either dist.metric or
  #     dist.mat
  # - linkage = type of linkage for hierarchical clustering (see stats::hclust)
  # - text.size = size of text for leaves
  # - title = string for plot title name
  # - manual.color = vector of manual colors for leaf text labels
  # - show.plt = logical; whether or not to show plot
  # - save = logical; whether or not to save plot as rds file
  # - save.filename = string ending in .rds; where to save file
  # 
  # output: list of 3
  # - plt = dendrogram plot; ggplot object
  # - hclust = output of hclust()
  # - dend = hierarchical clustering dendrogram data
  #
  # example usage:
  # out <- plotHclust(data = iris[, -5], y = iris$Species, show.plt = T)
  # plot(out$dend)
  ####### 

  data <- as.matrix(data)
  
  if (sum(is.na(data)) > 0) {
    stop("NAs found in data")
  }
  
  # distance matrix
  if (is.null(dist.mat)) {
    Dmat <- dist(data, method = dist.metric)
  } else {
    if (!("dist" %in% class(dist.mat))) {
      Dmat <- as.dist(dist.mat)
    } else {
      Dmat <- dist.mat
    }
  }
  
  # hierarchical clustering
  hclust_out <- hclust(Dmat, method = linkage)
  hclust_dend <- as.dendrogram(hclust_out)
  
  if (!is.null(leaf_colors)) {  # annotate tree leaves
    # color leaf labels
    if (is.factor(leaf_colors)) {  # categorical leaf_colors
      if (is.null(manual.color)) {
        if (nlevels(leaf_colors) <= 8) {
          my_colors <- brewer.pal(n = 8, name = "Dark2")
          my_colors[2] <- my_colors[1]
          my_colors[1] <- "#FF9300"
          lab_colors <- my_colors[leaf_colors][order.dendrogram(hclust_dend)]
        } else {
          my_colors <- colorFactor(palette = "viridis", 
                                   domain = levels(leaf_colors))
          lab_colors <- my_colors(leaf_colors)[order.dendrogram(hclust_dend)]
        }
      } else {
        lab_colors <- manual.color[leaf_colors][order.dendrogram(hclust_dend)]
      }
    } else {  # continuous leaf_colors
      if (is.null(manual.color)) {
        my_colors <- colorNumeric(palette = "viridis", 
                                  domain = c(min(leaf_colors), 
                                             max(leaf_colors)))
        lab_colors <- my_colors(leaf_colors)[order.dendrogram(hclust_dend)]
      } else {
        my_colors <- manual.color
        lab_colors <- my_colors[leaf_colors][order.dendrogram(hclust_dend)]
      }
    }
    
    lab_df <- data.frame(x = rep(0, nrow(data)),
                         y = rep(0, nrow(data)),
                         color = leaf_colors)
  } else {
    lab_colors <- "black"
  }
  
  # get leaf labels
  if (is.null(leaf_labels)) {
    labels(hclust_dend) <- rep("------", length(labels(hclust_dend)))
  } else {
    labels(hclust_dend) <- leaf_labels
  }
  
  if (is.null(title)) {
    title <- paste0(
      "Hierarchical Clustering: ",
      linkage, " Linkage, ", dist.metric, " Distance"
    )
  }
  
  # convert to ggplot object
  hclust_dend <- dendro_data(hclust_dend)
  hclust_plt <- suppressWarnings(
    suppressMessages(
      ggdendrogram(hclust_dend) +
        labs(title = title, color = "Label") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(breaks = seq_along(hclust_dend$labels$label),
                           labels = hclust_dend$labels$label,
                           expand = c(0, 0)) +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(color = lab_colors, 
                                         size = text.size))
    )
  )
  if (!is.null(leaf_colors)) {  # add legend
    if (is.factor(leaf_colors)) {
      hclust_plt <- hclust_plt +
        geom_point(aes(x = x, y = y, color = color), data = lab_df, size = -1) +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        scale_color_manual(values = my_colors)
    } else {
      hclust_plt <- hclust_plt +
        geom_point(aes(x = x, y = y, color = color), data = lab_df, size = -1) +
        scale_colour_viridis(discrete = F)
    }
  }
  
  if (show.plt) {
    print(hclust_plt)
  }
  
  if (save) { # save figure to file
    if (!is.null(save.filename)) {
      saveRDS(hclust_dend, paste0("./", save.filename))
    } else {
      saveRDS(hclust_dend, paste0("./", title, ".rds"))
    }
  }
  
  return(list(plt = hclust_plt, hclust = hclust_out, dend = hclust_dend))
}
