# R Utility Toolbox

A collection of my random but frequently used R functions to make life easier as a statistician and data scientist

## Overview of Toolbox

* [Data Cleaning/Preprocessing](#data-cleaningpreprocessing)
	- functions for basic data preprocessing and cleaning
* [Modeling](#modeling)
	- functions for fitting and evaluating prediction models with consistent inputs and outputs
* [Plots and Tables](#plots-and-tables)
	- functions to create custom ggplot and kable themes as well as to provide convenient wrappers for commonly used plots and tables
* [Rmarkdown Templates](#rmarkdown-templates)
	- Rmarkdown templates for generating reports from simulation studies
* [Miscellaneous](#miscellaneous)
         

### Data Cleaning/Preprocessing

- **clean_functions.R**: functions for basic data preprocessing and cleaning
	- **basicDataSummary()**: function to summarize basic data oddities (e.g., NAs, constant columns, missing factor levels)
	- **removeNACols()**: function to remove all columns with at least one NA value
	- **removeConstantCols()**: function to remove all columns that are constant

- **filter_features.R**: functions for filtering number of features in high-dimensional data
	- **filterByAssoc()**: function to reduce number of features by keeping those with the highest univariate association with the repsonse (measure via ANOVA)
	- **filterByCor()**: function to reduce number of features by keeping those with the highest correlation with the response
	- **filterByCorBootstrap()**: function to reduce number of features by keeping those with the highest correlation with the response, averaged across multiple bootstrap samples
	- **filterByLasso()**: function to reduce number of features by keeping those with the largest magnitude coefficients from Lasso
	- **filterByRF()**: function to reduce number of features by keeping those with the highest feature importance score from RF
	- **filterByVar()**: function to reduce number of features by keeping those with the largest variance

### Modeling

- **fit_models.R**: functions for fitting various models while returning a consistent output that includes the 1) fitted predictions, 2) the test predictions, 3) the model fit, 4) a feature importance data frame, and 5) the estimated support
	- **fitLM()**: fit a linear model
	- **fitLasso()**: fit a Lasso model
	- **fitRF()**: fit a random forest
	- **fitXGB()**: fit an xgboost model

- **eval_functions.R**: functions for evaluating prediction models
	- **evalPreds()**: function to calculate prediction error between observed and predicted responses, as measured by MSE, $R^2$, MAE, correlation, (balanced) classification error, AUC, and/or PR
	- **evalPermTest()**: function to perform permutation test for prediction error between observed and predicted responses

### Plots and Tables

- **ggplot_themes.R**: create customized ggplot theme
	- **prettyGGplotTheme()**: customized ggplot theme
	- **prettyGGplotColor()**: customized ggplot color theme
	- **prettyGGplotFill()**: customized ggplot fill theme

- **ggplot_wrappers.R**: convenient wrapper functions around common ggplot geometric objects (geoms) and adds custom ggplot theme
	- **plotBarplot()**
	- **plotBoxplot()**
	- **plotDensity()**
	- **plotHistogram()**
	- **plotLine()**
	- **plotScatter()**

- **eda_plotting_functions.R**: functions to generate common EDA plots
	- **plotPairs()**
	- **plotPCA()**
	- **plotHeatmap()**

- **table_wrappers.R**: convenient wrapper functions to make tables using kable and datatable
	- **prettyKable()**: function to make nice kable tables with options to bold certain entries
	- **prettyDT()**: function to make nice DT tables with options to bold certain entries

### Rmarkdown Templates

- **rmd_functions.R**: Rmarkdown helper functions
	- **subchunkify()**: function to allow for multiple plots of different sizes and captions within single R code chunk in Rmd

- **rmd_templates/**: Rmarkdown templates for simulation results
	- **custom_rmd_theme.css**: custom css style
	- **parameterized_results_plots.Rmd**: generates html document with plots from specified folder (inputted as a parameter)
	- **shiny_results_plots.Rmd**: generates shiny interactive document with plots from specified folder

### Miscellaneous

- **numerical_checks.R**: functions to conduct common numerical checks
	- **isEqual()**: function to check for equality between two matrices A and B up to given numerical tolerance
	- **isOrthonormal()**: function to check if matrix is orthonormal up to given numerical tolerance




