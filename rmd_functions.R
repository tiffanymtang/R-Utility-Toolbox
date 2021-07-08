# rmarkdown helper functions

subchunkify <- function(g, i, fig_height = 12, fig_width = 10, caption = "''",
                        other_args = "") {
  ####### Function Description ########
  # function to allow for multiple plots of different sizes and captions
  # within a single R code chunk
  #
  # code adapted from http://michaeljw.com/blog/post/subchunkify/
  # 
  # inputs:
  # - g = plot
  # - i = chunk id (should be unique for each plot)
  # - fig_height = height of figure
  # - fig_width = width of figure
  # - caption = figure caption; should be within surrounded by two sets of
  #     quotes, e.g., "'This is a valid caption.'"
  # - other_args = other arguments to pass to R code chunk header
  ####### 
  
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  
  if (!identical(other_args, "")) {
    if (!startsWith(other_args, ",")) {
      other_args <- paste0(", ", other_args)
    }
  }
  
  sub_chunk <- paste0("
  `","``{r subchunk_", i, 
                      ", fig.height=", fig_height, ", fig.width=", fig_width, 
                      ", fig.cap=", caption,", echo=FALSE", other_args, "}",
                      "\n(", g_deparsed, ")()",
                      "\n`","``
  ")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}