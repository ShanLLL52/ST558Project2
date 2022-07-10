ST558 Project2 Group10 README
================
Shan Luo, Chengxi Zou
2022-07-08

This repo is for ST558 Project2. We are trying to use [online new
popularity date
set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity) to
visualize data, fit four models, and comparing them for six different
data channels.

# Code to render README

``` r
rmarkdown::render("ST558_Project2_Group10_README.Rmd", 
          output_format = "github_document",
          output_file = "README.md",
          output_options = list(
            html_preview = FALSE))
```

# Code to render Analysis Rmd

``` r
channels <- c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world")
# Create file names
name <- c("Lifestyle", "Entertainment", "Business", "SocialMedia",
          "Tech", "World")
output_file <- paste0(name, "Analysis.md")
# Create a list for each channel with just channel name parameter
params <- lapply(channels, FUN = function(x){
  list(Channels = x)
})
# Put into a data frame
reports <- tibble::tibble(output_file, params)
# Automation
apply(reports, MARGIN = 1, FUN = function(x) {
  rmarkdown::render(input = "ST558_Project2_Group10.Rmd", 
                    output_format = "github_document", 
                    output_file = x[[1]], 
                    params = x[[2]], 
                    output_options = list(html_preview = FALSE)) 
})
```

# Package List

The Following Packages are used in this project:

[`tidyverse`](https://www.tidyverse.org)  
[`ggplot2`](https://ggplot2.tidyverse.org)  
[`caret`](https://topepo.github.io/caret/)  
[`rmarkdown`](https://cran.r-project.org/web/packages/rmarkdown/index.html)  
[`tibble`](https://cran.r-project.org/web/packages/tibble/index.html)

# Analysis for Each Channel

The analysis for [Lifestyle articles is available
here](LifestyleAnalysis.md).

The analysis for [Entertainment articles is available
here](EntertainmentAnalysis.md).

The analysis for [Business articles is available
here](BusinessAnalysis.md).

The analysis for [Social Media articles is available
here](SocialMediaAnalysis.md).

The analysis for [Tech articles is available here](TechAnalysis.md).

The analysis for [World articles is available here](WorldAnalysis.md).
