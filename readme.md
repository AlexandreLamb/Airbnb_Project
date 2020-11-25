# Airbnb Data Analyse Project

Quick overview of what it's good to know about the project

### Link to an shiny dashboard (with only one city du to storage limits)
[Shiny dashboard link](https://alexandrelambert.shinyapps.io/project/)

### Library need
  - library(dplyr)
  - library(stringr)
  - library(ggplot2)
  - library(data.table)
  - library(knitr)
  - library(rmarkdown)
  - library(shiny)
  - library(reshape2)
  - library(htmltools)
  - library(leaflet)
  - library(datasets)
  - library(tidyr)
  - library(R.utils)
  - library(lubridate)
  - library(log4r)


### Command to run the Rmd shiny dashboard (from app folder)(in commande line)
```sh
Rscript -e "rmarkdown::run('./dashboard.Rmd')"
```

### Architecture project
The main file is dashbaord.Rmd, a shiny flex_dashboard [see](https://rmarkdown.rstudio.com/flexdashboard/shiny.html)
You can found tab with graph and analisys, you can alos use the input tools on the left side to change the filter
There is 4 file who's need with the dashboard.Rmd plus the data:
- data_preparation.R (script who prepare the data)
- analize_data.R (script who make some analysis on the data for use them in the shiny app)
- configuration.R (file with R variable that are frequently use in the project)
- log_utils.R (file who have function to make some log)
