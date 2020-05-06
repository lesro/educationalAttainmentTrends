library(magrittr)
library(estimatr)
library(tidyverse)
library(flextable)
library(officer)
###library(directlabels)
library(ggrepel)
library(knitr)
library(rmarkdown)
select <- dplyr::select
source('R/trends.r')


##### NOT RUN
##### estimate year-by-year attainment rates
# source('R/makeAndest.r')

##### make tables
render('tables.Rmd')


##### make figures
source('figures.r')
