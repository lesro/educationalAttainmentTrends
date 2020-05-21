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



load('output/estByYear.RData')
load('output/ageDist18.RData')

overTimeAge <- processEsts(overTimeAge,ageDist18)

overTime <- processEsts(overTime)

subs <- c('Tot',
          'ByAgeCat',
          'Sex','Race','RaceM','RaceF')


##### make tables
render('tables.Rmd')


##### make figures
source('figures.r')
