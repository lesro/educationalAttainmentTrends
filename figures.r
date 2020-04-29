library(magrittr)
library(estimatr)
library(tidyverse)
library(flextable)
library(officer)
select <- dplyr::select
source('R/trends.r')


load('output/estByYear.RData')

overTime <- processEsts(overTime)

for(lev in c('hs','cc','bach')){

