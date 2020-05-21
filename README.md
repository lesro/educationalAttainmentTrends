# Replication code for "Change Over Time in Educational Attainment for Deaf Individuals from 2008-2018"

To replicate:

- Download CSV files from https://www2.census.gov/programs-surveys/acs/data/pums/
  - 1-year files
  - years 2008-2018
  - save all CSV files in the same folder, with names ss[YR]pusa.csv or ss[YR]pusb.csv e.g. ss08pusa.csv

- Install packages in `R`:
  - magrittr
  - estimatr
  - tidyverse
  - flextable
  - officer
  - ggrepel
  - knitr
  - rmarkdown
  - survey

- Modify "dataFolder" from R/makeAndest.r
- Run `source('R/makeAndest.r')`
- To replicate tables and figures from the paper (modulo a small bit of formatting): run `rmarkdown::render('tables.Rmd')`
- To replicate supplemental tables (in Tables/ folder) run `source('R/onlineSuppTables.r')`

