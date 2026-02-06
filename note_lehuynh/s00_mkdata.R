#'---
#' title: Generate data for analysis
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
  rio,            # import and export files
  here,           # locate files 
  tidyverse,      # data management and visualization
  chva.extras     # supplementary functions
)

#' # Data
# data #-----------
(lndmsoadeath <- rio::import(here("data_raw/numberofdailydeathsbymiddlesuperoutputareasmsoasoflondonjuly2006july2013andjuly2016.xls"),
                             sheet = 2,
                             skip = 9) %>% 
  tibble())

seqmsoa <- lndmsoadeath %>% 
  distinct(`MSOA Code`) %>% 
  arrange(`MSOA Code`) %>% 
  pull(`MSOA Code`)

head(seqmsoa)

