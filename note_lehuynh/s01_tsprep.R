#'---
#' title: PREPARE TIME SERIES DATA
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
  rio,            # import and export files
  here,           # locate files 
  tidyverse      # data management and visualization
)

#' # LOAD THE ORIGINAL MORTALITY DATA 
# original data #-----------
(data_orig <- rio::import(here("data/lndmsoadeath.csv")) %>% 
  tibble())

(dataorig <- data_orig %>% 
  rename(year = Year,
         month = Month,
         day = Day,
         MSOA11CD = `MSOA Code`,
         MSOA11NM = `MSOA Name`,
         d074 = `0-74`,
         d75plus = `75+`) %>%
  mutate(date = date(paste(year, month, day, sep = "/"))))

#' Check data
dataorig %>% count(year)
dataorig %>% count(year, month)
dataorig %>% count(day) %>% pull(day)

dataorig %>% count(MSOA11CD)
dataorig %>% count(MSOA11NM)
dataorig %>% count(MSOA11CD, MSOA11NM)

dataorig %>% nest(.by = MSOA11CD) %>% arrange(MSOA11CD)
dataorig %>% nest(.by = MSOA11CD) %>% arrange(MSOA11CD) %>% pluck("data", 1)

#' # PREPARE THE CASE TIME SERIES DATASET (STRATIFIED BY MSOA)
# working data #-------------------------
(seqdate <- dataorig %>% distinct(date) %>% pull())

#' ## Test code for one MSOA11CD
dataorig %>% 
  select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
  arrange(MSOA11CD, date) %>% 
  nest(.by = c(MSOA11CD, MSOA11NM))

dataorig %>% 
  select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
  arrange(MSOA11CD, date) %>% 
  nest(.by = c(MSOA11CD, MSOA11NM)) %>% 
  pluck("data", 1)
  
dataorig %>% 
    select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
    arrange(MSOA11CD, date) %>% 
    nest(.by = c(MSOA11CD, MSOA11NM)) %>% 
    pluck("data", 1) %>% 
    tidyr::complete(date = seqdate,
                    fill = list(d074 = NA,
                                d75plus = NA)) %>% 
    print(n = 32)

#' ## Generate case time series data for all MSOA11CD
(datafull <- dataorig %>% 
  select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
  arrange(MSOA11CD, date) %>% 
  nest(.by = c(MSOA11CD, MSOA11NM)) %>% 
  mutate(fill_NAdate = map(.x = data,
                           \(data) data %>%
                             # fill NA-date
                             tidyr::complete(date = seqdate,
                                             fill = list(d074 = 0,
                                                         d75plus = 0)) %>% 
                             mutate(year = lubridate::year(date),
                                    month = lubridate::month(date),
                                    day = lubridate::day(date),
                                    doy = lubridate::yday(date),
                                    dow = lubridate::wday(date),
                                    dtot = d074 + d75plus)
                           )) %>% 
  select(-data) %>% 
  unnest(fill_NAdate))

#' **Note**: `datafull_temperature.csv`: join temperature data using the original code
#---

#' # Load the aggregated data
# aggregated data #-----------
#' `data_aggregated_temperature.csv`: get aggregated data and 
#' join temperature data using the original code

(data_aggr <- rio::import(here("note_lehuynh/data_aggregated_temperature.csv")) %>% 
   tibble())

#' Compared with `datafull` → the aggregated data are the sum of deaths for all of London.
datafull %>% 
  nest(.by = date) %>% 
  mutate(d074_aggr = map_dbl(data,
                                   \(data) data %>% pull(d074) %>% sum()),
         d75plus_aggr = map_dbl(data,
                                   \(data) data %>% pull(d75plus) %>% sum()),
         dtot_aggr = map_dbl(data,
                                   \(data) data %>% pull(dtot) %>% sum()))

