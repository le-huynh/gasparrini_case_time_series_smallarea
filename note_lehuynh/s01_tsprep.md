PREPARE TIME SERIES DATA
================

``` r
pacman::p_load(
  rio,            # import and export files
  here,           # locate files 
  tidyverse      # data management and visualization
)
```

# LOAD THE ORIGINAL MORTALITY DATA

``` r
# original data #-----------
(data_orig <- rio::import(here("data/lndmsoadeath.csv")) %>% 
  tibble())
```

    ## # A tibble: 21,445 × 7
    ##     Year Month   Day `MSOA Code` `MSOA Name`              `0-74` `75+`
    ##    <int> <int> <int> <chr>       <chr>                     <int> <int>
    ##  1  2006     6     1 E02000004   Barking and Dagenham 003      0     1
    ##  2  2006     6     1 E02000005   Barking and Dagenham 004      1     0
    ##  3  2006     6     1 E02000022   Barking and Dagenham 021      1     0
    ##  4  2006     6     1 E02000025   Barnet 002                    1     0
    ##  5  2006     6     1 E02000043   Barnet 020                    0     1
    ##  6  2006     6     1 E02000045   Barnet 022                    1     0
    ##  7  2006     6     1 E02000050   Barnet 027                    1     0
    ##  8  2006     6     1 E02000052   Barnet 029                    0     1
    ##  9  2006     6     1 E02000059   Barnet 036                    0     1
    ## 10  2006     6     1 E02000067   Bexley 003                    0     1
    ## # ℹ 21,435 more rows

``` r
(dataorig <- data_orig %>% 
  rename(year = Year,
         month = Month,
         day = Day,
         MSOA11CD = `MSOA Code`,
         MSOA11NM = `MSOA Name`,
         d074 = `0-74`,
         d75plus = `75+`) %>%
  mutate(date = date(paste(year, month, day, sep = "/"))))
```

    ## # A tibble: 21,445 × 8
    ##     year month   day MSOA11CD  MSOA11NM                  d074 d75plus date      
    ##    <int> <int> <int> <chr>     <chr>                    <int>   <int> <date>    
    ##  1  2006     6     1 E02000004 Barking and Dagenham 003     0       1 2006-06-01
    ##  2  2006     6     1 E02000005 Barking and Dagenham 004     1       0 2006-06-01
    ##  3  2006     6     1 E02000022 Barking and Dagenham 021     1       0 2006-06-01
    ##  4  2006     6     1 E02000025 Barnet 002                   1       0 2006-06-01
    ##  5  2006     6     1 E02000043 Barnet 020                   0       1 2006-06-01
    ##  6  2006     6     1 E02000045 Barnet 022                   1       0 2006-06-01
    ##  7  2006     6     1 E02000050 Barnet 027                   1       0 2006-06-01
    ##  8  2006     6     1 E02000052 Barnet 029                   0       1 2006-06-01
    ##  9  2006     6     1 E02000059 Barnet 036                   0       1 2006-06-01
    ## 10  2006     6     1 E02000067 Bexley 003                   0       1 2006-06-01
    ## # ℹ 21,435 more rows

Check data

``` r
dataorig %>% count(year)
```

    ## # A tibble: 2 × 2
    ##    year     n
    ##   <int> <int>
    ## 1  2006 11355
    ## 2  2013 10090

``` r
dataorig %>% count(year, month)
```

    ## # A tibble: 6 × 3
    ##    year month     n
    ##   <int> <int> <int>
    ## 1  2006     6  3755
    ## 2  2006     7  3923
    ## 3  2006     8  3677
    ## 4  2013     6  3450
    ## 5  2013     7  3388
    ## 6  2013     8  3252

``` r
dataorig %>% count(day) %>% pull(day)
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31

``` r
dataorig %>% count(MSOA11CD)
```

    ## # A tibble: 983 × 2
    ##    MSOA11CD      n
    ##    <chr>     <int>
    ##  1 E02000001    16
    ##  2 E02000002    31
    ##  3 E02000003    29
    ##  4 E02000004    35
    ##  5 E02000005    18
    ##  6 E02000007    33
    ##  7 E02000008    47
    ##  8 E02000009    22
    ##  9 E02000010    22
    ## 10 E02000011    33
    ## # ℹ 973 more rows

``` r
dataorig %>% count(MSOA11NM)
```

    ## # A tibble: 983 × 2
    ##    MSOA11NM                     n
    ##    <chr>                    <int>
    ##  1 Barking and Dagenham 001    31
    ##  2 Barking and Dagenham 002    29
    ##  3 Barking and Dagenham 003    35
    ##  4 Barking and Dagenham 004    18
    ##  5 Barking and Dagenham 006    33
    ##  6 Barking and Dagenham 007    47
    ##  7 Barking and Dagenham 008    22
    ##  8 Barking and Dagenham 009    22
    ##  9 Barking and Dagenham 010    33
    ## 10 Barking and Dagenham 011    32
    ## # ℹ 973 more rows

``` r
dataorig %>% count(MSOA11CD, MSOA11NM)
```

    ## # A tibble: 983 × 3
    ##    MSOA11CD  MSOA11NM                     n
    ##    <chr>     <chr>                    <int>
    ##  1 E02000001 City of London 001          16
    ##  2 E02000002 Barking and Dagenham 001    31
    ##  3 E02000003 Barking and Dagenham 002    29
    ##  4 E02000004 Barking and Dagenham 003    35
    ##  5 E02000005 Barking and Dagenham 004    18
    ##  6 E02000007 Barking and Dagenham 006    33
    ##  7 E02000008 Barking and Dagenham 007    47
    ##  8 E02000009 Barking and Dagenham 008    22
    ##  9 E02000010 Barking and Dagenham 009    22
    ## 10 E02000011 Barking and Dagenham 010    33
    ## # ℹ 973 more rows

``` r
dataorig %>% nest(.by = MSOA11CD) %>% arrange(MSOA11CD)
```

    ## # A tibble: 983 × 2
    ##    MSOA11CD  data             
    ##    <chr>     <list>           
    ##  1 E02000001 <tibble [16 × 7]>
    ##  2 E02000002 <tibble [31 × 7]>
    ##  3 E02000003 <tibble [29 × 7]>
    ##  4 E02000004 <tibble [35 × 7]>
    ##  5 E02000005 <tibble [18 × 7]>
    ##  6 E02000007 <tibble [33 × 7]>
    ##  7 E02000008 <tibble [47 × 7]>
    ##  8 E02000009 <tibble [22 × 7]>
    ##  9 E02000010 <tibble [22 × 7]>
    ## 10 E02000011 <tibble [33 × 7]>
    ## # ℹ 973 more rows

``` r
dataorig %>% nest(.by = MSOA11CD) %>% arrange(MSOA11CD) %>% pluck("data", 1)
```

    ## # A tibble: 16 × 7
    ##     year month   day MSOA11NM            d074 d75plus date      
    ##    <int> <int> <int> <chr>              <int>   <int> <date>    
    ##  1  2006     6    10 City of London 001     0       1 2006-06-10
    ##  2  2006     6    28 City of London 001     1       0 2006-06-28
    ##  3  2006     6    30 City of London 001     0       1 2006-06-30
    ##  4  2006     7    16 City of London 001     0       1 2006-07-16
    ##  5  2006     7    24 City of London 001     0       1 2006-07-24
    ##  6  2006     8     2 City of London 001     1       0 2006-08-02
    ##  7  2006     8     4 City of London 001     0       1 2006-08-04
    ##  8  2006     8     6 City of London 001     0       1 2006-08-06
    ##  9  2006     8    11 City of London 001     0       1 2006-08-11
    ## 10  2006     8    12 City of London 001     1       0 2006-08-12
    ## 11  2006     8    14 City of London 001     0       1 2006-08-14
    ## 12  2006     8    19 City of London 001     0       1 2006-08-19
    ## 13  2013     7     5 City of London 001     1       0 2013-07-05
    ## 14  2013     7    17 City of London 001     1       0 2013-07-17
    ## 15  2013     7    22 City of London 001     0       1 2013-07-22
    ## 16  2013     7    24 City of London 001     1       0 2013-07-24

# PREPARE THE CASE TIME SERIES DATASET (STRATIFIED BY MSOA)

``` r
# working data #-------------------------
(seqdate <- dataorig %>% distinct(date) %>% pull())
```

    ##   [1] "2006-06-01" "2006-06-02" "2006-06-03" "2006-06-04" "2006-06-05" "2006-06-06" "2006-06-07"
    ##   [8] "2006-06-08" "2006-06-09" "2006-06-10" "2006-06-11" "2006-06-12" "2006-06-13" "2006-06-14"
    ##  [15] "2006-06-15" "2006-06-16" "2006-06-17" "2006-06-18" "2006-06-19" "2006-06-20" "2006-06-21"
    ##  [22] "2006-06-22" "2006-06-23" "2006-06-24" "2006-06-25" "2006-06-26" "2006-06-27" "2006-06-28"
    ##  [29] "2006-06-29" "2006-06-30" "2006-07-01" "2006-07-02" "2006-07-03" "2006-07-04" "2006-07-05"
    ##  [36] "2006-07-06" "2006-07-07" "2006-07-08" "2006-07-09" "2006-07-10" "2006-07-11" "2006-07-12"
    ##  [43] "2006-07-13" "2006-07-14" "2006-07-15" "2006-07-16" "2006-07-17" "2006-07-18" "2006-07-19"
    ##  [50] "2006-07-20" "2006-07-21" "2006-07-22" "2006-07-23" "2006-07-24" "2006-07-25" "2006-07-26"
    ##  [57] "2006-07-27" "2006-07-28" "2006-07-29" "2006-07-30" "2006-07-31" "2006-08-01" "2006-08-02"
    ##  [64] "2006-08-03" "2006-08-04" "2006-08-05" "2006-08-06" "2006-08-07" "2006-08-08" "2006-08-09"
    ##  [71] "2006-08-10" "2006-08-11" "2006-08-12" "2006-08-13" "2006-08-14" "2006-08-15" "2006-08-16"
    ##  [78] "2006-08-17" "2006-08-18" "2006-08-19" "2006-08-20" "2006-08-21" "2006-08-22" "2006-08-23"
    ##  [85] "2006-08-24" "2006-08-25" "2006-08-26" "2006-08-27" "2006-08-28" "2006-08-29" "2006-08-30"
    ##  [92] "2006-08-31" "2013-06-01" "2013-06-02" "2013-06-03" "2013-06-04" "2013-06-05" "2013-06-06"
    ##  [99] "2013-06-07" "2013-06-08" "2013-06-09" "2013-06-10" "2013-06-11" "2013-06-12" "2013-06-13"
    ## [106] "2013-06-14" "2013-06-15" "2013-06-16" "2013-06-17" "2013-06-18" "2013-06-19" "2013-06-20"
    ## [113] "2013-06-21" "2013-06-22" "2013-06-23" "2013-06-24" "2013-06-25" "2013-06-26" "2013-06-27"
    ## [120] "2013-06-28" "2013-06-29" "2013-06-30" "2013-07-01" "2013-07-02" "2013-07-03" "2013-07-04"
    ## [127] "2013-07-05" "2013-07-06" "2013-07-07" "2013-07-08" "2013-07-09" "2013-07-10" "2013-07-11"
    ## [134] "2013-07-12" "2013-07-13" "2013-07-14" "2013-07-15" "2013-07-16" "2013-07-17" "2013-07-18"
    ## [141] "2013-07-19" "2013-07-20" "2013-07-21" "2013-07-22" "2013-07-23" "2013-07-24" "2013-07-25"
    ## [148] "2013-07-26" "2013-07-27" "2013-07-28" "2013-07-29" "2013-07-30" "2013-07-31" "2013-08-01"
    ## [155] "2013-08-02" "2013-08-03" "2013-08-04" "2013-08-05" "2013-08-06" "2013-08-07" "2013-08-08"
    ## [162] "2013-08-09" "2013-08-10" "2013-08-11" "2013-08-12" "2013-08-13" "2013-08-14" "2013-08-15"
    ## [169] "2013-08-16" "2013-08-17" "2013-08-18" "2013-08-19" "2013-08-20" "2013-08-21" "2013-08-22"
    ## [176] "2013-08-23" "2013-08-24" "2013-08-25" "2013-08-26" "2013-08-27" "2013-08-28" "2013-08-29"
    ## [183] "2013-08-30" "2013-08-31"

## Test code for one MSOA11CD

``` r
dataorig %>% 
  select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
  arrange(MSOA11CD, date) %>% 
  nest(.by = c(MSOA11CD, MSOA11NM))
```

    ## # A tibble: 983 × 3
    ##    MSOA11CD  MSOA11NM                 data             
    ##    <chr>     <chr>                    <list>           
    ##  1 E02000001 City of London 001       <tibble [16 × 3]>
    ##  2 E02000002 Barking and Dagenham 001 <tibble [31 × 3]>
    ##  3 E02000003 Barking and Dagenham 002 <tibble [29 × 3]>
    ##  4 E02000004 Barking and Dagenham 003 <tibble [35 × 3]>
    ##  5 E02000005 Barking and Dagenham 004 <tibble [18 × 3]>
    ##  6 E02000007 Barking and Dagenham 006 <tibble [33 × 3]>
    ##  7 E02000008 Barking and Dagenham 007 <tibble [47 × 3]>
    ##  8 E02000009 Barking and Dagenham 008 <tibble [22 × 3]>
    ##  9 E02000010 Barking and Dagenham 009 <tibble [22 × 3]>
    ## 10 E02000011 Barking and Dagenham 010 <tibble [33 × 3]>
    ## # ℹ 973 more rows

``` r
dataorig %>% 
  select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
  arrange(MSOA11CD, date) %>% 
  nest(.by = c(MSOA11CD, MSOA11NM)) %>% 
  pluck("data", 1)
```

    ## # A tibble: 16 × 3
    ##    date        d074 d75plus
    ##    <date>     <int>   <int>
    ##  1 2006-06-10     0       1
    ##  2 2006-06-28     1       0
    ##  3 2006-06-30     0       1
    ##  4 2006-07-16     0       1
    ##  5 2006-07-24     0       1
    ##  6 2006-08-02     1       0
    ##  7 2006-08-04     0       1
    ##  8 2006-08-06     0       1
    ##  9 2006-08-11     0       1
    ## 10 2006-08-12     1       0
    ## 11 2006-08-14     0       1
    ## 12 2006-08-19     0       1
    ## 13 2013-07-05     1       0
    ## 14 2013-07-17     1       0
    ## 15 2013-07-22     0       1
    ## 16 2013-07-24     1       0

``` r
dataorig %>% 
    select(MSOA11CD, MSOA11NM, date, d074, d75plus) %>% 
    arrange(MSOA11CD, date) %>% 
    nest(.by = c(MSOA11CD, MSOA11NM)) %>% 
    pluck("data", 1) %>% 
    tidyr::complete(date = seqdate,
                    fill = list(d074 = NA,
                                d75plus = NA)) %>% 
    print(n = 32)
```

    ## # A tibble: 184 × 3
    ##    date        d074 d75plus
    ##    <date>     <int>   <int>
    ##  1 2006-06-01    NA      NA
    ##  2 2006-06-02    NA      NA
    ##  3 2006-06-03    NA      NA
    ##  4 2006-06-04    NA      NA
    ##  5 2006-06-05    NA      NA
    ##  6 2006-06-06    NA      NA
    ##  7 2006-06-07    NA      NA
    ##  8 2006-06-08    NA      NA
    ##  9 2006-06-09    NA      NA
    ## 10 2006-06-10     0       1
    ## 11 2006-06-11    NA      NA
    ## 12 2006-06-12    NA      NA
    ## 13 2006-06-13    NA      NA
    ## 14 2006-06-14    NA      NA
    ## 15 2006-06-15    NA      NA
    ## 16 2006-06-16    NA      NA
    ## 17 2006-06-17    NA      NA
    ## 18 2006-06-18    NA      NA
    ## 19 2006-06-19    NA      NA
    ## 20 2006-06-20    NA      NA
    ## 21 2006-06-21    NA      NA
    ## 22 2006-06-22    NA      NA
    ## 23 2006-06-23    NA      NA
    ## 24 2006-06-24    NA      NA
    ## 25 2006-06-25    NA      NA
    ## 26 2006-06-26    NA      NA
    ## 27 2006-06-27    NA      NA
    ## 28 2006-06-28     1       0
    ## 29 2006-06-29    NA      NA
    ## 30 2006-06-30     0       1
    ## 31 2006-07-01    NA      NA
    ## 32 2006-07-02    NA      NA
    ## # ℹ 152 more rows

## Generate case time series data for all MSOA11CD

``` r
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
```

    ## # A tibble: 180,872 × 11
    ##    MSOA11CD  MSOA11NM           date        d074 d75plus  year month   day   doy   dow  dtot
    ##    <chr>     <chr>              <date>     <int>   <int> <dbl> <dbl> <int> <dbl> <dbl> <int>
    ##  1 E02000001 City of London 001 2006-06-01     0       0  2006     6     1   152     5     0
    ##  2 E02000001 City of London 001 2006-06-02     0       0  2006     6     2   153     6     0
    ##  3 E02000001 City of London 001 2006-06-03     0       0  2006     6     3   154     7     0
    ##  4 E02000001 City of London 001 2006-06-04     0       0  2006     6     4   155     1     0
    ##  5 E02000001 City of London 001 2006-06-05     0       0  2006     6     5   156     2     0
    ##  6 E02000001 City of London 001 2006-06-06     0       0  2006     6     6   157     3     0
    ##  7 E02000001 City of London 001 2006-06-07     0       0  2006     6     7   158     4     0
    ##  8 E02000001 City of London 001 2006-06-08     0       0  2006     6     8   159     5     0
    ##  9 E02000001 City of London 001 2006-06-09     0       0  2006     6     9   160     6     0
    ## 10 E02000001 City of London 001 2006-06-10     0       1  2006     6    10   161     7     1
    ## # ℹ 180,862 more rows

**Note**: `datafull_temperature.csv`: join temperature data using the
original code

``` r
#---
```

# Load the aggregated data

``` r
# aggregated data #-----------
```

`data_aggregated_temperature.csv`: get aggregated data and join
temperature data using the original code

``` r
(data_aggr <- rio::import(here("note_lehuynh/data_aggregated_temperature.csv")) %>% 
   tibble())
```

    ## # A tibble: 184 × 10
    ##    date        d074 d75plus  dtot  year month   day   doy   dow tmean
    ##    <IDate>    <int>   <int> <int> <int> <int> <int> <int> <int> <dbl>
    ##  1 2006-06-01    45      72   117  2006     6     1   152     5  10.9
    ##  2 2006-06-02    51     108   159  2006     6     2   153     6  14.6
    ##  3 2006-06-03    45      85   130  2006     6     3   154     7  16.4
    ##  4 2006-06-04    51      88   139  2006     6     4   155     1  16.4
    ##  5 2006-06-05    55      74   129  2006     6     5   156     2  15.9
    ##  6 2006-06-06    56      78   134  2006     6     6   157     3  14.4
    ##  7 2006-06-07    58      78   136  2006     6     7   158     4  18.0
    ##  8 2006-06-08    62      74   136  2006     6     8   159     5  19.8
    ##  9 2006-06-09    61      82   143  2006     6     9   160     6  19.2
    ## 10 2006-06-10    57      95   152  2006     6    10   161     7  21.1
    ## # ℹ 174 more rows

Compared with `datafull` → the aggregated data are the sum of deaths for
all of London.

``` r
datafull %>% 
  nest(.by = date) %>% 
  mutate(d074_aggr = map_dbl(data,
                                   \(data) data %>% pull(d074) %>% sum()),
         d75plus_aggr = map_dbl(data,
                                   \(data) data %>% pull(d75plus) %>% sum()),
         dtot_aggr = map_dbl(data,
                                   \(data) data %>% pull(dtot) %>% sum()))
```

    ## # A tibble: 184 × 5
    ##    date       data                d074_aggr d75plus_aggr dtot_aggr
    ##    <date>     <list>                  <dbl>        <dbl>     <dbl>
    ##  1 2006-06-01 <tibble [983 × 10]>        45           72       117
    ##  2 2006-06-02 <tibble [983 × 10]>        51          108       159
    ##  3 2006-06-03 <tibble [983 × 10]>        45           85       130
    ##  4 2006-06-04 <tibble [983 × 10]>        51           88       139
    ##  5 2006-06-05 <tibble [983 × 10]>        55           74       129
    ##  6 2006-06-06 <tibble [983 × 10]>        56           78       134
    ##  7 2006-06-07 <tibble [983 × 10]>        58           78       136
    ##  8 2006-06-08 <tibble [983 × 10]>        62           74       136
    ##  9 2006-06-09 <tibble [983 × 10]>        61           82       143
    ## 10 2006-06-10 <tibble [983 × 10]>        57           95       152
    ## # ℹ 174 more rows
