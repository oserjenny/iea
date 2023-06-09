Citizenship Norms 1999-2009-2016
================
Jennifer Oser
Updated May 27, 2023

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --
    ## v ggplot2 3.4.0     v purrr   0.3.4
    ## v tibble  3.1.8     v dplyr   1.0.9
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1
    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(purrr)
library(writexl)

sessionInfo()
```

    ## R version 4.1.3 (2022-03-10)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] writexl_1.4.0   forcats_0.5.1   stringr_1.4.0   dplyr_1.0.9    
    ##  [5] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.8   
    ##  [9] ggplot2_3.4.0   tidyverse_1.3.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.2    xfun_0.32           haven_2.5.0        
    ##  [4] gargle_1.2.0        colorspace_2.0-3    vctrs_0.5.2        
    ##  [7] generics_0.1.3      htmltools_0.5.3     yaml_2.3.5         
    ## [10] utf8_1.2.2          rlang_1.0.6         pillar_1.8.0       
    ## [13] withr_2.5.0         glue_1.6.2          DBI_1.1.3          
    ## [16] dbplyr_2.2.1        modelr_0.1.8        readxl_1.4.0       
    ## [19] lifecycle_1.0.3     munsell_0.5.0       gtable_0.3.0       
    ## [22] cellranger_1.1.0    rvest_1.0.2         evaluate_0.16      
    ## [25] knitr_1.39          tzdb_0.3.0          fastmap_1.1.0      
    ## [28] fansi_1.0.3         broom_1.0.0         scales_1.2.0       
    ## [31] backports_1.4.1     googlesheets4_1.0.1 jsonlite_1.8.0     
    ## [34] fs_1.5.2            hms_1.1.1           digest_0.6.29      
    ## [37] stringi_1.7.6       grid_4.1.3          cli_3.3.0          
    ## [40] tools_4.1.3         magrittr_2.0.3      crayon_1.5.1       
    ## [43] pkgconfig_2.0.3     ellipsis_0.3.2      xml2_1.3.3         
    ## [46] reprex_2.0.1        googledrive_2.0.0   lubridate_1.8.0    
    ## [49] assertthat_0.2.1    rmarkdown_2.14      httr_1.4.3         
    ## [52] rstudioapi_0.13     R6_2.5.1            compiler_4.1.3

## Introduction

This file documents the data cleaning for the IEA 1999-2009-2016 data

For citizenship norm recodes in all three survey waves, the norms are
coded in the descending mean order of the 1999 data:
obey,rights,local,work,envir,vote,history,respect,news,protest,discuss,party

## 1999 data loading and merging

1999 data: <https://doi.org/10.3886/ICPSR21661.v3> Downloaded Jan 17,
2019 Title: IEA Civic Education Study, 1999: Civic Knowledge and
Engagement Among 14-Year-Olds in 23 European Countries, 2 Latin American
Countries, Hong Kong, Australia, and the United States (ICPSR 21661)

Load 1999 country files, in chronological order of file names. Bind all
1999 files. Total observations of resulting tbl1 (93,882) concur with
xls documentation of expected total n.

``` r
# all files
files <- list.files("../data", full.names = TRUE)

# helper function to load files
load_files <- function(file) {
  e <- new.env()
  load(file, envir = e)
  
  stopifnot(length(e) == 1)  # safety first
  
  get(names(e)[1], envir = e)
}

tbl1 <- files %>%
  magrittr::extract(1:28) %>%      # filter to 1999 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, BS3B1, BS3B11, BS3B9, BS3B4, BS3B13,
               BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B12, BS3B3, GENDER,BSGBOOK, EXPEDUC, BSGEDUM, BSGEDUF, TOTWGT)) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  mutate(`ICCS_year` = 1999) %>%     # add survey year variable
  select(`ICCS_year`, everything())

tbl1 <- tbl1 %>%
    mutate(
      across(where(is.factor), fct_explicit_na)
    )
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl1 %>% 
  select(BS3B1, BS3B11, BS3B9, BS3B4, BS3B13, BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B12, BS3B3) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl1 %>% count(!!sym(.x))) 
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   BS3B1                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         1995
    ## 2 (2) somewhat unimportant  2418
    ## 3 (3) somewhat important   20211
    ## 4 (4) very important       66431
    ## 5 (Missing)                 2827
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   BS3B11                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         2975
    ## 2 (2) somewhat unimportant 10785
    ## 3 (3) somewhat important   35630
    ## 4 (4) very important       38503
    ## 5 (Missing)                 5989
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   BS3B9                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         2986
    ## 2 (2) somewhat unimportant 11726
    ## 3 (3) somewhat important   42796
    ## 4 (4) very important       31396
    ## 5 (Missing)                 4978
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   BS3B4                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         5447
    ## 2 (2) somewhat unimportant 11830
    ## 3 (3) somewhat important   35762
    ## 4 (4) very important       35769
    ## 5 (Missing)                 5074
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   BS3B13                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         4064
    ## 2 (2) somewhat unimportant 12865
    ## 3 (3) somewhat important   36910
    ## 4 (4) very important       35383
    ## 5 (Missing)                 4660
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   BS3B2                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         4566
    ## 2 (2) somewhat unimportant 13382
    ## 3 (3) somewhat important   37364
    ## 4 (4) very important       35023
    ## 5 (Missing)                 3547
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   BS3B6                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         7861
    ## 2 (2) somewhat unimportant 17428
    ## 3 (3) somewhat important   32388
    ## 4 (4) very important       31589
    ## 5 (Missing)                 4616
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   BS3B10                       n
    ##   <fct>                    <int>
    ## 1 (1) not important         6109
    ## 2 (2) somewhat unimportant 18138
    ## 3 (3) somewhat important   40898
    ## 4 (4) very important       22579
    ## 5 (Missing)                 6158
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   BS3B8                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         7319
    ## 2 (2) somewhat unimportant 18421
    ## 3 (3) somewhat important   42831
    ## 4 (4) very important       20869
    ## 5 (Missing)                 4442
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   BS3B5                        n
    ##   <fct>                    <int>
    ## 1 (1) not important         8941
    ## 2 (2) somewhat unimportant 18358
    ## 3 (3) somewhat important   34180
    ## 4 (4) very important       22693
    ## 5 (Missing)                 9710
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   BS3B12                       n
    ##   <fct>                    <int>
    ## 1 (1) not importnat        13954
    ## 2 (2) somewhat unimportant 34883
    ## 3 (3) somewhat important   28004
    ## 4 (4) very important        8959
    ## 5 (Missing)                 8082
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   BS3B3                        n
    ##   <fct>                    <int>
    ## 1 (1) not important        23205
    ## 2 (2) somewhat unimportant 36163
    ## 3 (3) somewhat important   19853
    ## 4 (4) very important        6741
    ## 5 (Missing)                 7920

Cit norm, count and recode 1st indicator as example.

``` r
# recode
tbl1 <- tbl1 %>% 
  mutate(BS3B1_binary = fct_collapse(BS3B1, 
  "not important" = c("(1) not important", "(2) somewhat unimportant"),
  "important"     = c("(3) somewhat important", "(4) very important")))

# confirm correct recode
tbl1 %>%
  count(BS3B1, BS3B1_binary) 
```

    ## # A tibble: 5 x 3
    ##   BS3B1                    BS3B1_binary      n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  1995
    ## 2 (2) somewhat unimportant not important  2418
    ## 3 (3) somewhat important   important     20211
    ## 4 (4) very important       important     66431
    ## 5 (Missing)                (Missing)      2827

Repeat for all cit norm indicators. NOTE: BS3B12 recoded separately
below b/c of typo in string variable.

``` r
tbl1 <-tbl1 %>% 
  mutate(
    across(c(BS3B1, BS3B11, BS3B9, BS3B4, BS3B13, BS3B2, BS3B6, BS3B10, BS3B8, BS3B5, BS3B3),
           ~fct_collapse(.x,
                         "not important"= c("(1) not important", "(2) somewhat unimportant"),
                         "important"    = c("(3) somewhat important", "(4) very important")),
           .names = "{.col}_bin"
           )
  )
```

BS3B12 error troubleshoot when included in prior chunk. Count table
output shows that string text of 1st category “importnat” spelled
incorrectly, i.e. “a” and “n” transposed. BS3B12 “mutate” command to
correctly recode with this typo:

``` r
# troubleshoot
tbl1 %>% count(BS3B12) 
```

    ## # A tibble: 5 x 2
    ##   BS3B12                       n
    ##   <fct>                    <int>
    ## 1 (1) not importnat        13954
    ## 2 (2) somewhat unimportant 34883
    ## 3 (3) somewhat important   28004
    ## 4 (4) very important        8959
    ## 5 (Missing)                 8082

``` r
# recode
tbl1 <- tbl1 %>% 
  mutate(BS3B12_bin = fct_collapse(BS3B12, 
                                   "not important" = c("(1) not importnat", "(2) somewhat unimportant"),
                                   "important"     = c("(3) somewhat important", "(4) very important"))
  )
```

Confirm successful mutates for all cit norms indicators.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl1 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   BS3B1                    BS3B1_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  1995
    ## 2 (2) somewhat unimportant not important  2418
    ## 3 (3) somewhat important   important     20211
    ## 4 (4) very important       important     66431
    ## 5 (Missing)                (Missing)      2827
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   BS3B11                   BS3B11_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  2975
    ## 2 (2) somewhat unimportant not important 10785
    ## 3 (3) somewhat important   important     35630
    ## 4 (4) very important       important     38503
    ## 5 (Missing)                (Missing)      5989
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   BS3B9                    BS3B9_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  2986
    ## 2 (2) somewhat unimportant not important 11726
    ## 3 (3) somewhat important   important     42796
    ## 4 (4) very important       important     31396
    ## 5 (Missing)                (Missing)      4978
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   BS3B4                    BS3B4_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  5447
    ## 2 (2) somewhat unimportant not important 11830
    ## 3 (3) somewhat important   important     35762
    ## 4 (4) very important       important     35769
    ## 5 (Missing)                (Missing)      5074
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   BS3B13                   BS3B13_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  4064
    ## 2 (2) somewhat unimportant not important 12865
    ## 3 (3) somewhat important   important     36910
    ## 4 (4) very important       important     35383
    ## 5 (Missing)                (Missing)      4660
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   BS3B2                    BS3B2_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  4566
    ## 2 (2) somewhat unimportant not important 13382
    ## 3 (3) somewhat important   important     37364
    ## 4 (4) very important       important     35023
    ## 5 (Missing)                (Missing)      3547
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   BS3B6                    BS3B6_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  7861
    ## 2 (2) somewhat unimportant not important 17428
    ## 3 (3) somewhat important   important     32388
    ## 4 (4) very important       important     31589
    ## 5 (Missing)                (Missing)      4616
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   BS3B10                   BS3B10_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  6109
    ## 2 (2) somewhat unimportant not important 18138
    ## 3 (3) somewhat important   important     40898
    ## 4 (4) very important       important     22579
    ## 5 (Missing)                (Missing)      6158
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   BS3B8                    BS3B8_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  7319
    ## 2 (2) somewhat unimportant not important 18421
    ## 3 (3) somewhat important   important     42831
    ## 4 (4) very important       important     20869
    ## 5 (Missing)                (Missing)      4442
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   BS3B5                    BS3B5_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important  8941
    ## 2 (2) somewhat unimportant not important 18358
    ## 3 (3) somewhat important   important     34180
    ## 4 (4) very important       important     22693
    ## 5 (Missing)                (Missing)      9710
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   BS3B12                   BS3B12_bin        n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not importnat        not important 13954
    ## 2 (2) somewhat unimportant not important 34883
    ## 3 (3) somewhat important   important     28004
    ## 4 (4) very important       important      8959
    ## 5 (Missing)                (Missing)      8082
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   BS3B3                    BS3B3_bin         n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) not important        not important 23205
    ## 2 (2) somewhat unimportant not important 36163
    ## 3 (3) somewhat important   important     19853
    ## 4 (4) very important       important      6741
    ## 5 (Missing)                (Missing)      7920

Recode of individual-level control vars

``` r
tbl1 <- tbl1 %>% 
  mutate(female = ifelse(GENDER == "(0) Male", 0, 1),    # gender
         books = case_when(                              # books in respondent's home
           BSGBOOK == "(1) None"          ~ 0,
           BSGBOOK == "(2) 1 - 10"        ~ 0,
           BSGBOOK == "(3) 11 - 50"       ~ 1,
           BSGBOOK == "(4) 51 - 100"      ~ 1,
           BSGBOOK == "(5) 101 - 200"     ~ 2,
           BSGBOOK == "(6) More than 200" ~ 3
         ),
         edexp = case_when(                              # expected number of additional educ years
           EXPEDUC == "(0) 0"    ~ 0,
           EXPEDUC == "(1) 1-2"  ~ 0,
           EXPEDUC == "(2) 3-4"  ~ 0,
           EXPEDUC == "(3) 5-6"  ~ 1,
           EXPEDUC == "(4) 7-8"  ~ 1,
           EXPEDUC == "(5) 9-10" ~ 2,
           EXPEDUC == "(6) 10+"  ~ 2
         ),
         ed_mom = case_when(                             # mother education
           BSGEDUM == "(1) no.elem.school"          ~ 0,
           BSGEDUM == "(2) Fin. elem. sch."         ~ 0,
           BSGEDUM == "(3) Fin.s.high sch."         ~ 0,
           BSGEDUM == "(4) Fin.high sch."           ~ 1,
           BSGEDUM == "(5) Sme technic.educ. after" ~ 2,
           BSGEDUM == "(6) sme college,univ."       ~ 2,
           BSGEDUM == "(7) bach. degree"            ~ 2
         ),
         ed_dad = case_when(                             # father education
           BSGEDUF == "(1) No.elem.school"     ~ 0,
           BSGEDUF == "(2) Fin. elem. sch."    ~ 0,
           BSGEDUF == "(3) Fin.s.high sch."    ~ 0,
           BSGEDUF == "(4) Fin.high sch."      ~ 1,
           BSGEDUF == "(5) Sme techn.educ."    ~ 2,
           BSGEDUF == "(6) Sme college, univ." ~ 2,
           BSGEDUF == "(7) Bach. degree"       ~ 2
         )) 

# check recodes
sociodem_vars <- c("GENDER", "BSGBOOK", "EXPEDUC", "BSGEDUM", "BSGEDUF")
recoded_vars  <- c("female", "books", "edexp", "ed_mom", "ed_dad")

map2(recoded_vars, sociodem_vars, ~ tbl1 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 3 x 3
    ##   female GENDER         n
    ##    <dbl> <fct>      <int>
    ## 1      0 (0) Male   45214
    ## 2      1 (1) Female 47882
    ## 3      1 (Missing)    786
    ## 
    ## [[2]]
    ## # A tibble: 7 x 3
    ##   books BSGBOOK               n
    ##   <dbl> <fct>             <int>
    ## 1     0 (1) None           1394
    ## 2     0 (2) 1 - 10         8620
    ## 3     1 (3) 11 - 50       21027
    ## 4     1 (4) 51 - 100      21409
    ## 5     2 (5) 101 - 200     17490
    ## 6     3 (6) More than 200 22963
    ## 7    NA (Missing)           979
    ## 
    ## [[3]]
    ## # A tibble: 8 x 3
    ##   edexp EXPEDUC       n
    ##   <dbl> <fct>     <int>
    ## 1     0 (0) 0      1839
    ## 2     0 (1) 1-2    6705
    ## 3     0 (2) 3-4   23445
    ## 4     1 (3) 5-6   20336
    ## 5     1 (4) 7-8   19971
    ## 6     2 (5) 9-10  13236
    ## 7     2 (6) 10+    6931
    ## 8    NA (Missing)  1419
    ## 
    ## [[4]]
    ## # A tibble: 8 x 3
    ##   ed_mom BSGEDUM                         n
    ##    <dbl> <fct>                       <int>
    ## 1      0 (1) no.elem.school           4003
    ## 2      0 (2) Fin. elem. sch.         10590
    ## 3      0 (3) Fin.s.high sch.         13044
    ## 4      1 (4) Fin.high sch.           20991
    ## 5      2 (5) Sme technic.educ. after  8047
    ## 6      2 (6) sme college,univ.        6366
    ## 7      2 (7) bach. degree            14229
    ## 8     NA (Missing)                   16612
    ## 
    ## [[5]]
    ## # A tibble: 8 x 3
    ##   ed_dad BSGEDUF                    n
    ##    <dbl> <fct>                  <int>
    ## 1      0 (1) No.elem.school      3689
    ## 2      0 (2) Fin. elem. sch.     9783
    ## 3      0 (3) Fin.s.high sch.    12505
    ## 4      1 (4) Fin.high sch.      17891
    ## 5      2 (5) Sme techn.educ.     9309
    ## 6      2 (6) Sme college, univ.  5137
    ## 7      2 (7) Bach. degree       15120
    ## 8     NA (Missing)              20448

Select for LCA vars tibble, including rename all mutated variables and
display first five lines of dataframe.

``` r
tbl1 <- tbl1 %>%
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         TOTWGTS = TOTWGT,
         obey    = BS3B1_bin,
         rights  = BS3B11_bin,
         local   = BS3B9_bin,
         work    = BS3B4_bin,
         envir   = BS3B13_bin,
         vote    = BS3B2_bin,
         history = BS3B6_bin,
         respect = BS3B10_bin,
         news    = BS3B8_bin,
         protest = BS3B5_bin,
         discuss = BS3B12_bin,
         party   = BS3B3_bin,
         female,
         books,
         edexp,
         ed_mom,
         ed_dad)

tbl1 %>% head()
```

    ## # A tibble: 6 x 21
    ##   ICCS_year COUNTRY IDSTUD TOTWGTS obey   rights local work  envir vote  history
    ##       <dbl> <fct>    <dbl>   <dbl> <fct>  <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      1999 AUS      10302    57.2 impor~ not i~ impo~ impo~ impo~ impo~ import~
    ## 2      1999 AUS      10305    57.2 impor~ not i~ impo~ impo~ impo~ impo~ not im~
    ## 3      1999 AUS      10311    57.2 impor~ (Miss~ (Mis~ impo~ impo~ not ~ not im~
    ## 4      1999 AUS      10313    57.2 impor~ not i~ not ~ not ~ not ~ impo~ not im~
    ## 5      1999 AUS      10317    57.2 impor~ impor~ impo~ not ~ impo~ not ~ import~
    ## 6      1999 AUS      10319    57.2 impor~ impor~ impo~ impo~ impo~ impo~ not im~
    ## # ... with 10 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>
    ## # i Use `colnames()` to see all variable names

## 2009 dataloading and merging

2009 data: <https://doi.org/10.3886/ICPSR36997.v2> Downloaded Jan 17,
2019 Title: International Civic and Citizenship Education Study, 2009
(ICPSR 36997)

Load 2009 country files, in chronological order of file names. Bind all
2009 files. Note, total observations of resulting tbl2 (140,650) concur
with xls documentation of expected total n.

``` r
tbl2 <- files %>%
  magrittr::extract(29:66) %>%      # filter to 2009 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, IS2P21L, IS2P21I, IS2P21H, IS2P21K, 
               IS2P21J, IS2P21A, IS2P21C, IS2P21E, IS2P21D, IS2P21G, 
               IS2P21F, IS2P21B, SGENDER, IS2G11, IS2G03, IS2G07, IS2G09, 
               TOTWGTS)) %>% 
  reduce(rbind) %>% 
  as_tibble() %>% 
  mutate(`ICCS_year` = 2009) %>%     # survey year variable creation
  select(`ICCS_year`, everything()) %>% 
  mutate(
    across(where(is.factor), 
           ~fct_explicit_na(.x, na_level = NA))
    )
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl2 %>% 
  select(IS2P21L, IS2P21I, IS2P21H, IS2P21K, IS2P21J, IS2P21A, IS2P21C, IS2P21E, IS2P21D, IS2P21G, IS2P21F, IS2P21B) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl2 %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   IS2P21L                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       76977
    ## 2 (2) QUITE IMPORTANT      45856
    ## 3 (3) NOT VERY IMPORTANT   10022
    ## 4 (4) NOT IMPORTANT AT ALL  3961
    ## 5 <NA>                      3834
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   IS2P21I                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       53959
    ## 2 (2) QUITE IMPORTANT      59698
    ## 3 (3) NOT VERY IMPORTANT   18844
    ## 4 (4) NOT IMPORTANT AT ALL  3862
    ## 5 <NA>                      4287
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   IS2P21H                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       47674
    ## 2 (2) QUITE IMPORTANT      63169
    ## 3 (3) NOT VERY IMPORTANT   21124
    ## 4 (4) NOT IMPORTANT AT ALL  4368
    ## 5 <NA>                      4315
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   IS2P21K                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       53170
    ## 2 (2) QUITE IMPORTANT      58047
    ## 3 (3) NOT VERY IMPORTANT   20006
    ## 4 (4) NOT IMPORTANT AT ALL  5379
    ## 5 <NA>                      4048
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   IS2P21J                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       61438
    ## 2 (2) QUITE IMPORTANT      54712
    ## 3 (3) NOT VERY IMPORTANT   16255
    ## 4 (4) NOT IMPORTANT AT ALL  4043
    ## 5 <NA>                      4202
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   IS2P21A                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       58412
    ## 2 (2) QUITE IMPORTANT      54399
    ## 3 (3) NOT VERY IMPORTANT   20691
    ## 4 (4) NOT IMPORTANT AT ALL  4019
    ## 5 <NA>                      3129
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   IS2P21C                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       50412
    ## 2 (2) QUITE IMPORTANT      55702
    ## 3 (3) NOT VERY IMPORTANT   24553
    ## 4 (4) NOT IMPORTANT AT ALL  5582
    ## 5 <NA>                      4401
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   IS2P21E                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       40616
    ## 2 (2) QUITE IMPORTANT      65090
    ## 3 (3) NOT VERY IMPORTANT   24294
    ## 4 (4) NOT IMPORTANT AT ALL  6739
    ## 5 <NA>                      3911
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   IS2P21D                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       37359
    ## 2 (2) QUITE IMPORTANT      63832
    ## 3 (3) NOT VERY IMPORTANT   29728
    ## 4 (4) NOT IMPORTANT AT ALL  5920
    ## 5 <NA>                      3811
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   IS2P21G                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       35362
    ## 2 (2) QUITE IMPORTANT      51996
    ## 3 (3) NOT VERY IMPORTANT   37311
    ## 4 (4) NOT IMPORTANT AT ALL 11557
    ## 5 <NA>                      4424
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   IS2P21F                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       15669
    ## 2 (2) QUITE IMPORTANT      43337
    ## 3 (3) NOT VERY IMPORTANT   61418
    ## 4 (4) NOT IMPORTANT AT ALL 15929
    ## 5 <NA>                      4297
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   IS2P21B                      n
    ##   <fct>                    <int>
    ## 1 (1) VERY IMPORTANT       12868
    ## 2 (2) QUITE IMPORTANT      33456
    ## 3 (3) NOT VERY IMPORTANT   71041
    ## 4 (4) NOT IMPORTANT AT ALL 19402
    ## 5 <NA>                      3883

Recode all cit norm indicators.

``` r
tbl2 <- tbl2 %>% 
  mutate(
    across(c(IS2P21L, IS2P21I, IS2P21H, IS2P21K, IS2P21J, IS2P21A,
             IS2P21C, IS2P21E, IS2P21D, IS2P21G, IS2P21F, IS2P21B),
           ~fct_collapse(.x,
                         "not important" = c("(3) NOT VERY IMPORTANT", "(4) NOT IMPORTANT AT ALL"),
                         "important"     = c("(1) VERY IMPORTANT", "(2) QUITE IMPORTANT")),
           .names = "{col}_bin")
  )
```

Confirm successful recodes.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl2 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   IS2P21L                  IS2P21L_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     76977
    ## 2 (2) QUITE IMPORTANT      important     45856
    ## 3 (3) NOT VERY IMPORTANT   not important 10022
    ## 4 (4) NOT IMPORTANT AT ALL not important  3961
    ## 5 <NA>                     <NA>           3834
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   IS2P21I                  IS2P21I_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     53959
    ## 2 (2) QUITE IMPORTANT      important     59698
    ## 3 (3) NOT VERY IMPORTANT   not important 18844
    ## 4 (4) NOT IMPORTANT AT ALL not important  3862
    ## 5 <NA>                     <NA>           4287
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   IS2P21H                  IS2P21H_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     47674
    ## 2 (2) QUITE IMPORTANT      important     63169
    ## 3 (3) NOT VERY IMPORTANT   not important 21124
    ## 4 (4) NOT IMPORTANT AT ALL not important  4368
    ## 5 <NA>                     <NA>           4315
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   IS2P21K                  IS2P21K_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     53170
    ## 2 (2) QUITE IMPORTANT      important     58047
    ## 3 (3) NOT VERY IMPORTANT   not important 20006
    ## 4 (4) NOT IMPORTANT AT ALL not important  5379
    ## 5 <NA>                     <NA>           4048
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   IS2P21J                  IS2P21J_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     61438
    ## 2 (2) QUITE IMPORTANT      important     54712
    ## 3 (3) NOT VERY IMPORTANT   not important 16255
    ## 4 (4) NOT IMPORTANT AT ALL not important  4043
    ## 5 <NA>                     <NA>           4202
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   IS2P21A                  IS2P21A_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     58412
    ## 2 (2) QUITE IMPORTANT      important     54399
    ## 3 (3) NOT VERY IMPORTANT   not important 20691
    ## 4 (4) NOT IMPORTANT AT ALL not important  4019
    ## 5 <NA>                     <NA>           3129
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   IS2P21C                  IS2P21C_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     50412
    ## 2 (2) QUITE IMPORTANT      important     55702
    ## 3 (3) NOT VERY IMPORTANT   not important 24553
    ## 4 (4) NOT IMPORTANT AT ALL not important  5582
    ## 5 <NA>                     <NA>           4401
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   IS2P21E                  IS2P21E_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     40616
    ## 2 (2) QUITE IMPORTANT      important     65090
    ## 3 (3) NOT VERY IMPORTANT   not important 24294
    ## 4 (4) NOT IMPORTANT AT ALL not important  6739
    ## 5 <NA>                     <NA>           3911
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   IS2P21D                  IS2P21D_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     37359
    ## 2 (2) QUITE IMPORTANT      important     63832
    ## 3 (3) NOT VERY IMPORTANT   not important 29728
    ## 4 (4) NOT IMPORTANT AT ALL not important  5920
    ## 5 <NA>                     <NA>           3811
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   IS2P21G                  IS2P21G_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     35362
    ## 2 (2) QUITE IMPORTANT      important     51996
    ## 3 (3) NOT VERY IMPORTANT   not important 37311
    ## 4 (4) NOT IMPORTANT AT ALL not important 11557
    ## 5 <NA>                     <NA>           4424
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   IS2P21F                  IS2P21F_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     15669
    ## 2 (2) QUITE IMPORTANT      important     43337
    ## 3 (3) NOT VERY IMPORTANT   not important 61418
    ## 4 (4) NOT IMPORTANT AT ALL not important 15929
    ## 5 <NA>                     <NA>           4297
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   IS2P21B                  IS2P21B_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) VERY IMPORTANT       important     12868
    ## 2 (2) QUITE IMPORTANT      important     33456
    ## 3 (3) NOT VERY IMPORTANT   not important 71041
    ## 4 (4) NOT IMPORTANT AT ALL not important 19402
    ## 5 <NA>                     <NA>           3883

Recode of individual-level control vars

``` r
tbl2 <- tbl2 %>% 
  mutate(female = ifelse(SGENDER == "(0) BOY", 0, 1),    # gender
         books = case_when(                              # books in respondent's home
           IS2G11 == "(1) 0-10 BOOKS"          ~ 0,
           IS2G11 == "(2) 11-25 BOOKS"         ~ 1,
           IS2G11 == "(3) 26-100 BOOKS"        ~ 1,
           IS2G11 == "(4) 101-200 BOOKS"       ~ 2,
           IS2G11 == "(5) 201-500 BOOKS"       ~ 3,
           IS2G11 == "(6) MORE THAN 500 BOOKS" ~ 3
         ),
         edexp = case_when(                              # expected number of additional educ years
           IS2G03 == "(1) <ISCED 5A OR 6>"      ~ 2,
           IS2G03 == "(2) <ISCED 4 OR 5B>"      ~ 2,
           IS2G03 == "(3) <ISCED 3>"            ~ 1,
           IS2G03 == "(4) <ISCED 2>"            ~ 0,
           IS2G03 == "(5) NOT EXPECT <ISCED 2>" ~ 0
         ),
         ed_mom = case_when(                             # mother education
           IS2G07 == "(1) <ISCED 5A OR 6>"            ~ 2,
           IS2G07 == "(2) <ISCED 4 OR 5B>"            ~ 2,
           IS2G07 == "(3) <ISCED 3>"                  ~ 1,
           IS2G07 == "(4) <ISCED 2>"                  ~ 0,
           IS2G07 == "(5) <ISCED 1>"                  ~ 0,
           IS2G07 == "(6) DID NOT COMPLETE <ISCED 1>" ~ 0
         ),
         ed_dad = case_when(                             # father education
           IS2G09 == "(1) <ISCED 5A OR 6>"            ~ 2,
           IS2G09 == "(2) <ISCED 4 OR 5B>"            ~ 2,
           IS2G09 == "(3) <ISCED 3>"                  ~ 1,
           IS2G09 == "(4) <ISCED 2>"                  ~ 0,
           IS2G09 == "(5) <ISCED 1>"                  ~ 0,
           IS2G09 == "(6) DID NOT COMPLETE <ISCED 1>" ~ 0
         )) 

# check recodes
sociodem_vars <- c("SGENDER", "IS2G11", "IS2G03", "IS2G07", "IS2G09")
recoded_vars  <- c("female", "books", "edexp", "ed_mom", "ed_dad")

map2(recoded_vars, sociodem_vars, ~ tbl2 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 3 x 3
    ##   female SGENDER      n
    ##    <dbl> <fct>    <int>
    ## 1      0 (0) BOY  68985
    ## 2      1 (1) GIRL 70381
    ## 3      1 <NA>      1284
    ## 
    ## [[2]]
    ## # A tibble: 7 x 3
    ##   books IS2G11                      n
    ##   <dbl> <fct>                   <int>
    ## 1     0 (1) 0-10 BOOKS          16694
    ## 2     1 (2) 11-25 BOOKS         27373
    ## 3     1 (3) 26-100 BOOKS        44544
    ## 4     2 (4) 101-200 BOOKS       24736
    ## 5     3 (5) 201-500 BOOKS       16316
    ## 6     3 (6) MORE THAN 500 BOOKS  9471
    ## 7    NA <NA>                     1516
    ## 
    ## [[3]]
    ## # A tibble: 6 x 3
    ##   edexp IS2G03                       n
    ##   <dbl> <fct>                    <int>
    ## 1     0 (4) <ISCED 2>             7846
    ## 2     0 (5) NOT EXPECT <ISCED 2>  1120
    ## 3     1 (3) <ISCED 3>            33166
    ## 4     2 (1) <ISCED 5A OR 6>      72791
    ## 5     2 (2) <ISCED 4 OR 5B>      23125
    ## 6    NA <NA>                      2602
    ## 
    ## [[4]]
    ## # A tibble: 7 x 3
    ##   ed_mom IS2G07                             n
    ##    <dbl> <fct>                          <int>
    ## 1      0 (4) <ISCED 2>                  20080
    ## 2      0 (5) <ISCED 1>                  11516
    ## 3      0 (6) DID NOT COMPLETE <ISCED 1>  6096
    ## 4      1 (3) <ISCED 3>                  48335
    ## 5      2 (1) <ISCED 5A OR 6>            28234
    ## 6      2 (2) <ISCED 4 OR 5B>            20989
    ## 7     NA <NA>                            5400
    ## 
    ## [[5]]
    ## # A tibble: 7 x 3
    ##   ed_dad IS2G09                             n
    ##    <dbl> <fct>                          <int>
    ## 1      0 (4) <ISCED 2>                  20351
    ## 2      0 (5) <ISCED 1>                  10892
    ## 3      0 (6) DID NOT COMPLETE <ISCED 1>  5120
    ## 4      1 (3) <ISCED 3>                  46879
    ## 5      2 (1) <ISCED 5A OR 6>            27928
    ## 6      2 (2) <ISCED 4 OR 5B>            21003
    ## 7     NA <NA>                            8477

Select for LCA vars tibble, including rename all mutated variables and
display first five lines of dataframe.

``` r
tbl2 <- tbl2 %>%
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         TOTWGTS,
         obey    = IS2P21L_bin,
         rights  = IS2P21I_bin,
         local   = IS2P21H_bin,
         work    = IS2P21K_bin,
         envir   = IS2P21J_bin,
         vote    = IS2P21A_bin,
         history = IS2P21C_bin,
         respect = IS2P21E_bin,
         news    = IS2P21D_bin,
         protest = IS2P21G_bin,
         discuss = IS2P21F_bin,
         party   = IS2P21B_bin,
         female,
         books,
         edexp,
         ed_mom,
         ed_dad) 

tbl2 %>% head()
```

    ## # A tibble: 6 x 21
    ##   ICCS_year COUNTRY  IDSTUD TOTWGTS obey  rights local work  envir vote  history
    ##       <dbl> <fct>     <dbl>   <dbl> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      2009 AUT      1.00e7    26.6 not ~ impor~ impo~ not ~ impo~ impo~ not im~
    ## 2      2009 AUT      1.00e7    26.6 impo~ impor~ impo~ not ~ impo~ not ~ import~
    ## 3      2009 AUT      1.00e7    26.6 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 4      2009 AUT      1.00e7    26.6 impo~ impor~ impo~ not ~ impo~ impo~ import~
    ## 5      2009 AUT      1.00e7    26.6 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 6      2009 AUT      1.00e7    26.6 impo~ impor~ not ~ impo~ impo~ impo~ import~
    ## # ... with 10 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>
    ## # i Use `colnames()` to see all variable names

## 2016 data loading and merging

2016 data: <https://doi.org/10.3886/ICPSR37147.v1>  
Downloaded Jan 21, 2019 Title: International Civic and Citizenship
Education Study, 2016 (ICPSR 37147)

2016 country files, in chronological order of file names. Bind all 2016
country files. Note, total observations of resulting tbl (94,603) concur
with xls documentation of expected total n.

``` r
tbl3 <- files %>%
  magrittr::extract(67:90) %>%      # filter to 2016 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J,
               IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B, S_GENDER, IS3G11, IS3G03, IS3G07, IS3G09, TOTWGTS)) %>% 
  reduce(rbind) %>% 
  as_tibble()%>% 
  mutate(`ICCS_year` = 2016) %>%    # create survey year variable
  select(`ICCS_year`, everything()) %>% 
  mutate(
    across(where(is.factor), 
           ~fct_explicit_na(.x, na_level = NA))
    )
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl3 %>% 
  select(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl3 %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   IS3G23L                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       53951
    ## 2 (2) Quite important      30460
    ## 3 (3) Not very important    5857
    ## 4 (4) Not important at all  1711
    ## 5 <NA>                      2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   IS3G23I                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37255
    ## 2 (2) Quite important      40011
    ## 3 (3) Not very important   12492
    ## 4 (4) Not important at all  2228
    ## 5 <NA>                      2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   IS3G23H                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       32569
    ## 2 (2) Quite important      42817
    ## 3 (3) Not very important   14079
    ## 4 (4) Not important at all  2471
    ## 5 <NA>                      2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   IS3G23K                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38082
    ## 2 (2) Quite important      40137
    ## 3 (3) Not very important   11292
    ## 4 (4) Not important at all  2536
    ## 5 <NA>                      2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   IS3G23J                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       42126
    ## 2 (2) Quite important      37204
    ## 3 (3) Not very important   10437
    ## 4 (4) Not important at all  2188
    ## 5 <NA>                      2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   IS3G23A                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38931
    ## 2 (2) Quite important      37262
    ## 3 (3) Not very important   13975
    ## 4 (4) Not important at all  2490
    ## 5 <NA>                      1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   IS3G23C                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37043
    ## 2 (2) Quite important      36919
    ## 3 (3) Not very important   14836
    ## 4 (4) Not important at all  3023
    ## 5 <NA>                      2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   IS3G23E                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       33375
    ## 2 (2) Quite important      43140
    ## 3 (3) Not very important   12581
    ## 4 (4) Not important at all  3017
    ## 5 <NA>                      2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   IS3G23D                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       26980
    ## 2 (2) Quite important      43878
    ## 3 (3) Not very important   18199
    ## 4 (4) Not important at all  3297
    ## 5 <NA>                      2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   IS3G23G                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       22817
    ## 2 (2) Quite important      35020
    ## 3 (3) Not very important   26965
    ## 4 (4) Not important at all  7129
    ## 5 <NA>                      2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   IS3G23F                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       11348
    ## 2 (2) Quite important      30410
    ## 3 (3) Not very important   41390
    ## 4 (4) Not important at all  8739
    ## 5 <NA>                      2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   IS3G23B                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important        9003
    ## 2 (2) Quite important      22157
    ## 3 (3) Not very important   48896
    ## 4 (4) Not important at all 12187
    ## 5 <NA>                      2360

Recode all cit norm indicators.

``` r
tbl3 <- tbl3 %>% 
  mutate(
    across(c(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, 
             IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B),
           ~fct_collapse(.x,
                                    "not important" = c("(3) Not very important", "(4) Not important at all"),
                                    "important"     = c("(1) Very important", "(2) Quite important")),
           .names = "{col}_bin")
  )
```

Confirm successful mutates.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl3 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   IS3G23L                  IS3G23L_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     53951
    ## 2 (2) Quite important      important     30460
    ## 3 (3) Not very important   not important  5857
    ## 4 (4) Not important at all not important  1711
    ## 5 <NA>                     <NA>           2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   IS3G23I                  IS3G23I_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37255
    ## 2 (2) Quite important      important     40011
    ## 3 (3) Not very important   not important 12492
    ## 4 (4) Not important at all not important  2228
    ## 5 <NA>                     <NA>           2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   IS3G23H                  IS3G23H_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     32569
    ## 2 (2) Quite important      important     42817
    ## 3 (3) Not very important   not important 14079
    ## 4 (4) Not important at all not important  2471
    ## 5 <NA>                     <NA>           2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   IS3G23K                  IS3G23K_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38082
    ## 2 (2) Quite important      important     40137
    ## 3 (3) Not very important   not important 11292
    ## 4 (4) Not important at all not important  2536
    ## 5 <NA>                     <NA>           2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   IS3G23J                  IS3G23J_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     42126
    ## 2 (2) Quite important      important     37204
    ## 3 (3) Not very important   not important 10437
    ## 4 (4) Not important at all not important  2188
    ## 5 <NA>                     <NA>           2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   IS3G23A                  IS3G23A_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38931
    ## 2 (2) Quite important      important     37262
    ## 3 (3) Not very important   not important 13975
    ## 4 (4) Not important at all not important  2490
    ## 5 <NA>                     <NA>           1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   IS3G23C                  IS3G23C_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37043
    ## 2 (2) Quite important      important     36919
    ## 3 (3) Not very important   not important 14836
    ## 4 (4) Not important at all not important  3023
    ## 5 <NA>                     <NA>           2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   IS3G23E                  IS3G23E_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     33375
    ## 2 (2) Quite important      important     43140
    ## 3 (3) Not very important   not important 12581
    ## 4 (4) Not important at all not important  3017
    ## 5 <NA>                     <NA>           2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   IS3G23D                  IS3G23D_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     26980
    ## 2 (2) Quite important      important     43878
    ## 3 (3) Not very important   not important 18199
    ## 4 (4) Not important at all not important  3297
    ## 5 <NA>                     <NA>           2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   IS3G23G                  IS3G23G_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     22817
    ## 2 (2) Quite important      important     35020
    ## 3 (3) Not very important   not important 26965
    ## 4 (4) Not important at all not important  7129
    ## 5 <NA>                     <NA>           2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   IS3G23F                  IS3G23F_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     11348
    ## 2 (2) Quite important      important     30410
    ## 3 (3) Not very important   not important 41390
    ## 4 (4) Not important at all not important  8739
    ## 5 <NA>                     <NA>           2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   IS3G23B                  IS3G23B_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important      9003
    ## 2 (2) Quite important      important     22157
    ## 3 (3) Not very important   not important 48896
    ## 4 (4) Not important at all not important 12187
    ## 5 <NA>                     <NA>           2360

Recode of individual-level control vars

``` r
tbl3 <- tbl3 %>% 
  mutate(female = ifelse(S_GENDER == "(0) Boy", 0, 1),   # gender
         books = case_when(                              # books in respondent's home
           IS3G11 == "(1) None or very few (0–10 books)"                                ~ 0,
           IS3G11 == "(2) Enough to fill one shelf (11–25 books)"                       ~ 1,
           IS3G11 == "(3) Enough to fill one bookcase (26–100 books)"                   ~ 1,
           IS3G11 == "(4) Enough to fill two bookcase (101–200 books)"                  ~ 2,
           IS3G11 == "(5) Enough to fill three or more bookcases (more than 200 books)" ~ 3
         ),
         edexp = case_when(                              # expected number of additional educ years
           IS3G03 == "(1) <ISCED level 6, 7 or 8>"   ~ 2,
           IS3G03 == "(2) <ISCED level 4 or 5>"      ~ 2,
           IS3G03 == "(3) <ISCED level 3>"           ~ 1,
           IS3G03 == "(4) <ISCED level 2> or below" ~ 0
         ),
         ed_mom = case_when(                             # mother education
           IS3G07 == "(1) <ISCED level 6, 7 or 8>"              ~ 2,
           IS3G07 == "(2) <ISCED level 4 or 5>"                 ~ 2,
           IS3G07 == "(3) <ISCED level 3>"                      ~ 1,
           IS3G07 == "(4) <ISCED level 2>"                      ~ 0,
           IS3G07 == "(5) She did not complete <ISCED level 2>" ~ 0
         ),
         ed_dad = case_when(                             # father education
           IS3G09 == "(1) <ISCED level 6, 7 or 8>"             ~ 2,
           IS3G09 == "(2) <ISCED level 4 or 5>"                ~ 2,
           IS3G09 == "(3) <ISCED level 3>"                     ~ 1,
           IS3G09 == "(4) <ISCED level 2>"                     ~ 0,
           IS3G09 == "(5) He did not complete <ISCED level 2>" ~ 0
         )) 

# check recodes
sociodem_vars <- c("S_GENDER", "IS3G11", "IS3G03", "IS3G07", "IS3G09")
recoded_vars  <- c("female", "books", "edexp", "ed_mom", "ed_dad")

map2(recoded_vars, sociodem_vars, ~ tbl3 %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 3 x 3
    ##   female S_GENDER     n
    ##    <dbl> <fct>    <int>
    ## 1      0 (0) Boy  47674
    ## 2      1 (1) Girl 46903
    ## 3      1 <NA>        26
    ## 
    ## [[2]]
    ## # A tibble: 6 x 3
    ##   books IS3G11                                                               n
    ##   <dbl> <fct>                                                            <int>
    ## 1     0 (1) None or very few (0–10 books)                                14514
    ## 2     1 (2) Enough to fill one shelf (11–25 books)                       22778
    ## 3     1 (3) Enough to fill one bookcase (26–100 books)                   29001
    ## 4     2 (4) Enough to fill two bookcase (101–200 books)                  14512
    ## 5     3 (5) Enough to fill three or more bookcases (more than 200 books) 12648
    ## 6    NA <NA>                                                              1150
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   edexp IS3G03                           n
    ##   <dbl> <fct>                        <int>
    ## 1     0 (4) <ISCED level 2> or below  4116
    ## 2     1 (3) <ISCED level 3>          17506
    ## 3     2 (1) <ISCED level 6, 7 or 8>  52942
    ## 4     2 (2) <ISCED level 4 or 5>     18254
    ## 5    NA <NA>                          1785
    ## 
    ## [[4]]
    ## # A tibble: 6 x 3
    ##   ed_mom IS3G07                                       n
    ##    <dbl> <fct>                                    <int>
    ## 1      0 (4) <ISCED level 2>                      10256
    ## 2      0 (5) She did not complete <ISCED level 2>  6781
    ## 3      1 (3) <ISCED level 3>                      28161
    ## 4      2 (1) <ISCED level 6, 7 or 8>              26871
    ## 5      2 (2) <ISCED level 4 or 5>                 18886
    ## 6     NA <NA>                                      3648
    ## 
    ## [[5]]
    ## # A tibble: 6 x 3
    ##   ed_dad IS3G09                                      n
    ##    <dbl> <fct>                                   <int>
    ## 1      0 (4) <ISCED level 2>                     10685
    ## 2      0 (5) He did not complete <ISCED level 2>  6299
    ## 3      1 (3) <ISCED level 3>                     28911
    ## 4      2 (1) <ISCED level 6, 7 or 8>             23652
    ## 5      2 (2) <ISCED level 4 or 5>                18945
    ## 6     NA <NA>                                     6111

Select for LCA vars tibble, including rename all mutated variables and
display first five lines of dataframe.

``` r
tbl3 <- tbl3 %>% 
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         TOTWGTS,
         obey    = IS3G23L_bin,
         rights  = IS3G23I_bin,
         local   = IS3G23H_bin,
         work    = IS3G23K_bin,
         envir   = IS3G23J_bin,
         vote    = IS3G23A_bin,
         history = IS3G23C_bin,
         respect = IS3G23E_bin,
         news    = IS3G23D_bin,
         protest = IS3G23G_bin,
         discuss = IS3G23F_bin,
         party   = IS3G23B_bin,
         female,
         books,
         edexp,
         ed_mom,
         ed_dad)

tbl3 %>% head()
```

    ## # A tibble: 6 x 21
    ##   ICCS_year COUNTRY  IDSTUD TOTWGTS obey  rights local work  envir vote  history
    ##       <dbl> <fct>     <dbl>   <dbl> <fct> <fct>  <fct> <fct> <fct> <fct> <fct>  
    ## 1      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 2      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 3      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 4      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ impo~ import~
    ## 5      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ impo~ not im~
    ## 6      2016 BFL      1.00e7    22.5 impo~ impor~ impo~ impo~ impo~ not ~ not im~
    ## # ... with 10 more variables: respect <fct>, news <fct>, protest <fct>,
    ## #   discuss <fct>, party <fct>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>
    ## # i Use `colnames()` to see all variable names

## Combining recoded 1999, 2009 and 2016 data frames

Combine the three data frames and create unique id per observation
across binded country files to be able to import Latent Gold class
assignment for each unique observation.

``` r
tbl <- rbind(tbl1, tbl2, tbl3) %>% 
  mutate(id  = row_number(),
         id2 = paste0(COUNTRY, IDSTUD))
```

Check number of observations by survey year of the combined data frame.

``` r
# number of observations by survey year
tbl %>% 
  count(ICCS_year) %>% 
  knitr::kable()
```

| ICCS_year |      n |
|----------:|-------:|
|      1999 |  93882 |
|      2009 | 140650 |
|      2016 |  94603 |

## Exporting final combined datafile

Before exporting, convert citizenship norm indicators to integer (0 =
“not important”, 1 = “important”).

``` r
tbl <- tbl %>% 
  mutate(
    across(c(obey, rights, local, work, envir, vote, history, 
             respect, news, protest, discuss, party),
           ~ case_when(
             .x == "not important" ~ 0L,
             .x == "important"     ~ 1L,
             TRUE~NA_integer_
           ))
  ) %>% 
  mutate(
    ICCS_year = as.integer(ICCS_year),
    COUNTRY   = as.character(COUNTRY))

tbl %>% head()
```

    ## # A tibble: 6 x 23
    ##   ICCS_year COUNTRY IDSTUD TOTWGTS  obey rights local  work envir  vote history
    ##       <int> <chr>    <dbl>   <dbl> <int>  <int> <int> <int> <int> <int>   <int>
    ## 1      1999 AUS      10302    57.2     1      0     1     1     1     1       1
    ## 2      1999 AUS      10305    57.2     1      0     1     1     1     1       0
    ## 3      1999 AUS      10311    57.2     1     NA    NA     1     1     0       0
    ## 4      1999 AUS      10313    57.2     1      0     0     0     0     1       0
    ## 5      1999 AUS      10317    57.2     1      1     1     0     1     0       1
    ## 6      1999 AUS      10319    57.2     1      1     1     1     1     1       0
    ## # ... with 12 more variables: respect <int>, news <int>, protest <int>,
    ## #   discuss <int>, party <int>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>, id <int>, id2 <chr>
    ## # i Use `colnames()` to see all variable names

Save full data.

``` r
write_delim(tbl, "output/tbl_all_countries_all_waves.dat", delim = ",")
```

Export single dat file for 14 countries that participate in all 3 time
points:

``` r
# Select for countries in all 3 waves only

all_wave_countries <- tbl %>%
  count(COUNTRY, ICCS_year) %>% 
  count(COUNTRY) %>%
  filter(n == 3) %>% 
  pull(COUNTRY)

all_wave_countries %>% head()
```

    ## [1] "BGR" "CHL" "COL" "DNK" "EST" "FIN"

``` r
#Filter by countries in all 3 waves

tbl14countries_3waves <- tbl %>% 
  filter(COUNTRY %in% all_wave_countries)
  
tbl14countries_3waves %>% head()
```

    ## # A tibble: 6 x 23
    ##   ICCS_year COUNTRY IDSTUD TOTWGTS  obey rights local  work envir  vote history
    ##       <int> <chr>    <dbl>   <dbl> <int>  <int> <int> <int> <int> <int>   <int>
    ## 1      1999 BGR      70612    19.7     1      1    NA    NA    NA    NA       1
    ## 2      1999 BGR     100301    18.0     1      1     0     1     1     0       0
    ## 3      1999 BGR     100303    18.0     0      0     0     1     0     0       1
    ## 4      1999 BGR     100311    18.0     1      1     0     1     1     1       1
    ## 5      1999 BGR     160619    11.8     0      0     0     0     0     0       0
    ## 6      1999 BGR     300320    41.4     0      1     1     1     1     1       1
    ## # ... with 12 more variables: respect <int>, news <int>, protest <int>,
    ## #   discuss <int>, party <int>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>, id <int>, id2 <chr>
    ## # i Use `colnames()` to see all variable names

``` r
write_delim(tbl14countries_3waves, "output/tbl14countries_3waves.dat", delim = ",")
```

## Preparing 2016 all-country data with additional variables for 2-step analysis

2016 example recode of new specific variables using 1 country rda file
(Belgium)

View count of immigration var for recode.

``` r
tbl_belg2016 <- load_files("../data/37147-0064-Data.rda") %>% 
  as_tibble() %>% 
  mutate(
    across(where(is.factor), 
           ~fct_explicit_na(.x, na_level = NA))
    )
```

\*\*NONNATIVEBORN

View count frequencies of IS3G04A variable (country of birth) for recode

``` r
tbl_belg2016 %>%
  count(IS3G04A)
```

    ## # A tibble: 3 x 2
    ##   IS3G04A                                     n
    ##   <fct>                                   <int>
    ## 1 (0) Other                                 229
    ## 2 (1) Country of birth is country of test  2652
    ## 3 <NA>                                       50

Do recode of IS3G04A var –\> “nonnat_born”

``` r
tbl_belg2016 <- tbl_belg2016 %>%
  mutate(nonnat_born = case_when(
    IS3G04A == "(0) Other"                                ~ 1,
    IS3G04A == "(1) Country of birth is country of test"  ~ 0
  ))
```

Confirm “nonnat_born” recode is correct

``` r
tbl_belg2016  %>% 
  count(IS3G04A, nonnat_born)
```

    ## # A tibble: 3 x 3
    ##   IS3G04A                                 nonnat_born     n
    ##   <fct>                                         <dbl> <int>
    ## 1 (0) Other                                         1   229
    ## 2 (1) Country of birth is country of test           0  2652
    ## 3 <NA>                                             NA    50

``` r
tbl_belg2016  %>% 
  count (nonnat_born)
```

    ## # A tibble: 3 x 2
    ##   nonnat_born     n
    ##         <dbl> <int>
    ## 1           0  2652
    ## 2           1   229
    ## 3          NA    50

\*\*IMMIGRANTFAM

View count frequencies of S_IMMIG variable (country of birth) for recode

``` r
tbl_belg2016 %>% count(S_IMMIG)
```

    ## # A tibble: 4 x 2
    ##   S_IMMIG                                                      n
    ##   <fct>                                                    <int>
    ## 1 "(1) \tAt least one parent born in country"               2437
    ## 2 "(2) Students born in country but parent(s) born abroad"   217
    ## 3 "(3) Students and parent(s) born abroad"                   189
    ## 4  <NA>                                                       88

Do recode of S_IMMIG var –\> “immigrantfam” Note: distinction between
“nonnativeborn” and “immigrantfam”: “nonnativeborn” = 14yo respondent
not born in country of test Belgium 2016 = 7.81% of population
“immigrantfam” = 14yo and parent(s) born abroad Belgium 2016 = 6.45% of
population thus “immigrantfam” is likely to be a smaller and more
“foreign” subset of sample

``` r
tbl_belg2016 <- tbl_belg2016 %>%
  mutate(immigrantfam = case_when(
    S_IMMIG == "(1) \tAt least one parent born in country"               ~ 0,
    S_IMMIG == "(2) Students born in country but parent(s) born abroad"  ~ 0,
    S_IMMIG == "(3) Students and parent(s) born abroad"                  ~ 1,
  ))
```

Confirm “immigrantfam” recode is correct

``` r
tbl_belg2016  %>% 
  count(S_IMMIG, immigrantfam)
```

    ## # A tibble: 4 x 3
    ##   S_IMMIG                                                  immigrantfam     n
    ##   <fct>                                                           <dbl> <int>
    ## 1 "(1) \tAt least one parent born in country"                         0  2437
    ## 2 "(2) Students born in country but parent(s) born abroad"            0   217
    ## 3 "(3) Students and parent(s) born abroad"                            1   189
    ## 4  <NA>                                                              NA    88

``` r
tbl_belg2016  %>% 
  count(immigrantfam)
```

    ## # A tibble: 3 x 2
    ##   immigrantfam     n
    ##          <dbl> <int>
    ## 1            0  2654
    ## 2            1   189
    ## 3           NA    88

\*\*NON-NATIVE LANGUAGE View count frequencies of IS3G05

``` r
tbl_belg2016 %>% count(IS3G05)
```

    ## # A tibble: 3 x 2
    ##   IS3G05                                       n
    ##   <fct>                                    <int>
    ## 1 (0) Other                                  414
    ## 2 (1) Language at home is language of test  2392
    ## 3 <NA>                                       125

Do recode of IS3G05 var –\> “nonnat_lang”

``` r
tbl_belg2016 <- tbl_belg2016 %>%
  mutate(nonnat_lang = case_when(
    IS3G05 == "(0) Other"                                 ~ 1,
    IS3G05 == "(1) Language at home is language of test"  ~ 0,
    ))
```

Confirm “nonnativeborn” recode is correct

``` r
tbl_belg2016  %>% 
  count(IS3G05, nonnat_lang)
```

    ## # A tibble: 3 x 3
    ##   IS3G05                                   nonnat_lang     n
    ##   <fct>                                          <dbl> <int>
    ## 1 (0) Other                                          1   414
    ## 2 (1) Language at home is language of test           0  2392
    ## 3 <NA>                                              NA   125

Load all 2016 countries to select for previously coded variables along
with these new ones

``` r
tbl_allcountries_2016_2step <- files %>%
  magrittr::extract(67:90) %>%      # filter to 2016 files only
  map(~ .x %>%
        load_files() %>%
        select(COUNTRY, IDCNTRY, IDSTUD, IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J,
               IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B, S_GENDER, IS3G11, IS3G03, IS3G07, IS3G09, TOTWGTS, IS3G04A, S_IMMIG, IS3G05)) %>% 
  reduce(rbind) %>% 
  as_tibble()%>% 
  mutate(`ICCS_year` = 2016) %>%    # create survey year variable
  select(`ICCS_year`, everything()) %>% 
  mutate(
    across(where(is.factor), 
           ~fct_explicit_na(.x, na_level = NA))
    )
```

Cit norm, count all indicators to begin recode.

``` r
original_vars <- tbl_allcountries_2016_2step %>% 
  select(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B) %>% 
  colnames() 

original_vars %>% 
  map(~ tbl_allcountries_2016_2step %>% count(!!sym(.x)))
```

    ## [[1]]
    ## # A tibble: 5 x 2
    ##   IS3G23L                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       53951
    ## 2 (2) Quite important      30460
    ## 3 (3) Not very important    5857
    ## 4 (4) Not important at all  1711
    ## 5 <NA>                      2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 2
    ##   IS3G23I                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37255
    ## 2 (2) Quite important      40011
    ## 3 (3) Not very important   12492
    ## 4 (4) Not important at all  2228
    ## 5 <NA>                      2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 2
    ##   IS3G23H                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       32569
    ## 2 (2) Quite important      42817
    ## 3 (3) Not very important   14079
    ## 4 (4) Not important at all  2471
    ## 5 <NA>                      2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 2
    ##   IS3G23K                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38082
    ## 2 (2) Quite important      40137
    ## 3 (3) Not very important   11292
    ## 4 (4) Not important at all  2536
    ## 5 <NA>                      2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 2
    ##   IS3G23J                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       42126
    ## 2 (2) Quite important      37204
    ## 3 (3) Not very important   10437
    ## 4 (4) Not important at all  2188
    ## 5 <NA>                      2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 2
    ##   IS3G23A                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       38931
    ## 2 (2) Quite important      37262
    ## 3 (3) Not very important   13975
    ## 4 (4) Not important at all  2490
    ## 5 <NA>                      1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 2
    ##   IS3G23C                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       37043
    ## 2 (2) Quite important      36919
    ## 3 (3) Not very important   14836
    ## 4 (4) Not important at all  3023
    ## 5 <NA>                      2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 2
    ##   IS3G23E                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       33375
    ## 2 (2) Quite important      43140
    ## 3 (3) Not very important   12581
    ## 4 (4) Not important at all  3017
    ## 5 <NA>                      2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 2
    ##   IS3G23D                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       26980
    ## 2 (2) Quite important      43878
    ## 3 (3) Not very important   18199
    ## 4 (4) Not important at all  3297
    ## 5 <NA>                      2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 2
    ##   IS3G23G                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       22817
    ## 2 (2) Quite important      35020
    ## 3 (3) Not very important   26965
    ## 4 (4) Not important at all  7129
    ## 5 <NA>                      2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 2
    ##   IS3G23F                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important       11348
    ## 2 (2) Quite important      30410
    ## 3 (3) Not very important   41390
    ## 4 (4) Not important at all  8739
    ## 5 <NA>                      2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 2
    ##   IS3G23B                      n
    ##   <fct>                    <int>
    ## 1 (1) Very important        9003
    ## 2 (2) Quite important      22157
    ## 3 (3) Not very important   48896
    ## 4 (4) Not important at all 12187
    ## 5 <NA>                      2360

Recode all cit norm indicators.

``` r
tbl_allcountries_2016_2step <- tbl_allcountries_2016_2step %>% 
  mutate(
    across(c(IS3G23L, IS3G23I, IS3G23H, IS3G23K, IS3G23J, IS3G23A, 
             IS3G23C, IS3G23E, IS3G23D, IS3G23G, IS3G23F, IS3G23B),
           ~fct_collapse(.x,
                         "not important" = c("(3) Not very important", "(4) Not important at all"),
                         "important"     = c("(1) Very important", "(2) Quite important")),
           .names = "{.col}_bin")
  )
```

Confirm successful cit norm mutates.

``` r
bin_vars <- original_vars %>% 
  paste0("_bin")

map2(original_vars, bin_vars, ~ tbl_allcountries_2016_2step %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 5 x 3
    ##   IS3G23L                  IS3G23L_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     53951
    ## 2 (2) Quite important      important     30460
    ## 3 (3) Not very important   not important  5857
    ## 4 (4) Not important at all not important  1711
    ## 5 <NA>                     <NA>           2624
    ## 
    ## [[2]]
    ## # A tibble: 5 x 3
    ##   IS3G23I                  IS3G23I_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37255
    ## 2 (2) Quite important      important     40011
    ## 3 (3) Not very important   not important 12492
    ## 4 (4) Not important at all not important  2228
    ## 5 <NA>                     <NA>           2617
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   IS3G23H                  IS3G23H_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     32569
    ## 2 (2) Quite important      important     42817
    ## 3 (3) Not very important   not important 14079
    ## 4 (4) Not important at all not important  2471
    ## 5 <NA>                     <NA>           2667
    ## 
    ## [[4]]
    ## # A tibble: 5 x 3
    ##   IS3G23K                  IS3G23K_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38082
    ## 2 (2) Quite important      important     40137
    ## 3 (3) Not very important   not important 11292
    ## 4 (4) Not important at all not important  2536
    ## 5 <NA>                     <NA>           2556
    ## 
    ## [[5]]
    ## # A tibble: 5 x 3
    ##   IS3G23J                  IS3G23J_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     42126
    ## 2 (2) Quite important      important     37204
    ## 3 (3) Not very important   not important 10437
    ## 4 (4) Not important at all not important  2188
    ## 5 <NA>                     <NA>           2648
    ## 
    ## [[6]]
    ## # A tibble: 5 x 3
    ##   IS3G23A                  IS3G23A_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     38931
    ## 2 (2) Quite important      important     37262
    ## 3 (3) Not very important   not important 13975
    ## 4 (4) Not important at all not important  2490
    ## 5 <NA>                     <NA>           1945
    ## 
    ## [[7]]
    ## # A tibble: 5 x 3
    ##   IS3G23C                  IS3G23C_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     37043
    ## 2 (2) Quite important      important     36919
    ## 3 (3) Not very important   not important 14836
    ## 4 (4) Not important at all not important  3023
    ## 5 <NA>                     <NA>           2782
    ## 
    ## [[8]]
    ## # A tibble: 5 x 3
    ##   IS3G23E                  IS3G23E_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     33375
    ## 2 (2) Quite important      important     43140
    ## 3 (3) Not very important   not important 12581
    ## 4 (4) Not important at all not important  3017
    ## 5 <NA>                     <NA>           2490
    ## 
    ## [[9]]
    ## # A tibble: 5 x 3
    ##   IS3G23D                  IS3G23D_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     26980
    ## 2 (2) Quite important      important     43878
    ## 3 (3) Not very important   not important 18199
    ## 4 (4) Not important at all not important  3297
    ## 5 <NA>                     <NA>           2249
    ## 
    ## [[10]]
    ## # A tibble: 5 x 3
    ##   IS3G23G                  IS3G23G_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     22817
    ## 2 (2) Quite important      important     35020
    ## 3 (3) Not very important   not important 26965
    ## 4 (4) Not important at all not important  7129
    ## 5 <NA>                     <NA>           2672
    ## 
    ## [[11]]
    ## # A tibble: 5 x 3
    ##   IS3G23F                  IS3G23F_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important     11348
    ## 2 (2) Quite important      important     30410
    ## 3 (3) Not very important   not important 41390
    ## 4 (4) Not important at all not important  8739
    ## 5 <NA>                     <NA>           2716
    ## 
    ## [[12]]
    ## # A tibble: 5 x 3
    ##   IS3G23B                  IS3G23B_bin       n
    ##   <fct>                    <fct>         <int>
    ## 1 (1) Very important       important      9003
    ## 2 (2) Quite important      important     22157
    ## 3 (3) Not very important   not important 48896
    ## 4 (4) Not important at all not important 12187
    ## 5 <NA>                     <NA>           2360

Recode of individual-level control vars

``` r
tbl_allcountries_2016_2step <- tbl_allcountries_2016_2step %>% 
  mutate(female = ifelse(S_GENDER == "(0) Boy", 0, 1),   # gender
         books = case_when(                              # books in respondent's home
           IS3G11 == "(1) None or very few (0–10 books)"                                ~ 0,
           IS3G11 == "(2) Enough to fill one shelf (11–25 books)"                       ~ 1,
           IS3G11 == "(3) Enough to fill one bookcase (26–100 books)"                   ~ 1,
           IS3G11 == "(4) Enough to fill two bookcase (101–200 books)"                  ~ 2,
           IS3G11 == "(5) Enough to fill three or more bookcases (more than 200 books)" ~ 3
         ),
         edexp = case_when(                              # expected number of additional educ years
           IS3G03 == "(1) <ISCED level 6, 7 or 8>"   ~ 2,
           IS3G03 == "(2) <ISCED level 4 or 5>"      ~ 2,
           IS3G03 == "(3) <ISCED level 3>"           ~ 1,
           IS3G03 == "(4) <ISCED level 2> or below" ~ 0
         ),
         ed_mom = case_when(                             # mother education
           IS3G07 == "(1) <ISCED level 6, 7 or 8>"              ~ 2,
           IS3G07 == "(2) <ISCED level 4 or 5>"                 ~ 2,
           IS3G07 == "(3) <ISCED level 3>"                      ~ 1,
           IS3G07 == "(4) <ISCED level 2>"                      ~ 0,
           IS3G07 == "(5) She did not complete <ISCED level 2>" ~ 0
         ),
         ed_dad = case_when(                             # father education
           IS3G09 == "(1) <ISCED level 6, 7 or 8>"             ~ 2,
           IS3G09 == "(2) <ISCED level 4 or 5>"                ~ 2,
           IS3G09 == "(3) <ISCED level 3>"                     ~ 1,
           IS3G09 == "(4) <ISCED level 2>"                     ~ 0,
           IS3G09 == "(5) He did not complete <ISCED level 2>" ~ 0
         ),
         nonnat_born = case_when(                     # non-native born respondent
           IS3G04A == "(0) Other"                               ~ 1,
           IS3G04A == "(1) Country of birth is country of test" ~ 0
         ),
         immigrantfam = case_when(                   # non-native born respondent & parent(s)
          S_IMMIG == "(1) \tAt least one parent born in country"              ~ 0,
          S_IMMIG == "(2) Students born in country but parent(s) born abroad" ~ 0,
          S_IMMIG == "(3) Students and parent(s) born abroad"                 ~ 1,
         ),
         nonnat_lang = case_when(                    # non-native language at home
          IS3G05 == "(0) Other"                                 ~ 1,
          IS3G05 == "(1) Language at home is language of test"  ~ 0,
         )) 

# check recodes
sociodem_vars_2step <- c("S_GENDER", "IS3G11", "IS3G03", "IS3G07", "IS3G09", "IS3G04A", "S_IMMIG", "IS3G05")
recoded_vars_2step  <- c("female", "books", "edexp", "ed_mom", "ed_dad", "nonnat_born", "immigrantfam", "nonnat_lang")

map2(recoded_vars_2step, sociodem_vars_2step, ~ tbl_allcountries_2016_2step %>% count(!!sym(.x), !!sym(.y)))
```

    ## [[1]]
    ## # A tibble: 3 x 3
    ##   female S_GENDER     n
    ##    <dbl> <fct>    <int>
    ## 1      0 (0) Boy  47674
    ## 2      1 (1) Girl 46903
    ## 3      1 <NA>        26
    ## 
    ## [[2]]
    ## # A tibble: 6 x 3
    ##   books IS3G11                                                               n
    ##   <dbl> <fct>                                                            <int>
    ## 1     0 (1) None or very few (0–10 books)                                14514
    ## 2     1 (2) Enough to fill one shelf (11–25 books)                       22778
    ## 3     1 (3) Enough to fill one bookcase (26–100 books)                   29001
    ## 4     2 (4) Enough to fill two bookcase (101–200 books)                  14512
    ## 5     3 (5) Enough to fill three or more bookcases (more than 200 books) 12648
    ## 6    NA <NA>                                                              1150
    ## 
    ## [[3]]
    ## # A tibble: 5 x 3
    ##   edexp IS3G03                           n
    ##   <dbl> <fct>                        <int>
    ## 1     0 (4) <ISCED level 2> or below  4116
    ## 2     1 (3) <ISCED level 3>          17506
    ## 3     2 (1) <ISCED level 6, 7 or 8>  52942
    ## 4     2 (2) <ISCED level 4 or 5>     18254
    ## 5    NA <NA>                          1785
    ## 
    ## [[4]]
    ## # A tibble: 6 x 3
    ##   ed_mom IS3G07                                       n
    ##    <dbl> <fct>                                    <int>
    ## 1      0 (4) <ISCED level 2>                      10256
    ## 2      0 (5) She did not complete <ISCED level 2>  6781
    ## 3      1 (3) <ISCED level 3>                      28161
    ## 4      2 (1) <ISCED level 6, 7 or 8>              26871
    ## 5      2 (2) <ISCED level 4 or 5>                 18886
    ## 6     NA <NA>                                      3648
    ## 
    ## [[5]]
    ## # A tibble: 6 x 3
    ##   ed_dad IS3G09                                      n
    ##    <dbl> <fct>                                   <int>
    ## 1      0 (4) <ISCED level 2>                     10685
    ## 2      0 (5) He did not complete <ISCED level 2>  6299
    ## 3      1 (3) <ISCED level 3>                     28911
    ## 4      2 (1) <ISCED level 6, 7 or 8>             23652
    ## 5      2 (2) <ISCED level 4 or 5>                18945
    ## 6     NA <NA>                                     6111
    ## 
    ## [[6]]
    ## # A tibble: 3 x 3
    ##   nonnat_born IS3G04A                                     n
    ##         <dbl> <fct>                                   <int>
    ## 1           0 (1) Country of birth is country of test 89094
    ## 2           1 (0) Other                                3848
    ## 3          NA <NA>                                     1661
    ## 
    ## [[7]]
    ## # A tibble: 4 x 3
    ##   immigrantfam S_IMMIG                                                      n
    ##          <dbl> <fct>                                                    <int>
    ## 1            0 "(1) \tAt least one parent born in country"              84415
    ## 2            0 "(2) Students born in country but parent(s) born abroad"  4187
    ## 3            1 "(3) Students and parent(s) born abroad"                  2136
    ## 4           NA  <NA>                                                     3865
    ## 
    ## [[8]]
    ## # A tibble: 3 x 3
    ##   nonnat_lang IS3G05                                       n
    ##         <dbl> <fct>                                    <int>
    ## 1           0 (1) Language at home is language of test 85469
    ## 2           1 (0) Other                                 7266
    ## 3          NA <NA>                                      1868

Read in GDP data downloaded from IMF on 8 April, 2020 from source:
<https://www.imf.org/external/pubs/ft/weo/2018/02/weodata/index.aspx>
Manual download documentation: **Clicked on: “Download WEO Data: October
2018 Edition” –\> “By countries” –\> “All countries” **Downloaded 1st
file: “Gross domestic product per capita, constant prices - Purchasing
power parity; 2011 international dollars” **Downloaded 2nd file: “Gross
domestic product per capita, current prices U.S. dollars” Year range:
1998 - 2016 **The resulting xlsx saved in github “data” folder has 3
worksheets: 1st documents data; 2nd is GDP in constant ppp 2011; 3rd is
GDP current prices US.

QQ add here code for creating 4 GDP variables: gdp_constant
log_gdp_constant gdp_currentusd log_gdp_currentusd

``` r
# Load xls reading package
library(readxl)

# Import xlsx
readxl::excel_sheets("../data/GDP data downloaded from IMF_3worksheets_Apr2020.xlsx")
```

    ## [1] "Data Source"                  "GDP constant ppp 2011; intl$"
    ## [3] "GDP current prices US$"

``` r
# Read in worksheet: “GDP constant ppp 2011; intl$”
gdp_tbl1 <- readxl::read_excel("../data/GDP data downloaded from IMF_3worksheets_Apr2020.xlsx", 
                               sheet = "GDP constant ppp 2011; intl$")


# (1) Create <gdp_constant> var:
# Select for “ISO” in GDP worksheet that matches “COUNTRY” 3-letter variable in IEA data
# Select for year 2015 only - (column W)
gdp_tbl1 <- gdp_tbl1 %>% 
  filter(ISO %in% (tbl_allcountries_2016_2step %>% distinct(COUNTRY) %>% pull(COUNTRY))) %>% 
  select(COUNTRY = ISO, gdp_constant = `2015`) %>% 
  mutate(gdp_constant = as.numeric(gdp_constant))

gdp_tbl1
```

    ## # A tibble: 22 x 2
    ##    COUNTRY gdp_constant
    ##    <chr>          <dbl>
    ##  1 BGR           18114.
    ##  2 CHL           22205.
    ##  3 COL           12929.
    ##  4 HRV           20750.
    ##  5 DNK           44327.
    ##  6 DOM           14132.
    ##  7 EST           26894.
    ##  8 FIN           38610.
    ##  9 HKG           53457.
    ## 10 ITA           33831.
    ## # ... with 12 more rows
    ## # i Use `print(n = ...)` to see more rows

``` r
# (2) Create logged version of this var called “log_gdp_constant”
gdp_tbl1 <- gdp_tbl1 %>% 
  mutate(log_gdp_constant = log(gdp_constant))
  
# (3) Create <gdp_currentusd> var
# Read in worksheet: “GDP current prices US$”
# Same selection of “ISO” in GDP worksheet that matches “COUNTRY” var in IEA data
gdp_tbl2 <- readxl::read_excel("../data/GDP data downloaded from IMF_3worksheets_Apr2020.xlsx", 
                               sheet = "GDP current prices US$") %>% 
  filter(ISO %in% (tbl_allcountries_2016_2step %>% distinct(COUNTRY) %>% pull(COUNTRY))) %>% 
  select(COUNTRY = ISO, gdp_currentusd = `2015`) %>% 
  mutate(gdp_currentusd = as.numeric(gdp_currentusd))

# (4) Create logged version of this var called “log_gdp_currentusd”
gdp_tbl2 <- gdp_tbl2 %>% 
  mutate(log_gdp_currentusd = log(gdp_currentusd))

gdp_tbl <- left_join(gdp_tbl1, gdp_tbl2, by = "COUNTRY")

gdp_tbl
```

    ## # A tibble: 22 x 5
    ##    COUNTRY gdp_constant log_gdp_constant gdp_currentusd log_gdp_currentusd
    ##    <chr>          <dbl>            <dbl>          <dbl>              <dbl>
    ##  1 BGR           18114.             9.80           50.2               3.92
    ##  2 CHL           22205.            10.0           244.                5.50
    ##  3 COL           12929.             9.47          293.                5.68
    ##  4 HRV           20750.             9.94           49.5               3.90
    ##  5 DNK           44327.            10.7           301.                5.71
    ##  6 DOM           14132.             9.56           68.9               4.23
    ##  7 EST           26894.            10.2            22.6               3.12
    ##  8 FIN           38610.            10.6           233.                5.45
    ##  9 HKG           53457.            10.9           309.                5.73
    ## 10 ITA           33831.            10.4          1834.                7.51
    ## # ... with 12 more rows
    ## # i Use `print(n = ...)` to see more rows

Now combine these new variables into existing dataset:
`tbl_allcountries_2016_2step`.

``` r
tbl_allcountries_2016_2step <- tbl_allcountries_2016_2step %>% 
  mutate(COUNTRY = as.character(COUNTRY)) %>% 
  left_join(gdp_tbl, by = "COUNTRY")

tbl_allcountries_2016_2step %>% head()
```

    ## # A tibble: 6 x 49
    ##   ICCS_~1 COUNTRY IDCNTRY IDSTUD IS3G23L IS3G23I IS3G23H IS3G23K IS3G23J IS3G23A
    ##     <dbl> <chr>     <dbl>  <dbl> <fct>   <fct>   <fct>   <fct>   <fct>   <fct>  
    ## 1    2016 BFL         956 1.00e7 (2) Qu~ (1) Ve~ (2) Qu~ (2) Qu~ (1) Ve~ (2) Qu~
    ## 2    2016 BFL         956 1.00e7 (1) Ve~ (2) Qu~ (2) Qu~ (1) Ve~ (2) Qu~ (1) Ve~
    ## 3    2016 BFL         956 1.00e7 (2) Qu~ (1) Ve~ (1) Ve~ (2) Qu~ (1) Ve~ (2) Qu~
    ## 4    2016 BFL         956 1.00e7 (1) Ve~ (2) Qu~ (2) Qu~ (1) Ve~ (2) Qu~ (2) Qu~
    ## 5    2016 BFL         956 1.00e7 (1) Ve~ (2) Qu~ (2) Qu~ (1) Ve~ (2) Qu~ (1) Ve~
    ## 6    2016 BFL         956 1.00e7 (1) Ve~ (1) Ve~ (1) Ve~ (1) Ve~ (1) Ve~ (3) No~
    ## # ... with 39 more variables: IS3G23C <fct>, IS3G23E <fct>, IS3G23D <fct>,
    ## #   IS3G23G <fct>, IS3G23F <fct>, IS3G23B <fct>, S_GENDER <fct>, IS3G11 <fct>,
    ## #   IS3G03 <fct>, IS3G07 <fct>, IS3G09 <fct>, TOTWGTS <dbl>, IS3G04A <fct>,
    ## #   S_IMMIG <fct>, IS3G05 <fct>, IS3G23L_bin <fct>, IS3G23I_bin <fct>,
    ## #   IS3G23H_bin <fct>, IS3G23K_bin <fct>, IS3G23J_bin <fct>, IS3G23A_bin <fct>,
    ## #   IS3G23C_bin <fct>, IS3G23E_bin <fct>, IS3G23D_bin <fct>, IS3G23G_bin <fct>,
    ## #   IS3G23F_bin <fct>, IS3G23B_bin <fct>, female <dbl>, books <dbl>, ...
    ## # i Use `colnames()` to see all variable names

Select for vars to prepare for export.

``` r
tbl_allcountries_2016_2step <- tbl_allcountries_2016_2step %>%
  select(ICCS_year,
         COUNTRY,
         IDSTUD,
         TOTWGTS,
         obey    = IS3G23L_bin,
         rights  = IS3G23I_bin,
         local   = IS3G23H_bin,
         work    = IS3G23K_bin,
         envir   = IS3G23J_bin,
         vote    = IS3G23A_bin,
         history = IS3G23C_bin,
         respect = IS3G23E_bin,
         news    = IS3G23D_bin,
         protest = IS3G23G_bin,
         discuss = IS3G23F_bin,
         party   = IS3G23B_bin,
         female,
         books,
         edexp,
         ed_mom,
         ed_dad,
         nonnat_born,
         immigrantfam,
         nonnat_lang,
         gdp_constant,
         log_gdp_constant,
         gdp_currentusd,
         log_gdp_currentusd)
```

Convert citizenship norm indicators to integer (0 = “not important”, 1 =
“important”) in preparation for LG export

``` r
tbl_allcountries_2016_2step <- tbl_allcountries_2016_2step %>% 
  mutate(
    across(c(obey, rights, local, work, envir, vote, history, 
             respect, news, protest, discuss, party),
           ~ case_when(
              .x == "not important" ~ 0L,
              .x == "important"     ~ 1L,
              TRUE~NA_integer_
            ))
  ) %>% 
  mutate(
    ICCS_year = as.integer(ICCS_year),
    COUNTRY   = as.character(COUNTRY)
  )

 
tbl_allcountries_2016_2step %>% head()
```

    ## # A tibble: 6 x 28
    ##   ICCS_year COUNTRY  IDSTUD TOTWGTS  obey rights local  work envir  vote history
    ##       <int> <chr>     <dbl>   <dbl> <int>  <int> <int> <int> <int> <int>   <int>
    ## 1      2016 BFL      1.00e7    22.5     1      1     1     1     1     1       0
    ## 2      2016 BFL      1.00e7    22.5     1      1     1     1     1     1       0
    ## 3      2016 BFL      1.00e7    22.5     1      1     1     1     1     1       0
    ## 4      2016 BFL      1.00e7    22.5     1      1     1     1     1     1       1
    ## 5      2016 BFL      1.00e7    22.5     1      1     1     1     1     1       0
    ## 6      2016 BFL      1.00e7    22.5     1      1     1     1     1     0       0
    ## # ... with 17 more variables: respect <int>, news <int>, protest <int>,
    ## #   discuss <int>, party <int>, female <dbl>, books <dbl>, edexp <dbl>,
    ## #   ed_mom <dbl>, ed_dad <dbl>, nonnat_born <dbl>, immigrantfam <dbl>,
    ## #   nonnat_lang <dbl>, gdp_constant <dbl>, log_gdp_constant <dbl>,
    ## #   gdp_currentusd <dbl>, log_gdp_currentusd <dbl>
    ## # i Use `colnames()` to see all variable names

``` r
write_delim(tbl_allcountries_2016_2step, "output/tbl_allcountries_2016_2step.dat", delim = ",")
```
