---
title: "Upgrading R version with all your packages"
excerpt: "Learn how to do a painless upgrade of your R version"
date: 2022-05-06
author: "Fabrício Almeida-Silva"
draft: false
# layout options: single, single-sidebar
layout: single
categories:
- rstats
- project management
---

## Motivation

Have you ever upgraded R and lost all of your packages? As a consequence,
you had to install them again one by one. One. By. One. Oh, man... Boring, huh?
Here, I will guide you on how to upgrade your R version and reinstall your 
packages automatically. This way, you can spend your time on what really 
matters: writing some cool R code! This post is inspired by 
[this Gist code](https://gist.github.com/jthomasmock/65366b9c1fd750347b9de1d2c80ed827).


## 'Taking a picture' of your current R package universe

The first thing you need to do before upgrading your R version is to save
a list of all packages you have installed. Not only must you have package names,
but also from where they were downloaded (e.g., CRAN, Bioconductor, GitHub,
etc.). The code below will create a data frame of packages and their sources,
and save it as a .csv file in your current working directory.

**NOTE:** You need to have the packages `tidyverse` and `sessioninfo` installed.


```r
#----Create a data frame with all installed packages and their sources---------
library(tidyverse)
all_pkg <- sessioninfo::session_info("installed") |> 
  pluck("packages") |> 
  as_tibble()

# Classify sources: CRAN, Bioconductor, GitHub, r-universe, and local
split_repo <- all_pkg |> 
    mutate(repo = case_when(
        str_detect(source, "Bioconductor") ~ "Bioconductor",
        str_detect(source, "CRAN") ~ "CRAN",
        str_detect(source, "Github") ~ "GitHub",
        str_detect(source, "local") ~ "local",
        str_detect(source, "r-universe") ~ "r-universe",
        TRUE ~ NA_character_
    ), .before = "source") |>
    select(package, repo)

head(split_repo)
```

```
## # A tibble: 6 × 2
##   package       repo        
##   <chr>         <chr>       
## 1 abind         CRAN        
## 2 airway        Bioconductor
## 3 annotate      Bioconductor
## 4 AnnotationDbi Bioconductor
## 5 AnnotationHub Bioconductor
## 6 ape           CRAN
```

```r
split_repo |> 
  write_csv("packages.csv")
```

Now that you have a list of all your packages and their sources, you can
install the latest version of R. That will vary according to the operating 
system you use, so you'd better go to the [CRAN page](https://cran.rstudio.com/)
and see the instructions on how to upgrade R for your case. In my case,
on a Ubuntu 20.04 LTS machine, I just ran:


```bash
sudo apt-get update
sudo apt-get upgrade
```

Once you're done upgrading your R version, open a new R session (now with
the latest version) and run the following code to install all your beloved
packages:


```r
#----First, install tidyverse, remotes, and BiocManager-------------------------
if(!require("tidyverse", quietly = TRUE))
    install.packages("tidyverse")

if(!require("remotes", quietly = TRUE))
    install.packages("remotes")

if(!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

#----Read data frame with package names and sources-----------------------------
library(tidyverse)
all_pkg <- readr::read_csv("packages.csv", show_col_types = FALSE)

#----Reinstall packages---------------------------------------------------------
## CRAN packages
cran_pkg <- all_pkg |> 
  dplyr::filter(repo == "CRAN") |> 
  dplyr::pull(package)

cran_pkg |>
  install.packages()


## Bioconductor packages
bioc_pkg <- all_pkg |>
    dplyr::filter(repo == "Bioconductor") |>
    dplyr::pull(package)

bioc_pkg |>
    BiocManager::install()


## R-Universe
runi_pkg <- all_pkg |>
    dplyr::filter(repo == "r-universe") |>
    dplyr::pull(package)

runi_pkg |>
    install.packages(repos = "https://ropensci.r-universe.dev")


## GitHub packages - only list packages
gh_pkg <- all_pkg |>
    dplyr::filter(repo == "GitHub") |>
    dplyr::pull(package)
```

For GitHub packages, I suggest looking at them one by one (there shouldn't be
many of them) and deciding which ones you want to install, as many of them
are usually unstable and installed for testing purposes. Once you have
identified which ones you want to install, you can do it 
with `remotes::install_github()`.

And that's all! Whenever you need to upgrade R, just run the same code again
and you're all set. I hope this post helped you!

## Session information


```r
sessioninfo::session_info()
```

```
## ─ Session info ───────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.2.0 (2022-04-22)
##  os       Ubuntu 20.04.4 LTS
##  system   x86_64, linux-gnu
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       Europe/Brussels
##  date     2022-05-06
##  pandoc   2.17.1.1 @ /usr/lib/rstudio/bin/quarto/bin/ (via rmarkdown)
## 
## ─ Packages ───────────────────────────────────────────────────────────────────
##  package     * version date (UTC) lib source
##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.2.0)
##  backports     1.4.1   2021-12-13 [1] CRAN (R 4.2.0)
##  bit           4.0.4   2020-08-04 [1] CRAN (R 4.2.0)
##  bit64         4.0.5   2020-08-30 [1] CRAN (R 4.2.0)
##  blogdown      1.9     2022-03-28 [1] CRAN (R 4.2.0)
##  bookdown      0.26    2022-04-15 [1] CRAN (R 4.2.0)
##  broom         0.8.0   2022-04-13 [1] CRAN (R 4.2.0)
##  bslib         0.3.1   2021-10-06 [1] CRAN (R 4.2.0)
##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.2.0)
##  cli           3.3.0   2022-04-25 [1] CRAN (R 4.2.0)
##  colorspace    2.0-3   2022-02-21 [1] CRAN (R 4.2.0)
##  crayon        1.5.1   2022-03-26 [1] CRAN (R 4.2.0)
##  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.2.0)
##  dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.2.0)
##  digest        0.6.29  2021-12-01 [1] CRAN (R 4.2.0)
##  dplyr       * 1.0.9   2022-04-28 [1] CRAN (R 4.2.0)
##  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
##  evaluate      0.15    2022-02-18 [1] CRAN (R 4.2.0)
##  fansi         1.0.3   2022-03-24 [1] CRAN (R 4.2.0)
##  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.2.0)
##  forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.2.0)
##  fs            1.5.2   2021-12-08 [1] CRAN (R 4.2.0)
##  generics      0.1.2   2022-01-31 [1] CRAN (R 4.2.0)
##  ggplot2     * 3.3.6   2022-05-03 [1] CRAN (R 4.2.0)
##  glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
##  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.2.0)
##  haven         2.5.0   2022-04-15 [1] CRAN (R 4.2.0)
##  hms           1.1.1   2021-09-26 [1] CRAN (R 4.2.0)
##  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.2.0)
##  httr          1.4.3   2022-05-04 [1] CRAN (R 4.2.0)
##  jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.2.0)
##  jsonlite      1.8.0   2022-02-22 [1] CRAN (R 4.2.0)
##  knitr         1.39    2022-04-26 [1] CRAN (R 4.2.0)
##  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.2.0)
##  lubridate     1.8.0   2021-10-07 [1] CRAN (R 4.2.0)
##  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
##  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.2.0)
##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.0)
##  pillar        1.7.0   2022-02-01 [1] CRAN (R 4.2.0)
##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
##  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.2.0)
##  R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
##  readr       * 2.1.2   2022-01-30 [1] CRAN (R 4.2.0)
##  readxl        1.4.0   2022-03-28 [1] CRAN (R 4.2.0)
##  reprex        2.0.1   2021-08-05 [1] CRAN (R 4.2.0)
##  rlang         1.0.2   2022-03-04 [1] CRAN (R 4.2.0)
##  rmarkdown     2.14    2022-04-25 [1] CRAN (R 4.2.0)
##  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.2.0)
##  rvest         1.0.2   2021-10-16 [1] CRAN (R 4.2.0)
##  sass          0.4.1   2022-03-23 [1] CRAN (R 4.2.0)
##  scales        1.2.0   2022-04-13 [1] CRAN (R 4.2.0)
##  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
##  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.2.0)
##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.2.0)
##  tibble      * 3.1.7   2022-05-03 [1] CRAN (R 4.2.0)
##  tidyr       * 1.2.0   2022-02-01 [1] CRAN (R 4.2.0)
##  tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.2.0)
##  tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.2.0)
##  tzdb          0.3.0   2022-03-28 [1] CRAN (R 4.2.0)
##  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.2.0)
##  vctrs         0.4.1   2022-04-13 [1] CRAN (R 4.2.0)
##  vroom         1.5.7   2021-11-30 [1] CRAN (R 4.2.0)
##  withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
##  xfun          0.30    2022-03-02 [1] CRAN (R 4.2.0)
##  xml2          1.3.3   2021-11-30 [1] CRAN (R 4.2.0)
##  yaml          2.3.5   2022-02-21 [1] CRAN (R 4.2.0)
## 
##  [1] /home/faalm/R/x86_64-pc-linux-gnu-library/4.2
##  [2] /usr/local/lib/R/site-library
##  [3] /usr/lib/R/site-library
##  [4] /usr/lib/R/library
## 
## ──────────────────────────────────────────────────────────────────────────────
```
