# Dunlin: Data tools for Pharma

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/dunlin/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/dunlin/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/dunlin/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/dunlin/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/dunlin/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/dunlin/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/dunlin?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/dunlin?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/dunlin)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/dunlin)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/dunlin)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/dunlin)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/dunlin)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/dunlin)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/dunlin/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/dunlin/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/dunlin?color=red\&label=open%20issues)](https://github.com/insightsengineering/dunlin/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

`dunlin` provides a variety of data tools to reformat and manipulate a subset of the tables in a data set stored.

## Installation

It is recommended that you [create and use a GitHub PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/dunlin@*release")
```

## Usage

  ```r
  library(dm)
  library(dunlin)

  db <- dm_nycflights13()

  new_carrier <- c(NA, "", as.character(db$airlines$carrier[-c(1, 2)]))
  new_name <- c(NA, "", as.character(db$airlines$name[-c(1, 2)]))

  db <- db %>%
    dm_zoom_to("airlines") %>%
    mutate(
      carrier = new_carrier,
      name = new_name
    ) %>%
    dm_update_zoomed()

  db$airlines
  ```

  which returns `airlines` as

  ```text
  # A tibble: 15 × 2
     carrier name
     <chr>   <chr>
   1  NA      NA
   2 ""      ""
   3 "AS"    "Alaska Airlines Inc."
   4 "B6"    "JetBlue Airways"
   5 "DL"    "Delta Air Lines Inc."
   6 "EV"    "ExpressJet Airlines Inc."
   7 "F9"    "Frontier Airlines Inc."
   8 "FL"    "AirTran Airways Corporation"
   9 "HA"    "Hawaiian Airlines Inc."
  10 "MQ"    "Envoy Air"
  11 "UA"    "United Air Lines Inc."
  12 "US"    "US Airways Inc."
  13 "VX"    "Virgin America"
  14 "WN"    "Southwest Airlines Co."
  15 "YV"    "Mesa Airlines Inc."
  ```

  ```r
  new_format <- list(
    airlines = list(
      carrier = rule("No Coding available" = c("", NA, "<Missing>")),
      name = rule("<Missing>" = c("", NA, "<Missing>"))
    )
  )

  db <- dunlin::reformat(db, new_format, na_last = TRUE)

  db$airlines
  ```

  which reformulates `airlines` as

  ```text
  # A tibble: 15 × 2
     carrier             name
     <fct>               <fct>
   1 No Coding available <Missing>
   2 No Coding available <Missing>
   3 AS                  Alaska Airlines Inc.
   4 B6                  JetBlue Airways
   5 DL                  Delta Air Lines Inc.
   6 EV                  ExpressJet Airlines Inc.
   7 F9                  Frontier Airlines Inc.
   8 FL                  AirTran Airways Corporation
   9 HA                  Hawaiian Airlines Inc.
  10 MQ                  Envoy Air
  11 UA                  United Air Lines Inc.
  12 US                  US Airways Inc.
  13 VX                  Virgin America
  14 WN                  Southwest Airlines Co.
  15 YV                  Mesa Airlines Inc.
  ```
