# `dunlin`: Tools for Clinical Trial Data Wrangling

[![Check
🛠](https://github.com/insightsengineering/dunlin/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/dunlin/main/unit-test-report/)
[![Docs
📚](https://github.com/insightsengineering/dunlin/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/dunlin/)
[![Code Coverage
📔](https://raw.githubusercontent.com/insightsengineering/dunlin/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/dunlin/main/coverage-report/)

![GitHub
forks](https://img.shields.io/github/forks/insightsengineering/dunlin?style=social)![GitHub
repo
stars](https://img.shields.io/github/stars/insightsengineering/dunlin?style=social)

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/insightsengineering/dunlin)![GitHub
contributors](https://img.shields.io/github/contributors/insightsengineering/dunlin)![GitHub
last
commit](https://img.shields.io/github/last-commit/insightsengineering/dunlin)![GitHub
pull
requests](https://img.shields.io/github/issues-pr/insightsengineering/dunlin)![GitHub
repo
size](https://img.shields.io/github/repo-size/insightsengineering/dunlin)![GitHub
language
count](https://img.shields.io/github/languages/count/insightsengineering/dunlin)[![Project
Status: Active – The project has reached a stable, usable state and is
being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/insightsengineering/dunlin/main?color=purple&label=package%20version)](https://github.com/insightsengineering/dunlin/tree/main)
[![Open
Issues](https://img.shields.io/github/issues-raw/insightsengineering/dunlin?color=red&label=open%20issues)](https://github.com/insightsengineering/dunlin/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)

`dunlin` provides a variety of data tools to reformat and manipulate a
subset of the tables in a data set.

## Installation

``` r
# install.packages("pak")
pak::pak("insightsengineering/dunlin@*release")
```

Alternatively, you might also use the development version.

``` r
# install.packages("pak")
pak::pak("insightsengineering/dunlin")
```

## Usage

``` r
library(dunlin)

df1 <- data.frame(
  "id" = c("a", "b", NA, "a", "k", "x"),
  "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
  "val" = letters[1:6]
)
df2 <- data.frame(
  "id" = c("a", "b", NA, "a", "k", "x"),
  "id2" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
  "num" = 1:6
)

db <- list(df1 = df1, df2 = df2)

prop_db <- propagate(db, "df1", "val", c("id", "id2"))
```

which returns `prop_db` as

``` text
 $df1
    id  id2 val
1    a   f1   a
2    b   f2   b
3 <NA> <NA>   c
4    a <NA>   d
5    k   f1   e
6    x   f1   f

$df2
    id  id2 num val
1    a   f1   1   a
2    b   f2   2   b
3 <NA> <NA>   3   c
4    a <NA>   4   d
5    k   f1   5   e
6    x   f1   6   f
```

``` r
new_format <- list(
  df1 = list(
    id = rule("No ID available" = c("", NA, "<Missing>")),
    id2 = rule("<Missing>" = c("", NA, "<Missing>"))
  )
)

res <- reformat(prop_db, new_format, .na_last = TRUE)
```

which result in `res` as

``` text
$df1
               id       id2 val
1               a        f1   a
2               b        f2   b
3 No ID available <Missing>   c
4               a <Missing>   d
5               k        f1   e
6               x        f1   f

$df2
    id  id2 num val
1    a   f1   1   a
2    b   f2   2   b
3 <NA> <NA>   3   c
4    a <NA>   4   d
5    k   f1   5   e
6    x   f1   6   f
```

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over
time](https://starchart.cc/insightsengineering/dunlin.svg)](https://starchart.cc/insightsengineering/dunlin)

### Stargazers

[![Stargazers repo roster for
dunlin](https://reporoster.com/stars/insightsengineering/dunlin)](https://github.com/insightsengineering/dunlin/stargazers)

[![Forkers repo roster for
dunlin](https://reporoster.com/forks/insightsengineering/dunlin)](https://github.com/insightsengineering/dunlin/network/members)
