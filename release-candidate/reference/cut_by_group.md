# Cutting data by group

Cutting data by group

## Usage

``` r
cut_by_group(df, col_data, col_group, group, cat_col)
```

## Arguments

- df:

  (`dataframe`) with a column of data to be cut and a column specifying
  the group of each observation.

- col_data:

  (`character`) the column containing the data to be cut.

- col_group:

  (`character`) the column containing the names of the groups according
  to which the data should be split.

- group:

  (`nested list`) providing for each parameter value that should be
  analyzed in a categorical way: the name of the parameter
  (`character`), a series of breakpoints (`numeric`) where the first
  breakpoints is typically `-Inf` and the last `Inf`, and a series of
  name which will describe each category (`character`).

- cat_col:

  (`character`) the name of the new column in which the cut label should
  he stored.

## Value

`data.frame` with a column containing categorical values.

## Details

Function used to categorize numeric data stored in long format depending
on their group. Intervals are closed on the right (and open on the
left).

## Examples

``` r
group <- list(
  list(
    "Height",
    c(-Inf, 150, 170, Inf),
    c("=<150", "150-170", ">170")
  ),
  list(
    "Weight",
    c(-Inf, 65, Inf),
    c("=<65", ">65")
  ),
  list(
    "Age",
    c(-Inf, 31, Inf),
    c("=<31", ">31")
  ),
  list(
    "PreCondition",
    c(-Inf, 1, Inf),
    c("=<1", "<1")
  )
)
data <- data.frame(
  SUBJECT = rep(letters[1:10], 4),
  PARAM = rep(c("Height", "Weight", "Age", "other"), each = 10),
  AVAL = c(rnorm(10, 165, 15), rnorm(10, 65, 5), runif(10, 18, 65), rnorm(10, 0, 1)),
  index = 1:40
)

cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories")
#>    SUBJECT  PARAM         AVAL index my_new_categories
#> 1        a Height 174.43473063     1              >170
#> 2        b Height 195.97537343     2              >170
#> 3        c Height 140.53515897     3             =<150
#> 4        d Height 172.68640425     4              >170
#> 5        e Height 137.05482762     5             =<150
#> 6        f Height 157.16981228     6           150-170
#> 7        g Height 164.21097135     7           150-170
#> 8        h Height 173.14494514     8              >170
#> 9        i Height 151.28887759     9           150-170
#> 10       j Height 172.02231631    10              >170
#> 11       a Weight  66.81475628    11               >65
#> 12       b Weight  58.47728227    12              =<65
#> 13       c Weight  68.68888161    13               >65
#> 14       d Weight  74.44252465    14               >65
#> 15       e Weight  64.51277448    15              =<65
#> 16       f Weight  60.32076323    16              =<65
#> 17       g Weight  64.92024844    17              =<65
#> 18       h Weight  60.86605523    18              =<65
#> 19       i Weight  57.43800174    19              =<65
#> 20       j Weight  69.67681595    20               >65
#> 21       a    Age  44.79211270    21               >31
#> 22       b    Age  33.77879679    22               >31
#> 23       c    Age  46.02435108    23               >31
#> 24       d    Age  27.00134748    24              =<31
#> 25       e    Age  62.54490507    25               >31
#> 26       f    Age  43.49657921    26               >31
#> 27       g    Age  43.59635949    27               >31
#> 28       h    Age  31.09406624    28               >31
#> 29       i    Age  38.99501605    29               >31
#> 30       j    Age  35.46102567    30               >31
#> 31       a  other  -1.91008747    31              <NA>
#> 32       b  other  -0.27923724    32              <NA>
#> 33       c  other  -0.31344598    33              <NA>
#> 34       d  other   1.06730788    34              <NA>
#> 35       e  other   0.07003485    35              <NA>
#> 36       f  other  -0.63912332    36              <NA>
#> 37       g  other  -0.04996490    37              <NA>
#> 38       h  other  -0.25148344    38              <NA>
#> 39       i  other   0.44479712    39              <NA>
#> 40       j  other   2.75541758    40              <NA>
```
