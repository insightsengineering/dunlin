# Create Subject-level Flag from Long Data

Utility for creating subject-level flags from data frames that are more
than one line per subject. For example, use this function to create a
flag indicating whether a subject experienced any serious adverse
events.

The function works by first creating a logical variable in `data_long`
indicating whether the condition passed in the `...` argument is met. If
a subject has true on any row, then the new variable is added to `data`
as `TRUE`, otherwise that subject's value is populated with a `FALSE`.

## Usage

``` r
subject_level_flag(data, data_long, ..., .key = "USUBJID")
```

## Arguments

- data:

  (`data.frame`)  
  a subject-level data frame, e.g. `adsl`

- data_long:

  (`data.frame`)  
  a long data frame that is more than one line per subject, e.g. `adae`.
  The expressions passed in `...` will be evaluated in this data frame.

- ...:

  ([`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html))  
  Name and condition pairs. The name is the name of the new
  subject-level flag column. The condition is an expression that results
  in a logical vector. These name-condition pairs are passed directly to
  `dplyr::mutate(...)`.

- .key:

  (`character`)  
  Key columns create flags within and to merge by. Default is
  `'USUBJID'`

## Value

Subject-level data frame

## Examples

``` r
adsl <- tibble::tribble(
  ~USUBJID,      ~SEX,
  "01-701-1015", "F",
  "01-701-1023", "M",
  "01-701-1028", "M"
)

adae <- tibble::tribble(
  ~USUBJID,      ~AESER, ~AEACN,
  "01-701-1015", "Y",    "DOSE NOT CHANGED",
  "01-701-1015", "N",    "DOSE NOT CHANGED",
  "01-701-1028", "N",    "DRUG WITHDRAWN"
)

subject_level_flag(
  data = adsl,
  data_long = adae,
  ANY_AESER = AESER == "Y",
  ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN"
)
#> # A tibble: 3 × 4
#>   USUBJID     SEX   ANY_AESER ANY_DRUG_WITHDRAWN
#>   <chr>       <chr> <lgl>     <lgl>             
#> 1 01-701-1015 F     TRUE      FALSE             
#> 2 01-701-1023 M     FALSE     FALSE             
#> 3 01-701-1028 M     FALSE     TRUE              
```
