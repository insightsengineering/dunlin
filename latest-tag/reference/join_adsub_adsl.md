# Join `adsub` to `adsl`

Join `adsub` to `adsl`

## Usage

``` r
join_adsub_adsl(
  adam_db,
  keys,
  continuous_var,
  categorical_var,
  continuous_suffix,
  categorical_suffix,
  drop_na = TRUE,
  drop_lvl = TRUE
)

# S3 method for class 'list'
join_adsub_adsl(
  adam_db,
  keys = c("USUBJID", "STUDYID"),
  continuous_var = "all",
  categorical_var = "all",
  continuous_suffix = "",
  categorical_suffix = "_CAT",
  drop_na = TRUE,
  drop_lvl = FALSE
)
```

## Arguments

- adam_db:

  (`list` of `data.frame`) object input with an `adsl` and `adsub`
  table.

- keys:

  (`character`) the name of the columns in `adsl` uniquely identifying a
  row.

- continuous_var:

  (`character`) the value of a parameter in the `PARAMCD` column of the
  `adsub` table from which columns containing continuous values should
  be created. If `"all"`, all parameter values are selected, if `NULL`,
  none are selected.

- categorical_var:

  (`character`) the value of a parameter in the `PARAMCD` column of the
  `adsub` table from which columns containing categorical values should
  be created. If `"all"`, all parameter values are selected, if `NULL`,
  none are selected.

- continuous_suffix:

  (`string`) the suffixes to add to the newly generated columns
  containing continuous values.

- categorical_suffix:

  (`string`) the suffixes to add to the newly generated columns
  containing categorical values.

- drop_na:

  (`logical`) whether resulting columns containing only `NAs` should be
  dropped.

- drop_lvl:

  (`logical`) should missing levels be dropped in the resulting columns.

## Value

a `list` of `data.frame` with new columns in the `adsl` table.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c("S1", "S2", "S3", "S4"),
  STUDYID = "My_study",
  AGE = c(60, 44, 23, 31)
)

adsub <- data.frame(
  USUBJID = c("S1", "S2", "S3", "S4", "S1", "S2", "S3"),
  STUDYID = "My_study",
  PARAM = c("weight", "weight", "weight", "weight", "height", "height", "height"),
  PARAMCD = c("w", "w", "w", "w", "h", "h", "h"),
  AVAL = c(98, 75, 70, 71, 182, 155, 152),
  AVALC = c(">80", "<=80", "<=80", "<=80", ">180", "<=180", "<=180")
)

db <- list(adsl = adsl, adsub = adsub)

x <- join_adsub_adsl(adam_db = db)
x <- join_adsub_adsl(adam_db = db, continuous_var = c("w", "h"), categorical_var = "h")
```
