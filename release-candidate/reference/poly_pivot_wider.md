# Transforming data.frame with multiple Data Column into Wide Format

Transforming data.frame with multiple Data Column into Wide Format

## Usage

``` r
poly_pivot_wider(
  data,
  id,
  param_from,
  value_from,
  labels_from = NULL,
  drop_na = TRUE,
  drop_lvl = FALSE
)
```

## Arguments

- data:

  (`data.frame`) to be pivoted.

- id:

  (`character`) the name of the columns whose combination uniquely
  identify the observations.

- param_from:

  (`character`) the name of the columns containing the names of the
  parameters to be pivoted. The unique values in this column will become
  column names in the output.

- value_from:

  (`character`) the name of the column containing the values that will
  populate the output.

- labels_from:

  (`character`) the name of the column congaing the labels of the new
  columns. from. If not provided, the labels will be equal to the column
  names. When several labels are available for the same column, the
  first one will be selected.

- drop_na:

  (`logical`) should column containing only `NAs` be dropped.

- drop_lvl:

  (`logical`) should missing levels be dropped in the columns coming
  from `value_from`.

## Value

`list` of `data.frame` in a wide format with label attribute attached to
each columns.

## Details

This function is adapted to cases where the data are distributed in
several columns while the name of the parameter is in one. Typical
example is `adsub` where numeric data are stored in `AVAL` while
categorical data are in `AVALC`.

## Examples

``` r
test_data <- data.frame(
  the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
  the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
  the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "other"),
  the_label = c(
    "Weight (Kg)", "Height (cm)", "Gender", "Weight (Kg)",
    "Gender", "Height (cm)", "Height (cm)", "Pre-condition"
  ),
  the_val = c(65, 165, NA, 66, NA, 166, 155, NA),
  the_val2 = c(65, 165, "M", 66, "F", 166, 155, TRUE)
)

x <- poly_pivot_wider(
  test_data,
  c("the_obs", "the_obs2"),
  "the_param",
  c("the_val", "the_val2"),
  "the_label"
)
x
#> $the_val
#>   the_obs the_obs2 height weight
#> 1       A       Ax    165     65
#> 2       B       Bx    166     66
#> 3       C       Cx    155     NA
#> 4       D       Dx     NA     NA
#> 
#> $the_val2
#>   the_obs the_obs2 gender height other weight
#> 1       A       Ax      M    165  <NA>     65
#> 2       B       Bx      F    166  <NA>     66
#> 3       C       Cx   <NA>    155  <NA>   <NA>
#> 4       D       Dx   <NA>   <NA>  TRUE   <NA>
#> 
Reduce(function(u, v) merge(u, v, all = TRUE), x)
#>   the_obs the_obs2 height weight gender other
#> 1       A       Ax    165     65      M  <NA>
#> 2       B       Bx    166     66      F  <NA>
#> 3       C       Cx    155     NA   <NA>  <NA>
#> 4       D       Dx     NA     NA   <NA>  TRUE
```
