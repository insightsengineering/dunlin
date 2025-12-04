# Transforming data.frame with Multiple Identifying columns into Wide Format

Transforming data.frame with Multiple Identifying columns into Wide
Format

## Usage

``` r
multi_id_pivot_wider(
  data,
  id,
  param_from,
  value_from,
  drop_na = FALSE,
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

  (`character`) the name of the column containing the names of the
  parameters to be pivoted. The unique values in this column will become
  column names in the output.

- value_from:

  (`character`) the name of the column containing the values that will
  populate the output.

- drop_na:

  (`logical`) should column containing only `NAs` be dropped.

- drop_lvl:

  (`logical`) should missing levels be dropped in the columns coming
  from (`value_from`).

## Value

`data.frame` in a wide format.

## Details

This function allows to identify observations on the basis of several
columns. Warning: Instead of nesting duplicated values, the function
will throw an error if the same parameter is provided twice for the same
observation.

## Examples

``` r
test_data <- data.frame(
  the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
  the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
  the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "other"),
  the_val = c(65, 165, "M", 66, "F", 166, 155, TRUE)
)

multi_id_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val")
#>   the_obs the_obs2 gender height other weight
#> 1       A       Ax      M    165  <NA>     65
#> 2       B       Bx      F    166  <NA>     66
#> 3       C       Cx   <NA>    155  <NA>   <NA>
#> 4       D       Dx   <NA>   <NA>  TRUE   <NA>
multi_id_pivot_wider(test_data, "the_obs2", "the_param", "the_val")
#>   the_obs2 gender height other weight
#> 1       Ax      M    165  <NA>     65
#> 2       Bx      F    166  <NA>     66
#> 3       Cx   <NA>    155  <NA>   <NA>
#> 4       Dx   <NA>   <NA>  TRUE   <NA>
```
