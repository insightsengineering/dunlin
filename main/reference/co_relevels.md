# Reorder Two Columns Levels Simultaneously

Reorder Two Columns Levels Simultaneously

## Usage

``` r
co_relevels(df, primary, secondary, levels_primary)
```

## Arguments

- df:

  (`data.frame`) with two column whose factors should be reordered.

- primary:

  (`string`) the name of the column on which the levels reordering
  should be based.

- secondary:

  (`string`) the name of the column whose levels should be reordered
  following the levels of the primary column.

- levels_primary:

  (`character`) the levels in the desired order. Existing levels that
  are not included will be placed afterward in their current order.

## Value

a `data.frame` with the `secondary` column converted to factor with
reordered levels.

## Details

The function expect a 1:1 matching between the elements of the two
selected column.

## Examples

``` r
df <- data.frame(
  SUBJID = 1:3,
  PARAMCD = factor(c("A", "B", "C")),
  PARAM = factor(paste("letter", LETTERS[1:3]))
)
co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("C", "A", "B"))
#>   SUBJID PARAMCD    PARAM
#> 1      1       A letter A
#> 2      2       B letter B
#> 3      3       C letter C
```
