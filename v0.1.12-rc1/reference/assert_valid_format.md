# Assert Nested List can be used as Format Argument in Reformat.

Assert Nested List can be used as Format Argument in Reformat.

## Usage

``` r
assert_valid_format(object)
```

## Arguments

- object:

  (`list`) to assert.

## Value

invisible `TRUE` or an error message if the criteria are not fulfilled.

## Examples

``` r
format <- list(
  df1 = list(
    var1 = rule("X" = "x", "N" = c(NA, ""))
  ),
  df2 = list(
    var1 = rule(),
    var2 = rule("f11" = "F11", "NN" = NA)
  ),
  df3 = list()
)

assert_valid_format(format)
```
