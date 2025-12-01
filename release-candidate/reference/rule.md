# Create rule based on mappings

Create rule based on mappings

## Usage

``` r
rule(
  ...,
  .lst = list(...),
  .string_as_fct = TRUE,
  .na_last = TRUE,
  .drop = FALSE,
  .to_NA = ""
)
```

## Arguments

- ...:

  Mapping pairs, the argument name is the transformed while its values
  are original values.

- .lst:

  (`list`) of mapping.

- .string_as_fct:

  (`flag`) whether to convert characters to factors.

- .na_last:

  (`flag`) whether the level replacing `NA` should be last.

- .drop:

  (`flag`) whether to drop empty levels.

- .to_NA:

  (`character`) values that should be converted to `NA`. Set to `NULL`
  if nothing should be converted to `NA`.

## Value

a `rule` object.

## Note

Conversion to `NA` is the last step of the remapping process.

## Examples

``` r
rule("X" = "x", "Y" = c("y", "z"))
#> Mapping of:
#> X  <-  "x" 
#> Y  <-  "y", "z" 
#> Convert to <NA>: "" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
rule("X" = "x", "Y" = c("y", "z"), .drop = TRUE, .to_NA = c("a", "b"), .na_last = FALSE)
#> Mapping of:
#> X  <-  "x" 
#> Y  <-  "y", "z" 
#> Convert to <NA>: "a", "b" 
#> Convert to factor: TRUE 
#> Drop unused level: TRUE 
#> NA-replacing level in last position: FALSE 
```
