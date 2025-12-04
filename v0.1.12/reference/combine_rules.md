# Combine Two Rules

Combine Two Rules

## Usage

``` r
combine_rules(x, y, ...)
```

## Arguments

- x:

  (`rule`) to modify.

- y:

  (`rule`) rule whose mapping will take precedence over the ones
  described in `x`.

- ...:

  not used.

## Value

a `rule`.

## Note

The order of the mappings in the resulting rule corresponds to the order
of the mappings in `x` followed by the mappings that are only present in
`y`.

## Examples

``` r
r1 <- rule(
  "first" = c("from ori rule", "FROM ORI RULE"),
  "last" = c(NA, "last"),
  .to_NA = "X",
  .drop = TRUE
)
r2 <- rule(
  "first" = c("F", "f"),
  "second" = c("S", "s"),
  "third" = c("T", "t"),
  .to_NA = "something"
)
combine_rules(r1, r2)
#> Mapping of:
#> first  <-  "F", "f" 
#> last  <-  <NA>, "last" 
#> second  <-  "S", "s" 
#> third  <-  "T", "t" 
#> Convert to <NA>: "something" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
```
