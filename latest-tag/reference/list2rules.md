# Convert nested list into list of `rule`

Convert nested list into list of `rule`

## Usage

``` r
list2rules(obj)
```

## Arguments

- obj:

  (`nested list`) to convert into list of rules.

## Value

a `list` of `rule` objects.

## Examples

``` r
obj <- list(
  rule1 = list("X" = c("a", "b"), "Z" = "c", .to_NA = "xxxx"),
  rule2 = list(Missing = c(NA, "")),
  rule3 = list(Missing = c(NA, ""), .drop = TRUE),
  rule4 = list(Absent = c(NA, ""), .drop = TRUE, .to_NA = "yyyy")
)
list2rules(obj)
#> $rule1
#> Mapping of:
#> X  <-  "a", "b" 
#> Z  <-  "c" 
#> Convert to <NA>: "xxxx" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
#> 
#> $rule2
#> Mapping of:
#> Missing  <-  <NA>, "" 
#> Convert to <NA>: "" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
#> 
#> $rule3
#> Mapping of:
#> Missing  <-  <NA>, "" 
#> Convert to <NA>: "" 
#> Convert to factor: TRUE 
#> Drop unused level: TRUE 
#> NA-replacing level in last position: TRUE 
#> 
#> $rule4
#> Mapping of:
#> Absent  <-  <NA>, "" 
#> Convert to <NA>: "yyyy" 
#> Convert to factor: TRUE 
#> Drop unused level: TRUE 
#> NA-replacing level in last position: TRUE 
#> 
```
