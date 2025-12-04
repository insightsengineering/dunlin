# Combine Rules Found in Lists of Rules.

Combine Rules Found in Lists of Rules.

## Usage

``` r
combine_list_rules(x, val, ...)
```

## Arguments

- x:

  (`list`) of `rule` objects.

- val:

  (`list`) of `rule` objects.

- ...:

  passed to
  [`combine_rules`](https://insightsengineering.github.io/dunlin/reference/combine_rules.md).

## Value

a `list` of `rule` objects.

## Examples

``` r
l1 <- list(
  r1 = rule(
    "first" = c("overwritten", "OVERWRITTEN"),
    "almost first" = c(NA, "almost")
  ),
  r2 = rule(
    ANYTHING = "anything"
  )
)

l2 <- list(
  r1 = rule(
    "first" = c("F", "f"),
    "second" = c("S", "s"),
    "third" = c("T", "t"),
    .to_NA = "something"
  ),
  r3 = rule(
    SOMETHING = "something"
  )
)

combine_list_rules(l1, l2)
#> $r1
#> Mapping of:
#> first  <-  "F", "f" 
#> almost first  <-  <NA>, "almost" 
#> second  <-  "S", "s" 
#> third  <-  "T", "t" 
#> Convert to <NA>: "something" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
#> 
#> $r2
#> Mapping of:
#> ANYTHING  <-  "anything" 
#> Convert to <NA>: "" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
#> 
#> $r3
#> Mapping of:
#> SOMETHING  <-  "something" 
#> Convert to <NA>: "" 
#> Convert to factor: TRUE 
#> Drop unused level: FALSE 
#> NA-replacing level in last position: TRUE 
#> 
```
