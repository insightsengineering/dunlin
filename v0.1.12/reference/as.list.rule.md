# Convert Rule to List

Convert Rule to List

## Usage

``` r
# S3 method for class 'rule'
as.list(x, ...)
```

## Arguments

- x:

  (`rule`) to convert.

- ...:

  not used.

## Value

an object of class `list`.

## Examples

``` r
x <- rule("a" = c("a", "b"), "X" = "x", .to_NA = c("v", "w"))
as.list(x)
#> $a
#> [1] "a" "b"
#> 
#> $X
#> [1] "x"
#> 
#> $.string_as_fct
#> [1] TRUE
#> 
#> $.na_last
#> [1] TRUE
#> 
#> $.drop
#> [1] FALSE
#> 
#> $.to_NA
#> [1] "v" "w"
#> 
```
