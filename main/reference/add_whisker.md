# Add whisker values

Add whisker values

## Usage

``` r
add_whisker(x)
```

## Arguments

- x:

  Named (`character`) input.

## Value

invisible `NULL`. Assign the key-value pair provided as argument in the
whisker environment.

## Details

The names of the character gives the string to be replaced and the value
gives the new string.

## Examples

``` r
my_whiskers <- c(Placeholder = "Replacement", Placeholder2 = "Replacement2")
add_whisker(my_whiskers)
```
