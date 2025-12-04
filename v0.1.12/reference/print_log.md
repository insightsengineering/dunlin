# Print Log

Print Log

## Usage

``` r
print_log(data, incl, incl.adsl)

# S3 method for class 'data.frame'
print_log(data, incl = TRUE, incl.adsl = TRUE)

# S3 method for class 'list'
print_log(data, incl = TRUE, incl.adsl = TRUE)
```

## Arguments

- data:

  (`list` of `data.frame` or `data.frame`) filtered with `log_filter`.

- incl:

  (`flag`) should information about unfiltered `data.frame` be printed.

- incl.adsl:

  (`flag`) should indication of filtering performed through `adsl` be
  printed.

## Value

`NULL`. Print a description of the filtering applied to `data`.

## Examples

``` r
data <- log_filter(iris, Sepal.Length >= 7, "Sep")
print_log(data)
#> Filter Log:
#>   Sep: Sepal.Length >= 7 [150 --> 13 rows.]
data <- log_filter(
  list(
    adsl = iris,
    iris2 = iris,
    mtcars = mtcars,
    iris3 = iris
  ),
  Sepal.Length >= 7,
  "adsl",
  character(0),
  "adsl filter"
)
data <- log_filter(data, Sepal.Length >= 7, "iris2", character(0), "iris2 filter")
print_log(data)
#> Filter Log:
#>   - adsl:
#>   adsl filter: Sepal.Length >= 7 [150 --> 13 rows.]
#>   - iris2:
#>   adsl filter: Filtered by adsl: Sepal.Length >= 7 [150 --> 13 rows.] 
#>   iris2 filter: Sepal.Length >= 7 [13 --> 13 rows.]
#>   - mtcars:
#>   No filtering [32 rows.]
#>   - iris3:
#>   adsl filter: Filtered by adsl: Sepal.Length >= 7 [150 --> 13 rows.]
print_log(data, incl = FALSE)
#> Filter Log:
#>   - adsl:
#>   adsl filter: Sepal.Length >= 7 [150 --> 13 rows.]
#>   - iris2:
#>   adsl filter: Filtered by adsl: Sepal.Length >= 7 [150 --> 13 rows.] 
#>   iris2 filter: Sepal.Length >= 7 [13 --> 13 rows.]
#>   - iris3:
#>   adsl filter: Filtered by adsl: Sepal.Length >= 7 [150 --> 13 rows.]
print_log(data, incl.adsl = FALSE, incl = FALSE)
#> Filter Log:
#>   - adsl:
#>   adsl filter: Sepal.Length >= 7 [150 --> 13 rows.]
#>   - iris2:
#>   iris2 filter: Sepal.Length >= 7 [13 --> 13 rows.]
```
