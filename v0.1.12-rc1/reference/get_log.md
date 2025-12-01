# Get Log

Get Log

## Usage

``` r
get_log(data, incl, incl.adsl)

# S3 method for class 'data.frame'
get_log(data, incl = TRUE, incl.adsl = TRUE)

# S3 method for class 'list'
get_log(data, incl = TRUE, incl.adsl = TRUE)
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

`character` or `list of character` describing the filtering applied to
`data`.

## Examples

``` r
data <- log_filter(iris, Sepal.Length >= 7, "xx")
data <- log_filter(data, Sepal.Length < 2)
data <- log_filter(data, Sepal.Length >= 2, "yy")
get_log(data)

data <- log_filter(
  list(iris1 = iris, iris2 = iris),
  Sepal.Length >= 7,
  "iris1",
  character(0),
  "Sep"
)
get_log(data)
#> $iris1
#> [1] "Sep: Sepal.Length >= 7 [150 --> 13 rows.]"
#> 
#> $iris2
#> [1] "No filtering [150 rows.]"
#> 
```
