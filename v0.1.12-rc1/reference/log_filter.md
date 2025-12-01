# Filter Data with Log

Filter Data with Log

## Usage

``` r
log_filter(data, condition, ...)

# S3 method for class 'data.frame'
log_filter(data, condition, suffix = NULL, ...)

# S3 method for class 'list'
log_filter(
  data,
  condition,
  table,
  by = c("USUBJID", "STUDYID"),
  suffix = NULL,
  verbose = FALSE,
  mode = ifelse(table == "adsl", "all", "unique"),
  ...
)
```

## Arguments

- data:

  (`data.frame`) input data to subset, or named (`list` of
  `data.frame`).

- condition:

  (`call`) of subset condition. Must evaluate as logical.

- ...:

  further arguments to be passed to or from other methods.

- suffix:

  (`string`) optional argument describing the filter.

- table:

  (`string`) table name.

- by:

  (`character`) variable names shared by `table` and other datasets for
  filtering when `mode == "all"`.

- verbose:

  (`flag`) whether to print a report about the filtering.

- mode:

  (`string`) one of `all` or `unique` whether the other tables should be
  filtered based on the rows retained in `table`. Default value is
  `"all"` is `table == "adsl"` and `"unique"` otherwise.

## Value

a `data.frame` or `list` of `data.frame` filtered for the provided
conditions.

## Details

`log_filter` will filter the `data.frame` /named list of `data.frame`
according to the `condition`. All the variables in `condition` must
exist in the data (as variables) or in the parent frame(e.g., in global
environment). For a named list of `data.frame`, set
``` mode = "all"`` to filter other tables by keys retained in table (using by), or  ```mode
= "unique"\` to leave other tables unchanged.

## Examples

``` r
data <- iris
attr(data$Sepal.Length, "label") <- "cm"
log_filter(data, Sepal.Length >= 7)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1           7.0         3.2          4.7         1.4 versicolor
#> 2           7.1         3.0          5.9         2.1  virginica
#> 3           7.6         3.0          6.6         2.1  virginica
#> 4           7.3         2.9          6.3         1.8  virginica
#> 5           7.2         3.6          6.1         2.5  virginica
#> 6           7.7         3.8          6.7         2.2  virginica
#> 7           7.7         2.6          6.9         2.3  virginica
#> 8           7.7         2.8          6.7         2.0  virginica
#> 9           7.2         3.2          6.0         1.8  virginica
#> 10          7.2         3.0          5.8         1.6  virginica
#> 11          7.4         2.8          6.1         1.9  virginica
#> 12          7.9         3.8          6.4         2.0  virginica
#> 13          7.7         3.0          6.1         2.3  virginica

log_filter(list(iris = iris), Sepal.Length >= 7, "iris", character(0))
#> $iris
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1           7.0         3.2          4.7         1.4 versicolor
#> 2           7.1         3.0          5.9         2.1  virginica
#> 3           7.6         3.0          6.6         2.1  virginica
#> 4           7.3         2.9          6.3         1.8  virginica
#> 5           7.2         3.6          6.1         2.5  virginica
#> 6           7.7         3.8          6.7         2.2  virginica
#> 7           7.7         2.6          6.9         2.3  virginica
#> 8           7.7         2.8          6.7         2.0  virginica
#> 9           7.2         3.2          6.0         1.8  virginica
#> 10          7.2         3.0          5.8         1.6  virginica
#> 11          7.4         2.8          6.1         1.9  virginica
#> 12          7.9         3.8          6.4         2.0  virginica
#> 13          7.7         3.0          6.1         2.3  virginica
#> 
```
