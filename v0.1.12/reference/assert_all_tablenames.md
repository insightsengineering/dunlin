# Assert that all names are among names of a `list` of `data.frame`.

Assert that all names are among names of a `list` of `data.frame`.

## Usage

``` r
assert_all_tablenames(db, tab, null_ok = TRUE, qualifier = NULL)
```

## Arguments

- db:

  (`list` of `data.frame`) input to check for the presence of tables.

- tab:

  (`character`) the names of the tables to be checked.

- null_ok:

  (`flag`) can `x` be NULL.

- qualifier:

  (`string`) to be returned if the check fails.

## Value

invisible `TRUE` or an error message if the criteria are not fulfilled.

## Examples

``` r
lsd <- list(
  mtcars = mtcars,
  iris = iris
)
assert_all_tablenames(lsd, c("mtcars", "iris"), qualifier = "first test:")
```
