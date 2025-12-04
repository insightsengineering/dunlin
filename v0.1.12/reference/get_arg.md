# Getting Argument From System, Option or Default

Getting Argument From System, Option or Default

## Usage

``` r
get_arg(opt = NULL, sys = NULL, default = NULL, split = ";")
```

## Arguments

- opt:

  (`string`) the name of an option.

- sys:

  (`string`) the name of an environment variable.

- default:

  value to return if neither the environment variable nor the option are
  set.

- split:

  (`string`) the pattern used to split the values obtained using
  environment variable.

## Value

if defined, the value of the option (`opt`), a `character` from the
environment variable (`sys`) or the `default` in this order of priority.

## Examples

``` r
get_arg("my.option", "MY_ARG", "default")
#> [1] "default"
withr::with_envvar(c(MY_ARG = "x;y"), get_arg("my.option", "MY_ARG", "default"))
#> [1] "x" "y"
withr::with_options(c(my.option = "y"), get_arg("my.option", "MY_ARG", "default"))
#> [1] "y"
```
