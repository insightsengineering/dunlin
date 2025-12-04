# Render whiskers safely

Render whiskers safely

## Usage

``` r
render_safe(x)
```

## Arguments

- x:

  (`character`) input to be rendered safely.

## Value

`character` with substituted placeholders.

## Note

The strings enclosed in [`{}`](https://rdrr.io/r/base/Paren.html) are
substituted using the key-values pairs set with `add_whiskers`.

## Examples

``` r
render_safe("Name of {Patient_label}")
#> [1] "Name of Patients"
```
