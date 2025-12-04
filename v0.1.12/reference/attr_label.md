# Setting the Label Attribute

Setting the Label Attribute

## Usage

``` r
attr_label(var, label)
```

## Arguments

- var:

  (`object`) whose label attribute can be set.

- label:

  (`character`) the label to add.

## Value

`object` with label attribute.

## Examples

``` r
x <- c(1:10)
attr(x, "label")
#> NULL

y <- attr_label(x, "my_label")
attr(y, "label")
#> [1] "my_label"
```
