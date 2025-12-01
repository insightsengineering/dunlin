# Reformat Values

Replaces values in `vectors` or `list` of `data.frame` using
used-defined
[`rule`](https://insightsengineering.github.io/dunlin/reference/rule.md)
or list of
[`rule`](https://insightsengineering.github.io/dunlin/reference/rule.md).
See
[`vignette("Reformatting", package = "dunlin")`](https://insightsengineering.github.io/dunlin/articles/Reformatting.md)
for a detailed guide on using this function.

## Usage

``` r
reformat(obj, ...)

# Default S3 method
reformat(obj, format, ...)

# S3 method for class 'character'
reformat(obj, format, ..., verbose = FALSE)

# S3 method for class 'factor'
reformat(obj, format, ..., verbose = FALSE)

# S3 method for class 'list'
reformat(
  obj,
  format,
  ...,
  verbose = get_arg("dunlin.reformat.verbose", "R_DUNLIN_REFORMAT_VERBOSE", FALSE)
)
```

## Arguments

- obj:

  (`character`, `factor` or `list of data.frame`) to reformat.

- ...:

  for compatibility between methods and pass additional special mapping
  to transform rules.

  - `.string_as_fct` (`flag`) whether the reformatted character object
    should be converted to factor.

  - `.to_NA` (`character`) values that should be converted to `NA`. For
    `factor`, the corresponding levels are dropped. If `NULL`, the
    argument will be taken from the `to_NA`attribute of the rule.

  - `.drop` (`flag`) whether to drop empty levels. If `NULL`, the
    argument will be taken from the `drop`attribute of the rule.

  - `.na_last` (`flag`) whether the level replacing `NA` should be last.

- format:

  (`rule`) or (`list`) of `rule` depending on the class of obj.

- verbose:

  (`flag`) whether to print the format.

## Value

(`character`, `factor` or `list of data.frame`) with remapped values.

## Note

When the rule is empty rule or when values subject to reformatting are
absent from the object, no error is raised. The conversion to factor if
`.string_as_fct = TRUE`) is still carried out. The conversion of the
levels declared in `.to_NA` to `NA` values occurs after the remapping.
`NA` values created this way are not affected by a rule declaring a
remapping of `NA` values. For factors, level dropping is the last step,
hence, levels converted to `NA` by the `.to_NA` argument, will be
removed if `.drop` is `TRUE`. Arguments passed via `reformat` override
the ones defined during rule creation.

the variables listed under the `all_dataset` keyword will be reformatted
with the corresponding rule in every data set except where another rule
is specified for the same variable under a specific data set name.

## Examples

``` r
# Reformatting of character.
obj <- c("a", "b", "x", NA, "")
attr(obj, "label") <- "my label"
format <- rule("A" = "a", "NN" = NA)

reformat(obj, format)
#> [1] A    b    x    NN   <NA>
#> attr(,"label")
#> [1] my label
#> Levels: A b x NN
reformat(obj, format, .string_as_fct = FALSE, .to_NA = NULL)
#> [1] "A"  "b"  "x"  "NN" ""  
#> attr(,"label")
#> [1] "my label"

# Reformatting of factor.
obj <- factor(c("first", "a", "aa", "b", "x", NA), levels = c("first", "x", "b", "aa", "a", "z"))
attr(obj, "label") <- "my label"
format <- rule("A" = c("a", "aa"), "NN" = c(NA, "x"), "Not_present" = "z", "Not_a_level" = "P")

reformat(obj, format)
#> [1] first A     A     b     NN    NN   
#> attr(,"label")
#> [1] my label
#> Levels: A Not_present Not_a_level first b NN
reformat(obj, format, .na_last = FALSE, .to_NA = "b", .drop = FALSE)
#> [1] first A     A     <NA>  NN    NN   
#> attr(,"label")
#> [1] my label
#> Levels: A NN Not_present Not_a_level first

# Reformatting of list of data.frame.
df1 <- data.frame(
  var1 = c("a", "b", NA),
  var2 = factor(c("F1", "F2", NA))
)

df2 <- data.frame(
  var1 = c("x", NA, "y"),
  var2 = factor(c("F11", NA, "F22"))
)

db <- list(df1 = df1, df2 = df2)

format <- list(
  df1 = list(
    var1 = rule("X" = "x", "N" = NA, .to_NA = "b")
  ),
  df2 = list(
    var2 = rule("f11" = "F11", "NN" = NA)
  ),
  df_absent = list(
    var1 = rule("NO" = "no")
  ),
  all_datasets = list(
    var1 = rule("xx" = "x", "aa" = "a")
  )
)

reformat(db, format)
#> $df1
#>   var1 var2
#> 1    a   F1
#> 2 <NA>   F2
#> 3    N <NA>
#> 
#> $df2
#>   var1 var2
#> 1   xx  f11
#> 2 <NA>   NN
#> 3    y  F22
#> 
```
