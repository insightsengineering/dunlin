# Encode Categorical Missing Values in a `list` of `data.frame`

Encode Categorical Missing Values in a `list` of `data.frame`

## Usage

``` r
ls_explicit_na(
  data,
  omit_tables = NULL,
  omit_columns = NULL,
  char_as_factor = TRUE,
  na_level = "<Missing>"
)
```

## Arguments

- data:

  (`list` of `data.frame`) to be transformed.

- omit_tables:

  (`character`) the names of the tables to omit from processing.

- omit_columns:

  (`character`) the names of the columns to omit from processing.

- char_as_factor:

  (`logical`) should character columns be converted into factor.

- na_level:

  (`string`) the label to encode missing levels.

## Value

`list` of `data.frame` object with explicit missing levels.

## Details

This is a helper function to encode missing values (i.e `NA` and
`empty string`) of every `character` and `factor` variable found in a
`list` of `data.frame`. The `label` attribute of the columns is
preserved.

## Examples

``` r
df1 <- data.frame(
  "char" = c("a", "b", NA, "a", "k", "x"),
  "char2" = c("A", "B", NA, "A", "K", "X"),
  "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
  "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
)
df2 <- data.frame(
  "char" = c("a", "b", NA, "a", "k", "x"),
  "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
  "num" = c(1:5, NA)
)
df3 <- data.frame(
  "char" = c(NA, NA, "A")
)

db <- list(df1 = df1, df2 = df2, df3 = df3)

ls_explicit_na(db)
#> $df1
#>        char     char2      fact  logi
#> 1         a         A        f1    NA
#> 2         b         B        f2 FALSE
#> 3 <Missing> <Missing> <Missing>  TRUE
#> 4         a         A <Missing>    NA
#> 5         k         K        f1 FALSE
#> 6         x         X        f1    NA
#> 
#> $df2
#>        char      fact num
#> 1         a        f1   1
#> 2         b        f2   2
#> 3 <Missing> <Missing>   3
#> 4         a <Missing>   4
#> 5         k        f1   5
#> 6         x        f1  NA
#> 
#> $df3
#>        char
#> 1 <Missing>
#> 2 <Missing>
#> 3         A
#> 
ls_explicit_na(db, omit_tables = "df3", omit_columns = "char2")
#> $df1
#>        char char2      fact  logi
#> 1         a     A        f1    NA
#> 2         b     B        f2 FALSE
#> 3 <Missing>  <NA> <Missing>  TRUE
#> 4         a     A <Missing>    NA
#> 5         k     K        f1 FALSE
#> 6         x     X        f1    NA
#> 
#> $df2
#>        char      fact num
#> 1         a        f1   1
#> 2         b        f2   2
#> 3 <Missing> <Missing>   3
#> 4         a <Missing>   4
#> 5         k        f1   5
#> 6         x        f1  NA
#> 
#> $df3
#>   char
#> 1 <NA>
#> 2 <NA>
#> 3    A
#> 
```
