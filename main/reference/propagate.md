# Propagate Column

`propagate`copy columns from a given table of a `list` of `data.frame`
to all tables based on other common columns. If several rows are
associated with the same key, the rows will be duplicated in the
receiving tables. In safe mode, the key must be unique in the original
table.

## Usage

``` r
propagate(db, from, add, by, safe = TRUE)

# S3 method for class 'list'
propagate(db, from, add, by, safe = TRUE)
```

## Arguments

- db:

  (`list` of `data.frame`) object for which some variable need to be
  propagated.

- from:

  (`string`) the name of the table where the variables to propagate are
  stored.

- add:

  (`character`) the names of the variables to propagate.

- by:

  (`character`) the key binding the `from` table to the other tables.

- safe:

  (`flag`) should the key be checked for uniqueness in the `from` table.

## Value

updated `list` of `data.frame`.

## Examples

``` r
df1 <- data.frame(
  id1 = c("a", "a", "c", "d", "e", "f"),
  id2 = c("A", "B", "A", "A", "A", "A"),
  int = c(1, 2, 3, 4, 5, 6),
  bool = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
)

df2 <- data.frame(
  id1 = c("a", "a", "d", "e", "f", "g"),
  id2 = c("A", "B", "A", "A", "A", "A")
)

df3 <- data.frame(
  id1 = c("a", "c", "d", "e", "f", "x"),
  id2 = c("A", "A", "A", "A", "B", "A"),
  int = c(11, 22, 33, 44, 55, 66)
)

db <- list(df1 = df1, fd2 = df2, df3 = df3)
propagate(db, from = "df1", add = c("int", "bool"), by = c("id1", "id2"))
#> 
#> Updating: fd2 with: int, bool
#> Updating: df3 with: bool
#> $df1
#>   id1 id2 int  bool
#> 1   a   A   1  TRUE
#> 2   a   B   2 FALSE
#> 3   c   A   3  TRUE
#> 4   d   A   4 FALSE
#> 5   e   A   5  TRUE
#> 6   f   A   6 FALSE
#> 
#> $fd2
#>   id1 id2 int  bool
#> 1   a   A   1  TRUE
#> 2   a   B   2 FALSE
#> 3   d   A   4 FALSE
#> 4   e   A   5  TRUE
#> 5   f   A   6 FALSE
#> 6   g   A  NA    NA
#> 
#> $df3
#>   id1 id2 int  bool
#> 1   a   A  11  TRUE
#> 2   c   A  22  TRUE
#> 3   d   A  33 FALSE
#> 4   e   A  44  TRUE
#> 5   f   B  55    NA
#> 6   x   A  66    NA
#> 
```
