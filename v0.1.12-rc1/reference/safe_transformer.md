# Safe transformer

Safe transformer

## Usage

``` r
safe_transformer(text, envir)
```

## Arguments

- text:

  (`string`) to be substituted.

- envir:

  (`environment`) containing key-value pairs describing the substitution
  to perform.

## Value

`string` with substituted placeholders.

## Details

Obtain content in global environment by default. If not found, use the
environment here. The function first looks for an exact match. If not
found, it searches for a match in lower case then apply to the result
the same case as the original value.
