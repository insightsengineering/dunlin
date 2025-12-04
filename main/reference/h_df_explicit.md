# Encode Categorical Missing Values in a `data.frame`.

Encode Categorical Missing Values in a `data.frame`.

## Usage

``` r
h_df_explicit(
  df,
  omit_columns = NULL,
  char_as_factor = TRUE,
  na_level = "<Missing>"
)
```

## Arguments

- omit_columns:

  (`character`) the names of the columns to omit from processing.

- char_as_factor:

  (`logical`) should character columns be converted into factor.

- na_level:

  (`string`) the label to encode missing levels.

## Value

a `data.frame` object with explicit missing levels.
