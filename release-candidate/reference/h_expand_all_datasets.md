# Propagate the rules for all datasets

Propagate the rules for all datasets

## Usage

``` r
h_expand_all_datasets(format_list, ls_datasets = NULL)
```

## Arguments

- ls_datasets:

  (`character`) the name of all datasets in the object to reformat.

## Value

a nested `list` attributing a rule to be applied to specific variables
of specific datasets.

## Details

the rules described under `all_datasets` are propagated to all data sets
for the corresponding variables except in data sets where a rule is
already attributed to the same variable.
