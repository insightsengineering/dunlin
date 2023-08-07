# dunlin 0.1.4.9000

* `empty_rule` is removed now. `rule()` will create a normal `rule` object.

# dunlin 0.1.4

* `rules` now have attributes controlling their behavior during reformatting.
* `reformat` can override `rule` attribute with the corresponding arguments.
* Add `render_safe` and `add_whisker` functions to replaces placeholders enclosed in curly braces with replacement stored in a dedicated environment.
* `read_rule` function has been moved into the `citril` package.

# dunlin 0.1.3

* Add `log_filter` function for filtering with additional logs.
* Remove the use of `dm`.
* Introduced `ls_explicit_na` to replace `NA` values in an entire `list` of `data.frame`.
* `reformat` re-levels factors to follow the order provided in the rule that is applied and levels declared in the rule are all integrated to the result.

# dunlin 0.1.2

* Defined a Reformatting Map (`rule` object) which specifies the correspondence between the old and the new values in data.
* Enhanced function `dunlin::reformat()` that provides generic supports in `character` and/or `factor` reformatting while keeping the attributes of the original data (e.g. the data type or labels) unchanged.
* Improved performance of package by adding more unit tests.
* Upgraded package dependencies and removed deprecated functions.
* Updated `pkgdown` configuration.

# dunlin 0.1.1

* Initial package release.
