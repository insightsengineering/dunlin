#' Filter Data with Log
#' @param data (`data.frame`) input data to subset, or named (`list`) of (`data.frame`).
#' @param condition (`call`) of subset condition. Must eval as logical.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @details
#' `log_filter` will filter the data/named list of data according to the `condition`.
#' All the variables in `condition` must exist in the data (as variables) or in the parent
#' frame(e.g., in global environment).
#' For named list of data, if `adsl` is available, `log_filter` will also try to subset all
#' other datasets with `USUBJID`.
#' @export
log_filter <- function(data, condition, ...) {
  UseMethod("log_filter")
}

#' @rdname log_filter
#' @export
#' @examples
#' log_filter(iris, Sepal.Length >= 7)
log_filter.data.frame <- function(data, condition, ...) {
  condition <- match.call()$condition
  vars <- all.vars(condition)
  var_in_env <- vapply(vars, exists, envir = parent.frame(), inherits = TRUE, FUN.VALUE = TRUE)
  var_in_data <- vapply(vars, `%in%`, table = names(data), FUN.VALUE = TRUE)
  if (!all(var_in_env | var_in_data)) {
    stop(sprintf("Variable %s not found in data or environment.", toString(vars[!(var_in_data | var_in_env)])))
  }
  res <- do.call(subset, list(x = data, subset = condition), envir = parent.frame())
  rows <- list(c(nrow(data), nrow(res)))
  names(rows) <- deparse(condition)
  attr(res, "rows") <- c(attr(data, "rows"), rows)
  res
}

#' @rdname log_filter
#' @param table (`string`) table name.
#' @param by (`character`) variable names shared by adsl and other datasets for filtering.
#' @export
#' @examples
#' log_filter(list(iris = iris), Sepal.Length >= 7, "iris", character(0))
log_filter.list <- function(data, condition, table, by = c("USUBJID", "STUDYID"), ...) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_subset(table, names(data))
  checkmate::assert_names(colnames(data[[table]]), must.include = by)
  condition <- match.call()$condition
  data[[table]] <- eval(bquote(log_filter(data[[table]], .(condition))))
  if (identical(table, "adsl")) {
    for (k in setdiff(names(data), "adsl")) {
      if (all(by %in% names(data[[k]]))) {
        data[[k]] <- merge(data[[k]], data$adsl[by], by = by, all.x = FALSE, all.y = FALSE, sort = FALSE)
      }
    }
  }
  return(data)
}
