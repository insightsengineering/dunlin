#' Filter Data with Log
#' @param data (`data.frame`) input data to subset, or named (`list`) of (`data.frame`).
#' @param table (`string`) table name.
#' @param condition (`expression`) subset condition. Must eval as logical.
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

#' @export
log_filter.data.frame <- function(data, condition, ...) {
  checkmate::assert_class(condition, "expression")
  vars <- all.vars(condition)
  var_in_env <- vapply(vars, exists, envir = parent.frame(), inherits = TRUE, FUN.VALUE = TRUE)
  var_in_data <- vapply(vars, `%in%`, table = names(data), FUN.VALUE = TRUE)
  if (!all(var_in_env | var_in_data)) {
    stop("All vars in expression must exist in data!")
  }
  res <- do.call(subset, list(x = data, subset = condition))
  attr(res, "rows") <- c(attr(res, "rows"), list(c(nrow(data), nrow(res))))
  res
}

#' @export
log_filter.list <- function(data, table, condition, ...) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_subset(table, names(data))
  checkmate::assert_class(condition, "expression")
  data[[table]] <- log_filter(data[[table]], condition)
  if (identical(table, "adsl")) {
    for (k in setdiff(names(data), "adsl")) {
      if ("USUBJID" %in% names(data[[k]]) && "USUBJID" %in% names(data[["adsl"]])) {
        data[[k]] <- data[[k]][data[[k]]$USUBJID %in% data[["adsl"]]$USUBJID]
      }
    }
  }
  return(data)
}
