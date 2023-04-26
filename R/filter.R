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
#' @param by (`character`) variable names shared by `adsl` and other datasets for filtering.
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
        if (length(by) == 0) by <- intersect(names(data[[k]]), names(data$adsl))

        ori_n <- nrow(data[[k]])
        ori_att <- attr(data[[k]], "rows")

        data[[k]] <- merge(data[[k]], data$adsl[by], by = by, all.x = FALSE, all.y = FALSE, sort = FALSE)

        rows <- list(c(ori_n, nrow(data[[k]])))
        names(rows) <- paste0("Filtered by adsl: ", deparse(condition), collapse = "")
        attr(data[[k]], "rows") <- c(ori_att, rows)
      }
    }
  }
  return(data)
}

# Get Log ----

#' Get Log
#'
#' @param data (`list` of `data.frame` or `data.frame`) filtered with `log_filter`.
#' @param incl (`flag`) should information about unfiltered `data.frame` be printed.
#'
#' @export
get_log <- function(data, incl) {
  UseMethod("get_log")
}

#' @rdname get_log
#' @export
#' @examples
#' data <- log_filter(iris, Sepal.Length >= 7)
#' get_log(data)
#'
get_log.data.frame <- function(data, incl = TRUE) {
  checkmate::assert_flag(incl)

  att <- attr(data, "rows")

  if (!is.null(att)) {
    start_row <- lapply(att, "[[", 1)
    end_row <- lapply(att, "[[", 2)
    paste0(names(att), " [", start_row, " --> ", end_row, " rows.]")
  } else if (incl) {
    paste0("No filtering [", nrow(data), " rows.]")
  } else {
    NULL
  }
}


#' @rdname get_log
#' @export
#' @examples
#' data <- log_filter(list(iris1 = iris, iris2 = iris), Sepal.Length >= 7, "iris1", character(0))
#' get_log(data)
#'
get_log.list <- function(data, incl = TRUE) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_flag(incl)

  lapply(data, get_log, incl = incl)
}

# Print Log ----

#' Print Log
#'
#' @inheritParams get_log
#'
#' @export
print_log <- function(data, incl) {
  UseMethod("print_log")
}

#' @rdname print_log
#' @export
#' @examples
#' data <- log_filter(iris, Sepal.Length >= 7)
#' print_log(data)
print_log.data.frame <- function(data, incl = TRUE) {
  checkmate::assert_flag(incl)

  cat("Filter Log:")
  cat(paste0("\n  ", get_log(data, incl = incl)))

  invisible()
}

#' @rdname print_log
#' @export
#' @examples
#' data <- log_filter(list(adsl = iris, iris2 = iris), Sepal.Length >= 7, "adsl", character(0))
#' print_log(data)
print_log.list <- function(data, incl = TRUE) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_flag(incl)

  filter_log <- get_log(data, incl = incl)

  if (!incl) {
    filter_log <- filter_log[!vapply(filter_log, is.null, logical(1))]
  }

  cat("Filter Log:")
  if (length(filter_log) == 0) {
    cat("\n  No filtering")
  } else {
    mapply(
      function(x, y) {
        cat(paste0("\n  - ", x, ":"))
        cat(paste0("\n  ", y, ""))
      },
      as.list(names(filter_log)),
      filter_log
    )
  }

  invisible()
}
