#' Filter Data with Log
#' @param data (`data.frame`) input data to subset, or named (`list` of `data.frame`).
#' @param condition (`call`) of subset condition. Must evaluate as logical.
#' @param suffix (`string`) optional argument describing the filter.
#' @param ... further arguments to be passed to or from other methods.
#' @returns a `data.frame` or `list` of `data.frame` filtered for the provided conditions.
#' @details
#' `log_filter` will filter the data/named list of data according to the `condition`.
#' All the variables in `condition` must exist in the data (as variables) or in the parent
#' frame(e.g., in global environment).
#' For named list of data, if `ADSL` is available, `log_filter` will also try to subset all
#' other datasets with `USUBJID`.
#' @export
log_filter <- function(data, condition, ...) {
  UseMethod("log_filter")
}

#' @rdname log_filter
#' @export
#' @examples
#' data <- iris
#' attr(data$Sepal.Length, "label") <- "cm"
#' log_filter(data, Sepal.Length >= 7)
#'
log_filter.data.frame <- function(data, condition, suffix = NULL, ...) {
  checkmate::assert_string(suffix, null.ok = TRUE)

  condition <- match.call()$condition
  vars <- all.vars(condition)
  var_in_env <- vapply(vars, exists, envir = parent.frame(), inherits = TRUE, FUN.VALUE = TRUE)
  var_in_data <- vapply(vars, `%in%`, table = names(data), FUN.VALUE = TRUE)
  if (!all(var_in_env | var_in_data)) {
    rlang::abort(sprintf("Variable %s not found in data or environment.", toString(vars[!(var_in_data | var_in_env)])))
  }
  res <- eval(bquote(dplyr::filter(data, .(condition))))
  rows <- list(list(init = nrow(data), final = nrow(res), suffix = suffix))
  rlbl <- paste0(deparse(condition), collapse = "")
  rlbl <- stringr::str_replace_all(rlbl, "\\s+", " ")
  names(rows) <- rlbl
  attr(res, "rows") <- c(attr(data, "rows"), rows)

  res
}

#' @rdname log_filter
#' @param table (`character`) table name.
#' @param by (`character`) variable names shared by `adsl` and other data sets for filtering.
#' @export
#' @examples
#' log_filter(list(iris = iris, iris2 = iris), Sepal.Length >= 7, table = c("iris", "iris2"), character(0))
log_filter.list <- function(data, condition, table, by = c("USUBJID", "STUDYID"), suffix = NULL, ...) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_string(suffix, null.ok = TRUE)
  checkmate::assert_character(table)
  assert_all_tablenames(data, table)
  condition <- match.call()$condition

  for (t in table) {
    checkmate::assert_names(colnames(data[[t]]), must.include = by, .var.name = paste("table:", t), what = "colname")
    data[[t]] <- eval(bquote(log_filter(data[[t]], .(condition), .(suffix))))

    if (identical(t, "adsl")) {
      for (k in setdiff(names(data), "adsl")) {
        if (all(by %in% names(data[[k]]))) {
          if (length(by) == 0) by <- intersect(names(data[[k]]), names(data$adsl))

          ori_n <- nrow(data[[k]])
          ori_att <- attr(data[[k]], "rows")

          data[[k]] <- dplyr::semi_join(data[[k]], data$adsl, by = by)

          rows <- list(list(init = ori_n, final = nrow(data[[k]]), suffix = suffix))
          names(rows) <- paste0("Filtered by adsl: ", deparse(condition), collapse = "")
          attr(data[[k]], "rows") <- c(ori_att, rows)
        }
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
#' @param incl.adsl (`flag`) should indication  of filtering performed through `adsl` be printed.
#' @returns `character` or `list of character` describing the filtering applied to `data`.
#'
#' @export
get_log <- function(data, incl, incl.adsl) {
  UseMethod("get_log")
}

#' @rdname get_log
#' @export
#' @examples
#' data <- log_filter(iris, Sepal.Length >= 7, "xx")
#' data <- log_filter(data, Sepal.Length < 2)
#' data <- log_filter(data, Sepal.Length >= 2, "yy")
#' get_log(data)
#'
get_log.data.frame <- function(data, incl = TRUE, incl.adsl = TRUE) {
  checkmate::assert_flag(incl)

  att <- attr(data, "rows")
  if (!incl.adsl) {
    sel <- grepl("Filtered by adsl", names(att))
    att <- att[!sel]
  }

  if (length(att) != 0L) {
    start_row <- lapply(att, "[[", "init")
    end_row <- lapply(att, "[[", "final")
    suffix <- lapply(att, "[[", "suffix")
    suffix <- vapply(suffix, function(x) ifelse(is.null(x), "", paste0(x, ": ")), character(1))
    res <- paste0(suffix, names(att), " [", start_row, " --> ", end_row, " rows.]")
  } else if (incl) {
    paste0("No filtering [", nrow(data), " rows.]")
  } else {
    NULL
  }
}


#' @rdname get_log
#' @export
#' @examples
#' data <- log_filter(
#'   list(iris1 = iris, iris2 = iris),
#'   Sepal.Length >= 7,
#'   "iris1",
#'   character(0),
#'   "Sep"
#' )
#' get_log(data)
#'
get_log.list <- function(data, incl = TRUE, incl.adsl = TRUE) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_flag(incl)

  lapply(data, get_log, incl = incl, incl.adsl = incl.adsl)
}

# Print Log ----

#' Print Log
#'
#' @inheritParams get_log
#' @returns `NULL`. Print a description of the filtering applied to `data`.
#' @export
#'
print_log <- function(data, incl, incl.adsl) {
  UseMethod("print_log")
}

#' @rdname print_log
#' @export
#' @examples
#' data <- log_filter(iris, Sepal.Length >= 7, "Sep")
#' print_log(data)
print_log.data.frame <- function(data, incl = TRUE, incl.adsl = TRUE) {
  checkmate::assert_flag(incl)

  cat("Filter Log:")
  cat(paste0("\n  ", get_log(data, incl = incl, incl.adsl = incl.adsl)))
  cat("\n")
  invisible()
}

#' @rdname print_log
#' @export
#' @examples
#' data <- log_filter(
#'   list(
#'     adsl = iris,
#'     iris2 = iris,
#'     mtcars = mtcars,
#'     iris3 = iris
#'   ),
#'   Sepal.Length >= 7,
#'   "adsl",
#'   character(0),
#'   "adsl filter"
#' )
#' data <- log_filter(data, Sepal.Length >= 7, "iris2", character(0), "iris2 filter")
#' print_log(data)
#' print_log(data, incl = FALSE)
#' print_log(data, incl.adsl = FALSE, incl = FALSE)
print_log.list <- function(data, incl = TRUE, incl.adsl = TRUE) {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_flag(incl)

  filter_log <- get_log(data, incl = incl, incl.adsl = incl.adsl)

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
  cat("\n")

  invisible()
}
