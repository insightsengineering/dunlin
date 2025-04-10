#' Setting the Label Attribute
#'
#' @param var (`object`) whose label attribute can be set.
#' @param label (`character`) the label to add.
#' @returns `object` with label attribute.
#'
#' @export
#' @examples
#' x <- c(1:10)
#' attr(x, "label")
#'
#' y <- attr_label(x, "my_label")
#' attr(y, "label")
attr_label <- function(var, label) {
  checkmate::assert_character(label)

  x <- var
  attr(x, "label") <- label

  x
}

#' Setting the Label Attribute to Data Frame Columns
#'
#' @param df (`data.frame`).
#' @param label (`character`) the labels to add.
#' @returns `data.frame` with label attributes.
#'
#' @export
#' @examples
#' res <- attr_label_df(mtcars, letters[1:11])
#' res
#' lapply(res, attr, "label")
attr_label_df <- function(df, label) {
  checkmate::assert_data_frame(df)
  checkmate::assert_character(label, len = ncol(df))

  res <- mapply(attr_label, var = df, label = as.list(label), SIMPLIFY = FALSE)
  as.data.frame(res)
}

#' Getting Argument From System, Option or Default
#'
#' @param opt (`string`) the name of an option.
#' @param sys (`string`) the name of an environment variable.
#' @param default value to return if neither the environment variable nor the option are set.
#' @param split (`string`) the pattern used to split the values obtained using environment variable.
#'
#' @returns if defined, the value of the option (`opt`), a `character` from the environment variable (`sys`) or the
#'   `default` in this order of priority.
#'
#' @export
#' @examplesIf require("withr")
#' get_arg("my.option", "MY_ARG", "default")
#' withr::with_envvar(c(MY_ARG = "x;y"), get_arg("my.option", "MY_ARG", "default"))
#' withr::with_options(c(my.option = "y"), get_arg("my.option", "MY_ARG", "default"))
get_arg <- function(opt = NULL, sys = NULL, default = NULL, split = ";") {
  checkmate::assert_string(sys, null.ok = TRUE)
  checkmate::assert_string(opt, null.ok = TRUE)
  checkmate::assert_string(split)

  if (!is.null(opt)) {
    val <- getOption(opt, default = "")
    if (!identical(val, "")) {
      return(val)
    }
  }

  if (!is.null(sys)) {
    val <- Sys.getenv(sys, unset = "")
    if (!identical(val, "")) {
      val <- stringr::str_split_1(val, split)
      return(val)
    }
  }

  default
}
