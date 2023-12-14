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
