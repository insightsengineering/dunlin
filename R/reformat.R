#' Reformat Values
#' @param obj object to reformat.
#' @param format (`rule`) or (`list`) of `rules` depending on the class of obj.
#' @param na_last (`flag`) whether the level replacing `NA` should be last.
#' @param ... not used. Only for compatibility between methods.
#'
#' @export
#' @details
#' For character values, reformatting will only change those should be changed.
#' NA values are not changed, if the rule does not contain `NA_character_`.
#' Factors works similarly to characters.
#' For `dm` objects, see `reformat` for more details.
#' `empty_rule` will do nothing to the data.
#'
#' @rdname reformat
#'
reformat <- function(obj, format, na_last = FALSE) {
  UseMethod("reformat")
}

#' @export
#' @rdname reformat
reformat.default <- function(obj, format, ...) {
  if (!is(format, "empty_rule")) {
    warning("Not implemented! Only empty rule allowed.")
  }
  return(obj)
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of character.
#' obj <- c("a", "b", "x", NA)
#' attr(obj, "label") <- "my label"
#' format <- rule("A" = "a", "NN" = NA)
#'
#' reformat(obj, format)
reformat.character <- function(obj, format, ...) {
  if (is(format, "empty_rule")) {
    return(obj)
  }
  checkmate::assert_class(format, "rule")
  value_match <- unlist(format)
  m <- match(obj, value_match)
  obj[!is.na(m)] <- names(format)[m[!is.na(m)]]
  return(obj)
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of factor.
#' obj <- factor(c("a", "aa", "b", "x", NA), levels = c("x", "b", "aa", "a", "z"))
#' attr(obj, "label") <- "my label"
#' format <- rule("A" = c("a", "aa"), "NN" = c(NA, "x"), "Not Present" = "z")
#'
#' reformat(obj, format)
#' reformat(obj, format, na_last = TRUE)
reformat.factor <- function(obj, format, na_last = FALSE) {
  if (is(format, "empty_rule")) {
    return(obj)
  }
  checkmate::assert_class(format, "rule")
  if (any(is.na(format))) {
    obj <- forcats::fct_na_value_to_level(obj)
  }

  format <- format[format %in% levels(obj)]

  res <- forcats::fct_recode(obj, !!!format)

  if (any(is.na(format)) && na_last) {
    na_lvl <- names(format)[is.na(format)]
    forcats::fct_relevel(res, na_lvl, after = Inf)
  } else {
    res
  }
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of dm.
#' library(dm)
#'
#' df1 <- data.frame(
#'   var1 = c("a", "b", NA),
#'   var2 = factor(c("F1", "F2", NA))
#' )
#'
#' df2 <- data.frame(
#'   var1 = c("x", NA, "y"),
#'   var2 = factor(c("F11", NA, "F22"))
#' )
#'
#' db <- dm(df1 = df1, df2 = df2)
#'
#' format <- list(
#'   df1 = list(
#'     var1 = rule("X" = "x", "N" = NA)
#'   ),
#'   df2 = list(
#'     var1 = empty_rule,
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.dm <- function(obj,
                        format,
                        na_last = FALSE) {
  checkmate::assert_class(obj, "dm")

  ls_df <- as.list(obj)
  ls_res <- reformat(ls_df, format = format, na_last = na_last)
  res <- as_dm(ls_res)

  pk <- dm::dm_get_all_pks(obj)

  for (i in seq_len(nrow(pk))) {
    res <- dm_add_pk(res, !!pk[["table"]][i], !!pk[["pk_col"]][[i]])
  }

  fk <- dm::dm_get_all_fks(obj)

  for (i in seq_len(nrow(fk))) {
    res <- dm::dm_add_fk(
      res,
      !!fk[["child_table"]][i],
      !!fk[["child_fk_cols"]][[i]],
      !!fk[["parent_table"]][i],
      !!fk[["parent_key_cols"]][[i]]
    )
  }

  res
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of list of data.frame.
#' df1 <- data.frame(
#'   var1 = c("a", "b", NA),
#'   var2 = factor(c("F1", "F2", NA))
#' )
#'
#' df2 <- data.frame(
#'   var1 = c("x", NA, "y"),
#'   var2 = factor(c("F11", NA, "F22"))
#' )
#'
#' db <- list(df1 = df1, df2 = df2)
#'
#' format <- list(
#'   df1 = list(
#'     var1 = rule("X" = "x", "N" = NA)
#'   ),
#'   df2 = list(
#'     var1 = empty_rule,
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.list <- function(obj,
                          format,
                          na_last = FALSE) {
  checkmate::assert_list(obj, type = c("data.frame", "tibble"))
  checkmate::assert_named(obj)
  checkmate::assert_list(format, names = "unique", types = "list", null.ok = TRUE)

  if (length(format) == 0) {
    return(obj)
  }

  lapply(format, function(x) {
    checkmate::assert_list(x, names = "unique", types = "rule")
  })

  for (tab in names(format)) {
    if (is(format[[tab]], "empty_rule")) next

    local_map <- format[[tab]]
    local_map <- local_map[names(local_map) %in% names(obj[[tab]])]


    obj[[tab]][names(local_map)] <- mapply(
      function(rl, col) reformat(obj[[tab]][[col]], format = rl, na_last = na_last),
      local_map,
      names(local_map),
      SIMPLIFY = FALSE
    )
  }

  obj
}
