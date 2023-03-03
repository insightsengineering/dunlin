#' Reformat Values
#' @param obj object to reformat.
#' @param format (`list`) or (`list`) of `lists` depending on the class of obj.
#' @param string_as_fct (`flag`) whether the reformatted character object should be converted to factor.
#' @param na_last (`flag`) whether the level replacing `NA` should be last.
#' @param ... not used. Only for compatibility between methods.
#'
#' @export
#'
#' @rdname reformat
#'
reformat <- function(obj, ...) {
  UseMethod("reformat")
}

#' @export
#' @rdname reformat
reformat.default <- function(obj, format, ...) {
  if (length(format) != 0) {
    warning("Not implemented! Only empty list format allowed.")
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
#' format <- list("A" = "a", "NN" = c(NA, ""))
#'
#' reformat(obj, format, string_as_fct = FALSE)
reformat.character <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_list(format, types = c("character", "logical"))
  checkmate::assert_flag(string_as_fct)
  checkmate::assert_flag(na_last)

  if (string_as_fct) {
    # Keep attributes.
    att <- attributes(obj)
    obj_fact <- as.factor(obj)
    supp_att_name <- setdiff(names(att), attributes(obj_fact))
    supp_att <- att[supp_att_name]
    attributes(obj_fact) <- c(attributes(obj_fact), supp_att)

    if (length(format) == 0) {
      return(obj_fact)
    }

    reformat(obj_fact, format, na_last = na_last)
  } else {
    if (length(format) == 0) {
      return(obj)
    }

    format <- h_unnest_format(format)
    m <- match(obj, format)
    obj[!is.na(m)] <- names(format)[m[!is.na(m)]]
    obj
  }
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of factor.
#' obj <- factor(c("a", "aa", "x", "NA", "b"), levels = c("x", "b", "aa", "a", "z"))
#' attr(obj, "label") <- "my label"
#' format <- list("A" = c("a", "aa"), "Missing" = c(NA, "x"), "Not Present" = "z")
#'
#' reformat(obj, format)
#' reformat(obj, format, na_last = FALSE)
reformat.factor <- function(obj, format, na_last = TRUE, ...) {
  checkmate::assert_list(format, types = c("character", "logical"))
  checkmate::assert_flag(na_last)

  if (length(format) == 0) {
    return(obj)
  }

  format <- h_unnest_format(format)

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
#'     var1 = list("X" = "x", "N" = NA)
#'   ),
#'   df2 = list(
#'     var1 = list(),
#'     var2 = list("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.dm <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_list(format, names = "unique", types = "list", null.ok = TRUE)
  checkmate::assert_flag(string_as_fct)
  checkmate::assert_flag(na_last)

  pk <- dm::dm_get_all_pks(obj)
  fk <- dm::dm_get_all_fks(obj)

  obj <- as.list(obj)
  obj <- reformat(obj, format = format, string_as_fct = string_as_fct, na_last = na_last)
  obj <- as_dm(obj)

  for (i in seq_len(nrow(pk))) {
    obj <- dm_add_pk(obj, !!pk[["table"]][i], !!pk[["pk_col"]][[i]])
  }

  for (i in seq_len(nrow(fk))) {
    obj <- dm::dm_add_fk(
      obj,
      !!fk[["child_table"]][i],
      !!fk[["child_fk_cols"]][[i]],
      !!fk[["parent_table"]][i],
      !!fk[["parent_key_cols"]][[i]]
    )
  }

  obj
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
#'     var1 = list("X" = "x", "N" = c(NA, ""))
#'   ),
#'   df2 = list(
#'     var1 = list(),
#'     var2 = list("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.list <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_list(obj, type = c("data.frame", "tibble"))
  checkmate::assert_named(obj)
  checkmate::assert_flag(string_as_fct)
  checkmate::assert_flag(na_last)


  if (length(format) == 0) {
    return(obj)
  }

  assert_valid_format(format)

  for (tab in names(format)) {
    if (is(format[[tab]], "empty_rule")) next

    local_map <- format[[tab]]
    local_map <- local_map[names(local_map) %in% names(obj[[tab]])]


    obj[[tab]][names(local_map)] <- mapply(
      function(rl, col) reformat(obj[[tab]][[col]], format = rl, string_as_fct = string_as_fct, na_last = na_last),
      local_map,
      names(local_map),
      SIMPLIFY = FALSE
    )
  }

  obj
}
