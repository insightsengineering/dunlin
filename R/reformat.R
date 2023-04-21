#' Reformat Values
#' @param obj object to reformat.
#' @param format (`rule`) or (`list`) of `rule` depending on the class of obj.
#' @param string_as_fct (`flag`) whether the reformatted character object should be converted to factor.
#' @param bool_as_fct (`flag`) whether to reformat an object of class `logical`.
#'
#' @param na_last (`flag`) whether the level replacing `NA` should be last.
#' @param ... not used. Only for compatibility between methods.
#'
#' @export
#' @note When the rule is empty rule or when values subject to reformatting are absent from the object, no error is
#'   raised. The rest of the reformatting process (for instance the conversion to factor  and the reformatting of
#'   factors levels if `string_as_fct = TRUE`) is still carried out.
#'
#' @rdname reformat
#'
reformat <- function(obj, ...) {
  UseMethod("reformat")
}

#' @export
#' @rdname reformat
reformat.default <- function(obj, format, ...) {
  if (!is(format, "empty_rule")) {
    warning(paste0(c("Not implemented for class: ", toString(class(obj)), "! Only empty rule allowed.")))
  }
  return(obj)
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of character.
#' obj <- c(TRUE, FALSE, TRUE, NA)
#' attr(obj, "label") <- "my label"
#' format <- rule("<Missing>" = NA)
#'
#' reformat(obj, format)
#' reformat(obj, format, bool_as_fct = TRUE)
reformat.logical <- function(obj, format, bool_as_fct = FALSE, na_last = TRUE, ...) {
  checkmate::assert_class(format, "rule")
  checkmate::assert_flag(bool_as_fct)

  if (bool_as_fct) {
    # Keep attributes.
    att <- attributes(obj)
    obj_fact <- as.factor(obj)
    supp_att_name <- setdiff(names(att), attributes(obj_fact))
    supp_att <- att[supp_att_name]
    attributes(obj_fact) <- c(attributes(obj_fact), supp_att)

    if (is(format, "empty_rule")) {
      return(obj_fact)
    }
    reformat(obj_fact, format, na_last = na_last)
  } else {
    return(obj)
  }
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
reformat.character <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_class(format, "rule")
  checkmate::assert_flag(string_as_fct)
  checkmate::assert_flag(na_last)

  if (string_as_fct) {
    # Keep attributes.
    att <- attributes(obj)
    obj_fact <- as.factor(obj)
    supp_att_name <- setdiff(names(att), attributes(obj_fact))
    supp_att <- att[supp_att_name]
    attributes(obj_fact) <- c(attributes(obj_fact), supp_att)

    if (is(format, "empty_rule")) {
      return(obj_fact)
    }

    reformat(obj_fact, format, na_last = na_last)
  } else {
    if (is(format, "empty_rule")) {
      return(obj)
    }

    value_match <- unlist(format)
    m <- match(obj, value_match)
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
#' obj <- factor(c("first", "a", "aa", "b", "x", NA), levels = c("first", "x", "b", "aa", "a", "z"))
#' attr(obj, "label") <- "my label"
#' format <- rule("A" = c("a", "aa"), "NN" = c(NA, "x"), "Not Present" = "z", "Not A level" = "P")
#'
#' reformat(obj, format)
#' reformat(obj, format, na_last = TRUE)
reformat.factor <- function(obj, format, na_last = TRUE, ...) {
  checkmate::assert_class(format, "rule")
  checkmate::assert_flag(na_last)

  if (is(format, "empty_rule")) {
    return(obj)
  }

  any_na <- anyNA(obj)

  checkmate::assert_class(format, "rule")
  if (any(is.na(format)) && any_na) {
    obj <- forcats::fct_na_value_to_level(obj)
  }

  format <- format[format %in% levels(obj)]
  res <- forcats::fct_recode(obj, !!!format)
  res <- forcats::fct_relevel(res, unique(names(format)))

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
reformat.dm <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  assert_valid_format(format)
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
#'     var1 = rule("X" = "x", "N" = NA)
#'   ),
#'   df2 = list(
#'     var1 = empty_rule,
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.list <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_list(obj, type = c("data.frame", "tibble"))
  checkmate::assert_named(obj)
  checkmate::assert_list(format, names = "unique", types = "list", null.ok = TRUE)
  checkmate::assert_flag(string_as_fct)
  checkmate::assert_flag(na_last)

  if (length(format) == 0) {
    return(obj)
  }

  assert_valid_format(format)

  for (tab in names(format)) {
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
