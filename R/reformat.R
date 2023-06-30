#' Reformat Values
#' @param obj object to reformat.
#' @param format (`rule`) or (`list`) of `rule` depending on the class of obj.
#' @param string_as_fct (`flag`) whether the reformatted character object should be converted to factor.
#' @param na_last (`flag`) whether the level replacing `NA` should be last.
#' @param ... used to pass additional arguments
#' * `.to_NA` (`character`) values that should be converted to `NA`. For `factor`, the corresponding levels are
#'   dropped. If `NULL`, the argument will be taken from the `to_NA`attribute of the rule.
#' * `.drop` (`flag`) whether to drop empty levels. If `NULL`, the argument will be taken from the `drop`attribute of
#'   the rule.
#'
#' @export
#' @note When the rule is empty rule or when values subject to reformatting are absent from the object, no error is
#'   raised. The rest of the reformatting process (for instance the conversion to factor  and the reformatting of
#'   factors levels if `string_as_fct = TRUE`) is still carried out. The conversion of the levels declared in `to_NA` to
#'   `NA` values occurs after the remapping. `NA` values created this way are not affected by a rule declaring a
#'   remapping of `NA` values. For factors, level dropping is the last step, hence, levels converted to `NA` by the
#'   `to_NA` argument, will be removed if `drop` is `TRUE`.
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
#' obj <- c("a", "b", "x", NA)
#' attr(obj, "label") <- "my label"
#' format <- rule("A" = "a", "NN" = NA)
#'
#' reformat(obj, format)
#' reformat(obj, format, string_as_fct = FALSE, .to_NA = "x")
#'
reformat.character <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) { # .to_NA = NULL, .drop = FALSE,
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

    reformat(obj_fact, format, na_last = na_last, ...)
  } else {
    if (is(format, "empty_rule")) {
      return(obj)
    }

    format <- modify_rule_attr(..., format = format)

    value_match <- unlist(format)
    m <- match(obj, value_match)
    obj[!is.na(m)] <- names(format)[m[!is.na(m)]]


    val_to_NA <- attr(format, "arg")[[".to_NA"]]
    if (!is.null(val_to_NA)) {
      obj[obj %in% val_to_NA] <- NA_character_
    }
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
#' format <- rule("A" = c("a", "aa"), "NN" = c(NA, "x"), "Not_present" = "z", "Not_a_level" = "P")
#'
#' reformat(obj, format)
#' reformat(obj, format, na_last = FALSE, .to_NA = "b", .drop = TRUE)
#'
reformat.factor <- function(obj, format, na_last = TRUE, ...) {
  checkmate::assert_class(format, "rule")
  checkmate::assert_flag(na_last)

  if (is(format, "empty_rule")) {
    return(obj)
  }

  format <- modify_rule_attr(..., format = format)

  any_na <- anyNA(obj)

  checkmate::assert_class(format, "rule")
  if (any(is.na(format)) && any_na) {
    obj <- forcats::fct_na_value_to_level(obj)
  }

  absent_format <- format[!format %in% levels(obj)]
  sel_format <- format[format %in% levels(obj)]
  res <- forcats::fct_recode(obj, !!!sel_format)
  res <- forcats::fct_expand(res, unique(names(absent_format)))
  res <- forcats::fct_relevel(res, unique(names(format)))

  if (any(is.na(format)) && na_last) {
    na_lvl <- names(format)[is.na(format)]
    res <- forcats::fct_relevel(res, na_lvl, after = Inf)
  }

  val_to_NA <- attr(format, "arg")[[".to_NA"]]
  res <- if (!is.null(val_to_NA)) {
    forcats::fct_na_level_to_value(res, val_to_NA)
  } else {
    res
  }

  drop_lvl <- attr(format, "arg")[[".drop"]]
  res <- if (drop_lvl) {
    forcats::fct_drop(res)
  } else {
    res
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
#'     var1 = rule("X" = "x", "N" = NA, .to_NA = "b")
#'   ),
#'   df2 = list(
#'     var1 = empty_rule,
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   )
#' )
#'
#' reformat(db, format)
reformat.list <- function(obj, format, string_as_fct = TRUE, na_last = TRUE, ...) {
  checkmate::assert_list(obj, types = c("data.frame", "tibble"))
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
      function(rl, col) reformat(obj[[tab]][[col]], format = rl, string_as_fct = string_as_fct, na_last = na_last, ...),
      local_map,
      names(local_map),
      SIMPLIFY = FALSE
    )
  }

  obj
}
