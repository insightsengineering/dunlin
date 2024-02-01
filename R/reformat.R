#' Reformat Values
#' @param obj (`character`, `factor` or `list of data.frame`) to reformat.
#' @param format (`rule`) or (`list`) of `rule` depending on the class of obj.
#' @param ... for compatibility between methods and pass additional special mapping to transform rules.
#' * `.string_as_fct` (`flag`) whether the reformatted character object should be converted to factor.
#' * `.to_NA` (`character`) values that should be converted to `NA`. For `factor`, the corresponding levels are
#'   dropped. If `NULL`, the argument will be taken from the `to_NA`attribute of the rule.
#' * `.drop` (`flag`) whether to drop empty levels. If `NULL`, the argument will be taken from the `drop`attribute of
#'   the rule.
#' * `.na_last` (`flag`) whether the level replacing `NA` should be last.
#' @returns (`character`, `factor` or `list of data.frame`) with remapped values.
#'
#' @export
#' @note When the rule is empty rule or when values subject to reformatting are absent from the object, no error is
#'   raised. The conversion to factor if `.string_as_fct = TRUE`) is still carried out. The conversion of the levels
#'   declared in `.to_NA` to `NA` values occurs after the remapping. `NA` values created this way are not affected by a
#'   rule declaring a remapping of `NA` values. For factors, level dropping is the last step, hence, levels converted to
#'   `NA` by the `.to_NA` argument, will be removed if `.drop` is `TRUE`. Arguments passed via `reformat` override the
#'   ones defined during rule creation.
#'
#' @rdname reformat
#'
reformat <- function(obj, ...) {
  UseMethod("reformat")
}

#' @export
#' @rdname reformat
reformat.default <- function(obj, format, ...) {
  rlang::warn(paste0("Not implemented for class: ", toString(class(obj)), "! Returning original object."))
  return(obj)
}

#' @export
#' @rdname reformat
#'
#' @examples
#'
#' # Reformatting of character.
#' obj <- c("a", "b", "x", NA, "")
#' attr(obj, "label") <- "my label"
#' format <- rule("A" = "a", "NN" = NA)
#'
#' reformat(obj, format)
#' reformat(obj, format, .string_as_fct = FALSE, .to_NA = NULL)
#'
reformat.character <- function(obj, format, ...) {
  checkmate::assert_class(format, "rule")

  # Give priority to argument defined in reformat.
  format <- do.call(rule, modifyList(as.list(format), list(...), keep.null = TRUE))

  if (attr(format, ".string_as_fct")) {
    # Keep attributes.
    att <- attributes(obj)
    obj_fact <- as.factor(obj)
    supp_att_name <- setdiff(names(att), attributes(obj_fact))
    supp_att <- att[supp_att_name]
    attributes(obj_fact) <- c(attributes(obj_fact), supp_att)

    reformat(obj_fact, format)
  } else {
    value_match <- unlist(format)
    m <- match(obj, value_match)
    obj[!is.na(m)] <- names(format)[m[!is.na(m)]]
    val_to_NA <- attr(format, ".to_NA")
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
#' reformat(obj, format, .na_last = FALSE, .to_NA = "b", .drop = FALSE)
#'
reformat.factor <- function(obj, format, ...) {
  checkmate::assert_class(format, "rule")

  format <- do.call(rule, modifyList(as.list(format), list(...), keep.null = TRUE))

  any_na <- anyNA(obj)
  if (any(is.na(format)) && any_na) {
    obj <- forcats::fct_na_value_to_level(obj)
  }

  absent_format <- format[!format %in% levels(obj)]
  sel_format <- format[format %in% levels(obj)]
  obj <- forcats::fct_recode(obj, !!!sel_format)
  obj <- forcats::fct_expand(obj, unique(names(absent_format)))
  obj <- forcats::fct_relevel(obj, unique(names(format)))

  if (any(is.na(format)) && attr(format, ".na_last")) {
    na_lvl <- names(format)[is.na(format)]
    obj <- forcats::fct_relevel(obj, na_lvl, after = Inf)
  }

  drop_lvl <- attr(format, ".drop")
  if (drop_lvl) {
    obj <- forcats::fct_drop(obj)
  }

  # Levels converted to NA are dropped.
  val_to_NA <- attr(format, ".to_NA")
  if (!is.null(val_to_NA)) {
    obj <- forcats::fct_na_level_to_value(obj, val_to_NA)
  }

  obj
}

#' @export
#' @rdname reformat
#'
#' @note the variables listed under the `all_dataset` keyword will be reformatted with the corresponding rule in every
#'   data set except where another rule is specified for the same variable under a specific data set name.
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
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   ),
#'   all_datasets = list(
#'     var1 = rule("xx" = "x", "aa" = "a")
#'   )
#' )
#'
#' reformat(db, format)
reformat.list <- function(obj, format, ...) {
  checkmate::assert_list(obj, types = c("data.frame", "tibble"))
  checkmate::assert_named(obj)
  checkmate::assert_list(format, names = "unique", types = "list", null.ok = TRUE)

  if (length(format) == 0) {
    return(obj)
  }

  assert_valid_format(format)

  ls_datasets <- names(obj)
  format <- h_expand_all_datasets(format, ls_datasets)

  for (tab in ls_datasets) {
    local_map <- format[[tab]]
    local_map <- local_map[names(local_map) %in% names(obj[[tab]])]

    obj[[tab]][names(local_map)] <- mapply(
      function(rl, col) reformat(obj[[tab]][[col]], format = rl, ...),
      local_map,
      names(local_map),
      SIMPLIFY = FALSE
    )
  }

  obj
}

#' Propagate the rules for all datasets
#'
#' @inheritParams reformat
#' @param ls_datasets (`character`) the name of all datasets in the object to reformat.
#' @returns a nested `list` attributing a rule to be applied to specific variables of specific datasets.
#'
#' @details the rules described  under `all_datasets` are propagated to all datasets for the corresponding variables
#'   except in datasets where a rule is already attributed to the same variable.
#'
#' @keywords internal
h_expand_all_datasets <- function(format_list, ls_datasets = NULL) {
  assert_valid_list_format(list(f = format_list))
  checkmate::assert_character(ls_datasets, null.ok = TRUE)

  spec_datasets <- format_list[setdiff(names(format_list), "all_datasets")]

  if (!is.null(ls_datasets)) {
    to_all_datasets <- list()
    to_all_datasets[ls_datasets] <- format_list["all_datasets"]
    to_all_datasets <- base::Filter(function(x) !is.null(x), to_all_datasets)

    modifyList(to_all_datasets, spec_datasets)
  } else {
    spec_datasets
  }
}
