#' Create rule based on mappings
#' @param ... Mapping pairs, the argument name is the transformed while
#' its values are original values.
#' @param .lst (`list`) of mapping.
#' @param .string_as_fct (`flag`) whether to convert characters to factors.
#' @param .na_last (`flag`)  whether the level replacing `NA` should be last.
#' @param .drop (`flag`) whether to drop empty levels.
#' @param .to_NA (`character`) values that should be converted to `NA`. Set to `NULL` if nothing should be converted to
#'   `NA`.
#' @returns a `rule` object.
#'
#' @note Conversion to `NA` is the last step of the remapping process.
#'
#' @export
#' @examples
#' rule("X" = "x", "Y" = c("y", "z"))
#' rule("X" = "x", "Y" = c("y", "z"), .drop = TRUE, .to_NA = c("a", "b"), .na_last = FALSE)
#'
rule <- function(..., .lst = list(...), .string_as_fct = TRUE, .na_last = TRUE, .drop = FALSE, .to_NA = "") {
  checkmate::assert_flag(.string_as_fct)
  checkmate::assert_flag(.na_last)
  checkmate::assert_flag(.drop)
  checkmate::assert_character(.to_NA, null.ok = TRUE, any.missing = FALSE)

  .lst[is.na(.lst)] <- NA_character_
  if (!checkmate::test_list(.lst, types = c("character"))) {
    rlang::abort("Value mapping may only contain the type: {character}")
  }
  vals <- as.character(unlist(.lst, use.names = FALSE))
  checkmate::assert_character(vals, unique = TRUE)
  nms <- unlist(lapply(seq_len(length(.lst)), function(x) {
    rep(names(.lst)[x], length(.lst[[x]]))
  }))

  res <- structure(
    setNames(vals, nms),
    class = c("rule", "character"),
    .string_as_fct = .string_as_fct,
    .na_last = .na_last,
    .drop = .drop,
    .to_NA = .to_NA
  )

  res
}

#' @export
#'
print.rule <- function(x, ...) {
  cat("Mapping of:\n")
  nms <- unique(names(x))
  if (length(x) == 0) {
    cat("Empty mapping.\n")
  } else {
    for (i in nms) {
      ori_nms <- unlist(x[names(x) %in% i])
      ori_nms <- ifelse(is.na(ori_nms), "<NA>", stringr::str_c("\"", ori_nms, "\""))
      ori_nms <- toString(ori_nms)
      cat(i, " <- ", ori_nms, "\n")
    }
  }
  .to_NA <- attr(x, ".to_NA")
  if (!is.null(.to_NA)) {
    cat("Convert to <NA>:", toString(stringr::str_c("\"", .to_NA, "\"")), "\n")
  }
  cat("Convert to factor:", attr(x, ".string_as_fct"), "\n")
  cat("Drop unused level:", attr(x, ".drop"), "\n")
  cat("NA-replacing level in last position:", attr(x, ".na_last"), "\n")
}

#' Convert nested list into list of `rule`
#' @param obj (`nested list`) to convert into list of rules.
#' @returns a `list` of `rule` objects.
#' @export
#' @examples
#' obj <- list(
#'   rule1 = list("X" = c("a", "b"), "Z" = "c", .to_NA = "xxxx"),
#'   rule2 = list(Missing = c(NA, "")),
#'   rule3 = list(Missing = c(NA, ""), .drop = TRUE),
#'   rule4 = list(Absent = c(NA, ""), .drop = TRUE, .to_NA = "yyyy")
#' )
#' list2rules(obj)
#'
list2rules <- function(obj) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(obj, types = "list", add = coll)
  checkmate::assert_names(names(obj), type = "unique", add = coll)
  checkmate::reportAssertions(coll)

  lapply(obj, function(x) {
    do.call("rule", x)
  })
}

#' Convert Rule to List
#' @param x (`rule`) to convert.
#' @param ... not used.
#' @returns an object of class `list`.
#'
#' @export
#' @examples
#' x <- rule("a" = c("a", "b"), "X" = "x", .to_NA = c("v", "w"))
#' as.list(x)
as.list.rule <- function(x, ...) {
  nms <- names(x)
  unames <- unique(nms)
  res <- lapply(unames, function(i) {
    unname(x[nms == i])
  })


  att <- attributes(x)
  arg <- att[!names(att) %in% c("names", "class")]

  res <- c(res, unname(arg))
  unames <- c(unames, names(arg))

  r_list <- setNames(res, unames)

  # Explicitly declare .to_NA value, even if NULL.
  .to_NA <- r_list[[".to_NA"]]
  if (is.null(.to_NA)) {
    r_list[".to_NA"] <- list(NULL)
  }

  r_list
}

#' Combine Two Rules
#'
#' @param x (`rule`) to modify.
#' @param y (`rule`) rule whose mapping will take precedence over the ones described in `x`.
#' @param ... not used.
#'
#' @note The order of the mappings in the resulting rule corresponds to the order of the mappings in `x` followed by the
#'   mappings that are only present in `y`.
#'
#' @returns a `rule`.
#' @export
#' @examples
#' r1 <- rule(
#'   "first" = c("from ori rule", "FROM ORI RULE"),
#'   "last" = c(NA, "last"),
#'   .to_NA = "X",
#'   .drop = TRUE
#' )
#' r2 <- rule(
#'   "first" = c("F", "f"),
#'   "second" = c("S", "s"),
#'   "third" = c("T", "t"),
#'   .to_NA = "something"
#' )
#' combine_rules(r1, r2)
combine_rules <- function(x, y, ...) {
  checkmate::assert_class(x, "rule", null.ok = TRUE)
  checkmate::assert_class(y, "rule", null.ok = TRUE)

  if (is.null(x) && is.null(y)) {
    rlang::abort("Both rules are NULL.")
  }

  # If one of the rules is NULL, return the other (via empty list).
  x <- as.list(x)
  y <- as.list(y)

  x[names(y)] <- y

  r <- do.call(rule, x)
  r
}

#' Combine Rules Found in Lists of Rules.
#'
#' @param x (`list`) of `rule` objects.
#' @param val (`list`) of `rule` objects.
#' @param ... passed to [`dunlin::combine_rules`].
#'
#' @returns a `list` of `rule` objects.
#' @export
#' @examples
#' l1 <- list(
#'   r1 = rule(
#'     "first" = c("overwritten", "OVERWRITTEN"),
#'     "almost first" = c(NA, "almost")
#'   ),
#'   r2 = rule(
#'     ANYTHING = "anything"
#'   )
#' )
#'
#' l2 <- list(
#'   r1 = rule(
#'     "first" = c("F", "f"),
#'     "second" = c("S", "s"),
#'     "third" = c("T", "t"),
#'     .to_NA = "something"
#'   ),
#'   r3 = rule(
#'     SOMETHING = "something"
#'   )
#' )
#'
#' combine_list_rules(l1, l2)
combine_list_rules <- function(x, val, ...) {
  # Unique names prevents zero-character names.
  checkmate::assert_list(x, types = "rule", null.ok = FALSE, names = "unique")
  checkmate::assert_list(val, types = "rule", null.ok = FALSE, names = "unique")

  vnames <- names(val)

  for (v in vnames) {
    x[[v]] <- combine_rules(x[[v]], val[[v]], ...)
  }
  x
}
