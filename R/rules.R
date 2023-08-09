#' Create rule based on mappings
#' @param ... Mapping pairs, the argument name is the transformed while
#' its values are original values.
#' @param .lst (`list`) of mapping.
#' @param .string_as_fct (`flag`) whether to convert characters to factors.
#' @param .na_last (`flag`)  whether the level replacing `NA` should be last.
#' @param .drop (`flag`) whether to drop empty levels.
#' @param .to_NA (`character`) values that should be converted to `NA`.
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
  nms <- names(x)
  if (length(x) == 0) {
    cat("Empty mapping.\n")
  } else {
    for (i in seq_len(length(x))) {
      cat(nms[i], " <- ", if (length(x[[i]]) > 1) sprintf("[%s]", toString(x[[i]])) else x[[i]], "\n")
    }
  }
  if (!is.null(attr(x, ".to_NA"))) cat("NA <- ", toString(attr(x, ".to_NA")), "\n")
  cat("Convert to factor:", attr(x, ".string_as_fct"), "\n")
  cat("Drop unused level:", attr(x, ".drop"), "\n")
  cat("NA-replacing level in last position:", attr(x, ".na_last"), "\n")
}

#' Convert nested list into list of `rule`
#' @param obj (`nested list`) to convert into list of rules.
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
  checkmate::assert_list(obj, unique = TRUE, types = "list", add = coll)
  checkmate::assert_names(names(obj), type = "unique", add = coll)
  checkmate::reportAssertions(coll)

  lapply(obj, function(x) {
    do.call("rule", x)
  })
}

#' Convert Rule to List
#' @param x (`rule`) to convert.
#' @param ... not used.
#'
#' @export
#' @examples
#'
#' x <- rule("a" = c("a", "b"), "X" = "x")
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

  setNames(res, unames)
}
