#' Create rule based on mappings
#' @param ... Mapping pairs, the argument name is the transformed while
#' its values are original values.
#' @param .lst (`list`) of mapping.
#' @param drop (`flag`) whether to drop empty levels.
#' @param to_NA (`character`) values that should be converted to `NA`.
#'
#' @note Conversion to `NA` is the last step of the remapping process.
#'
#' @export
#' @examples
#' rr <- rule("X" = "x", "Y" = c("y", "z"), to_NA = c("a", "b"))
#' rr
#'
rule <- function(..., .lst = list(...), drop = FALSE, to_NA = NULL) {
  checkmate::assert_list(.lst, types = c("character", "numeric", "logical"), any.missing = FALSE)
  checkmate::assert_flag(drop)
  checkmate::assert_character(to_NA, null.ok = TRUE, any.missing = FALSE)

  if (length(.lst) == 0) {
    return(empty_rule)
  } else {
    vals <- as.character(unlist(.lst, use.names = FALSE))
    checkmate::assert_character(vals, unique = TRUE)
    nms <- unlist(lapply(seq_len(length(.lst)), function(x) {
      rep(names(.lst)[x], length(.lst[[x]]))
    }))
    structure(
      setNames(vals, nms),
      class = c("rule", "character"),
      drop = drop,
      to_NA = to_NA
    )
  }
}

#' Create empty rule
#' @export
empty_rule <- structure(
  character(0L),
  class = c("empty_rule", "rule", "character")
)

#' @export
#'
print.rule <- function(x, ...) {
  cat("Mapping of:\n")
  nms <- names(x)
  for (i in seq_len(length(x))) {
    cat(nms[i], " <- ", if (length(x[[i]]) > 1) sprintf("[%s]", toString(x[[i]])) else x[[i]], "\n")
  }
  if (!is.null(attr(x, "to_NA"))) cat("NA <- ", toString(attr(x, "to_NA")), "\n")
  cat("Drop unused level:", attr(x, "drop"), "\n")
}

#' Convert nested list into list of `rule`
#' @param obj (`nested list`) to convert into list of rules.
#' @export
#' @examples
#' obj <- list(
#'   rule1 = list("X" = c("a", "b"), "Z" = "c"),
#'   rule2 = list(Missing = c(NA, ""))
#' )
#' list2rules(obj)
#'
list2rules <- function(obj) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(obj, unique = TRUE, types = "list", add = coll)
  checkmate::assert_names(names(obj), type = "unique", add = coll)
  checkmate::reportAssertions(coll)

  lapply(obj, function(x) rule(.lst = x))
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
  setNames(res, unames)
}

#' @export
print.empty_rule <- function(x, ...) {
  cat("Empty mapping\n")
}

#' Read `YAML` File describing `rule`
#' @param file (`string`) of path to the rule `YAML` file.
#' @export
read_rules <- function(file) {
  checkmate::assert_file_exists(file)
  content <- yaml::read_yaml(file)
  list2rules(content)
}
