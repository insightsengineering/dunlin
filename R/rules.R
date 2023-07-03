#' Create rule based on mappings
#' @param ... Mapping pairs, the argument name is the transformed while
#' its values are original values. Special name enable more control on the behavior or the `rule`. See below.
#' @param .lst (`list`) of mapping.
#'
#' @note Conversion to `NA` is the last step of the remapping process.
#' @note Special mapping values include:
#' * `.string_as_fct` (`flag`) whether to convert characters to factors.
#' * `.na_last` (`flag`)  whether the level replacing `NA` should be last.
#' * `.drop` (`flag`) whether to drop empty levels.
#' * `.to_NA` (`character`) values that should be converted to `NA`.
#'
#' @export
#' @examples
#' rule("X" = "x", "Y" = c("y", "z"))
#' rule("X" = "x", "Y" = c("y", "z"), .drop = TRUE, .to_NA = c("a", "b"), .na_last = FALSE)
#'
rule <- function(..., .lst = list(...)) {
  checkmate::assert_list(.lst)

  special_mapping <- list(
    ".string_as_fct" = "logical",
    ".drop" = "logical",
    ".to_NA" = c("character", "NULL"),
    ".na_last" = "logical"
  )

  map <- .lst[setdiff(names(.lst), names(special_mapping))]
  names_arg <- intersect(names(.lst), names(special_mapping))

  if (length(map) == 0) {
    res <- empty_rule
    attr(res, ".string_as_fct") <- .lst[[".string_as_fct"]] %||% TRUE
    return(res)
  } else {
    map[is.na(map)] <- NA_character_
    if (!checkmate::test_list(map, types = c("character"))) {
      stop("Value mapping may only contain the type: {character}")
    }

    vals <- as.character(unlist(map, use.names = FALSE))
    checkmate::assert_character(vals, unique = TRUE)
    nms <- unlist(lapply(seq_len(length(map)), function(x) {
      rep(names(map)[x], length(map[[x]]))
    }))

    # Set default value of the rule.
    res <- structure(
      setNames(vals, nms),
      class = c("rule", "character"),
      .string_as_fct = TRUE,
      .drop = FALSE,
      .to_NA = NULL,
      .na_last = TRUE
    )


    for (i in names_arg) {
      checkmate::assert_multi_class(.lst[[i]], special_mapping[[i]], .var.name = names(.lst[i]))
      attr(res, i) <- .lst[[i]]
    }

    res
  }
}

#' Create empty rule
#' @export
empty_rule <- structure(
  character(0L),
  class = c("empty_rule", "rule", "character"),
  .string_as_fct = TRUE
)

#' @export
#'
print.rule <- function(x, ...) {
  cat("Mapping of:\n")
  nms <- names(x)
  for (i in seq_len(length(x))) {
    cat(nms[i], " <- ", if (length(x[[i]]) > 1) sprintf("[%s]", toString(x[[i]])) else x[[i]], "\n")
  }
  if (!is.null(attr(x, ".to_NA"))) cat("NA <- ", toString(attr(x, ".to_NA")), "\n")
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
