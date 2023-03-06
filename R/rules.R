#' Create rule based on mappings
#' @param ... Mapping pairs, the argument name is the transformed while
#' its values are original values.
#' @param .lst (`list`) of mapping.
#' @export
rule <- function(..., .lst = list(...)) {
  checkmate::assert_list(.lst, types = c("character", "numeric", "logical"), any.missing = FALSE)
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
      class = c("rule", "character")
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

#' Read Format yaml file
#' @param file (`string`) of path to the rule yaml file.
#' @export
read_format <- function(file) {
  checkmate::assert_file_exists(file)
  content <- yaml::read_yaml(file)
  assert_valid_list_format(content)

  lapply(content, function(tab) lapply(tab, function(r) rule(.lst = r)))
}

#' Write Format yaml file
#' @param format (`list`) object.
#' @param file (`string`) file path to write to.
#' @export
write_format <- function(format, file) {
  assert_valid_format(format)
  checkmate::assert_string(file)

  res <- rapply(format, as.list, classes = "rule", how = "replace")

  yaml::write_yaml(res, file)
}
