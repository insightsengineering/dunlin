#' Read rule yaml file
#' @param file (`string`) of path to the rule yaml file.
#' @export
read_rules <- function(file) {
  checkmate::assert_file_exists(file)
  content <- yaml::read_yaml(file)
  rules(.lst = lapply(content, do.call, what = rule))
}

#' Write rule
#' @param rules (`rules`) object.
#' @param file (`string`) file path to write to.
#' @param append (`flag`) indicator of whether to overwrite or add rules.
#' @export
write_rules <- function(rules, file, append = TRUE) {
  checkmate::assert_class(rules, "rules")
  checkmate::assert_string(file)
  checkmate::assert_flag(append)
  if (append) {
    if (file.exists(file)) {
      original <- read_rules(file)
    } else {
      original <- rules()
    }
    yaml::write_yaml(as.list(append_rules(original, rules)), file)
  } else {
    yaml::write_yaml(as.list(rules), file)
  }
}

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

#' Create rule sets
#' @param ... Named arguments of rules.
#' @param .lst (`list`) of rules.
#' @export
rules <- function(..., .lst = list(...)) {
  checkmate::assert_list(.lst, types = "rule", any.missing = FALSE, names = "unique")
  class(.lst) <- c("rules", "list")
  return(.lst)
}

#' Append rules to an existing rules sets
#' @param rules_base (`rules`) to update.
#' @param rule_add (`rules`) to add/update.
#' @export
append_rules <- function(rules_base, rule_add) {
  checkmate::assert_class(rules_base, "rules")
  checkmate::assert_class(rule_add, "rules")
  rules(.lst = modifyList(rules_base, rule_add))
}

#' @export
print.rule <- function(x) {
  cat("Mapping of:\n")
  nms <- names(x)
  for (i in seq_len(length(x))) {
    cat(nms[i], " <- ", if (length(x[[i]]) > 1) sprintf("[%s]", toString(x[[i]])) else x[[i]], "\n")
  }
}

#' @export
print.empty_rule <- function(x) {
  cat("Empty mapping\n")
}

#' @export
print.rules <- function(x) {
  for (i in names(x)) {
    cat("rule ", i, "\n")
    print(x[[i]])
  }
}

#' @export
as.list.rule <- function(x) {
  nms <- names(x)
  unames <- unique(nms)
  res <- lapply(unames, function(i) {
    unname(x[nms == i])
  })
  setNames(res, unames)
}

#' @export
as.list.rules <- function(x) {
  class(x) <- "list"
  lapply(x, as.list.rule)
}
