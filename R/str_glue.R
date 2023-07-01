#' Safe transformer
#' @keywords internal
safe_transformer <- function(text, envir) {
  if (!exists(text, envir = envir)) {
    text
  }
  else {
    get(text, envir = envir)
  }
}

#' `str` class
#' @export
setClass(
  "str",
  contains = "character"
)

#' create `str` object
#' @export
str <- function(x) {
  checkmate::assert_string(x)
  new("str", x)
}

#' toString method
#' @export
setMethod(
  "toString",
  "str",
  function(x, ...) {
    glue::glue(x, .envir = parent.frame(), .transformer = safe_transformer)
  }
)

#' `show` method
#' @export
setMethod(
  "show",
  "str",
  function(object) {
    cat(toString(object), "\n")
  }
)

#' Conversion from `str` to `character`
#' @export
#' @name `as`
setAs("str", "character", function(from, to) toString(from))
