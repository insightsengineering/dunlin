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
  checkmate::assert_character(x)
  new("str", x)
}

#' toString method
#' @export
setMethod(
  "toString",
  "str",
  function(x, ...) {
    vapply(
      x,
      glue::glue,
      .envir = parent.frame(),
      .transformer = safe_transformer,
      FUN.VALUE = character(1)
    )
    
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

setAs("str", "character", function(from, to) toString(from))
