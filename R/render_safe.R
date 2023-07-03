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

#' Render whiskers safely
#' @param x (`character`) input to be rendered safely.
#' @param envir (`environment`) in which input is rendered.
#' @export
render_safe <- function(x, envir = parent.frame(), ...) {
  checkmate::assert_character(x)
  ret <- lapply(
    x,
    glue::glue,
    .transformer = safe_transformer,
    .envir = envir,
    .null = "NULL"
  )
  ret <- vapply(ret, `[[`, i = 1L, FUN.VALUE = "")
  setNames(ret, names(x))
}
