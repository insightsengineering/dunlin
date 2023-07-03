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
render_safe <- function(x) {
  checkmate::assert_character(x)
  ret <- lapply(
    x,
    glue::glue,
    .transformer = safe_transformer,
    .envir = whisker_env,
    .null = "NULL",
    .open = "{{",
    .close = "}}"
  )
  ret <- vapply(ret, `[[`, i = 1L, FUN.VALUE = "")
  setNames(ret, names(x))
}
#' Add whisker values
#' @export
add_whisker <- function(x) {
  checkmate::assert_character(x, names = "unique", any.missing = FALSE)
  lapply(
    names(x),
    function(i) {
      assign(i, x[i], envir = whisker_env)
    }
  )
}
