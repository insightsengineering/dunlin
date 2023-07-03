#' Safe transformer
#' @details Obtain content in global environment by default.
#' If not found, use the environment here.
#' @keywords internal
safe_transformer <- function(text, envir) {
  if (exists(text, envir = globalenv())) {
    get(text, envir = globalenv())
  } else if (exists(text, envir = envir)) {
    get(text, envir = envir)
  } else {
    text
  }
}

#' Render whiskers safely
#' @param x (`character`) input to be rendered safely.
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
#' @param x Named (`character`) input.
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
