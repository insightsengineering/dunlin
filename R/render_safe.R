#' Safe transformer
#' @details Obtain content in global environment by default.
#' If not found, use the environment here.
#' @keywords internal
safe_transformer <- function(text, envir) {
  text_lower <- tolower(text)
  res <- if (exists(text_lower, envir = envir, inherits = FALSE)) {
    get(text_lower, envir = envir)
  } else {
    text
  }
  if (is.character(res)) {
    if (identical(text, tolower(text))) {
      res <- tolower(res)
    } else if (identical(text, toupper(text))) {
      res <- toupper(res)
    } else if (identical(text, stringr::str_to_title(text))) {
      res <- stringr::str_to_title(res)
    }
  }
  res
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
    .open = "{",
    .close = "}"
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
  invisible()
}

#' Remove whisker values
#' @param x Named (`character`) input.
#' @export
remove_whisker <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  rm(list = x, envir = whisker_env)
}
