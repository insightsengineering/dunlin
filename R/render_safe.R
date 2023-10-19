#' Safe transformer
#'
#' @param text (`string`) to be substituted.
#' @param envir (`environment`) containing key-value pairs describing the substitution to perform.
#'
#' @details Obtain content in global environment by default.
#' If not found, use the environment here. The function first looks for an exact match. If not found, it searches for a
#' match in lower case then apply to the result the same case as the original value.
#'
#' @keywords internal
safe_transformer <- function(text, envir) {
  if (exists(text, envir = envir, inherits = FALSE)) {
    res <- get(text, envir = envir)

    if (!checkmate::test_string(res)) {
      stop(
        sprintf(
          "%s should correspond to a string but is %s. use `add_whisker` or `remove_whisker` to correct it.",
          text,
          toString(class(res))
        )
      )
    }

    return(res)
  }

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
  if (!checkmate::test_string(res)) {
    stop(
      sprintf(
        "%s should correspond to a string but is %s. use `add_whisker` or `remove_whisker` to correct it.",
        text,
        toString(
          class(res)
        )
      )
    )
  }

  return(res)
}

#' Render whiskers safely
#' @param x (`character`) input to be rendered safely.
#'
#' @note The strings enclosed in `{}` are substituted using the key-values pairs set with `add_whiskers`.
#'
#' @export
#' @examples
#' render_safe("Name of {Patient_label}")
render_safe <- function(x) {
  checkmate::assert_character(x, null.ok = TRUE)
  if (is.null(x)) {
    return(NULL)
  }
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
#'
#' @details The names of the character gives the string to be replaced and the value gives the new string.
#'
#' @export
#' @examples
#' \dontrun{
#' my_whiskers <- c(Placeholder = "Replacement", Placeholder2 = "Replacement2")
#' add_whisker(my_whiskers)
#' }
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

#' Show Whisker Values
#' @export
#' @examples
#' show_whisker()
show_whisker <- function() {
  l <- ls(envir = whisker_env)
  val <- lapply(l, function(x) get(x, envir = whisker_env))

  val <- val[vapply(val, checkmate::test_string, logical(1))]

  lapply(val, function(x) cat(sprintf("%s --> %s\n", names(x), x)))
  invisible()
}
