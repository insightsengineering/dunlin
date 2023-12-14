#' Safe transformer
#'
#' @param text (`string`) to be substituted.
#' @param envir (`environment`) containing key-value pairs describing the substitution to perform.
#' @returns `string` with substituted placeholders.
#'
#' @details Obtain content in global environment by default.
#' If not found, use the environment here. The function first looks for an exact match. If not found, it searches for a
#' match in lower case then apply to the result the same case as the original value.
#'
#' @keywords internal
safe_transformer <- function(text, envir) {
  if (exists(text, envir = envir, inherits = FALSE, mode = "character")) {
    res <- get(text, envir = envir, mode = "character")
    return(toString(res))
  }

  text_lower <- tolower(text)
  res <- if (exists(text_lower, envir = envir, inherits = FALSE, mode = "character")) {
    get(text_lower, envir = envir, mode = "character")
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

  return(toString(res))
}

#' Render whiskers safely
#' @param x (`character`) input to be rendered safely.
#' @returns `character` with substituted placeholders.
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
#' @returns invisible `NULL`. Assign the key-value pair provided as argument in the whisker environment.
#'
#' @details The names of the character gives the string to be replaced and the value gives the new string.
#'
#' @export
#' @examples
#' my_whiskers <- c(Placeholder = "Replacement", Placeholder2 = "Replacement2")
#' add_whisker(my_whiskers)
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
#' @returns invisible `NULL`. Removes `x` from the whisker environment.
#' @export
remove_whisker <- function(x) {
  checkmate::assert_character(x, any.missing = FALSE)
  rm(list = x, envir = whisker_env)
}

#' Show Whisker Values
#' @returns invisible `NULL`. Prints the values stored in the whisker environment.
#' @export
#' @examples
#' show_whisker()
show_whisker <- function() {
  l <- ls(envir = whisker_env)
  val <- lapply(
    l,
    function(x) {
      if (exists(x, envir = whisker_env, mode = "character")) {
        setNames(
          toString(get(x, envir = whisker_env, mode = "character")),
          x
        )
      }
    }
  )

  lapply(val, function(x) cat(sprintf("%s --> %s\n", names(x), x)))
  invisible()
}
