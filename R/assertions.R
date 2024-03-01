#' Assert Nested List can be used as Format Argument in Reformat.
#'
#' @param object (`list`) to assert.
#' @returns invisible `TRUE` or an error message if the criteria are not fulfilled.
#'
#' @export
#' @examples
#' format <- list(
#'   df1 = list(
#'     var1 = rule("X" = "x", "N" = c(NA, ""))
#'   ),
#'   df2 = list(
#'     var1 = rule(),
#'     var2 = rule("f11" = "F11", "NN" = NA)
#'   ),
#'   df3 = list()
#' )
#'
#' assert_valid_format(format)
assert_valid_format <- function(object) {
  coll <- checkmate::makeAssertCollection()

  # Check object.
  checkmate::assert_list(object, names = "unique", types = "list", add = coll)

  # Check table level.
  mapply(
    function(x, xtable) {
      checkmate::assert_list(
        x,
        names = "unique",
        types = "rule",
        any.missing = FALSE,
        .var.name = paste0("[", xtable, "]"),
        add = coll
      )
    },
    object,
    names(object)
  )

  checkmate::reportAssertions(coll)
}

#' Assert List can be Converted into a Nested List Compatible with the Format Argument of Reformat.
#'
#' @param object (`list`) to assert.
#' @returns invisible `TRUE` or an error message if the criteria are not fulfilled.
#'
#' @export
#' @examples
#' format <- list(
#'   df1 = list(
#'     var1 = list("X" = "x", "N" = c(NA, ""))
#'   ),
#'   df2 = list(
#'     var1 = list(),
#'     var2 = list("f11" = "F11", "NN" = NA)
#'   ),
#'   df3 = list()
#' )
#'
#' assert_valid_list_format(format)
assert_valid_list_format <- function(object) {
  coll <- checkmate::makeAssertCollection()

  # Check object.
  checkmate::assert_list(object, names = "unique", types = "list", add = coll)

  # Check table level.
  mapply(
    function(x, xtable) {
      checkmate::assert_list(
        x,
        names = "unique",
        types = "list",
        any.missing = FALSE,
        .var.name = paste0("[", xtable, "]"),
        add = coll
      )
    },
    object,
    names(object)
  )

  # Check variable level.
  mapply(
    function(x, xtable) {
      xvar <- names(x)
      mapply(
        function(x, xvar) {
          checkmate::assert_list(
            x,
            names = "unique",
            types = c("character", "numeric", "logical"),
            .var.name = paste0("[", xtable, ".", xvar, "]"),
            add = coll
          )
        },
        x,
        xvar
      )
    },
    object,
    names(object)
  )

  checkmate::reportAssertions(coll)
}

# assert_all_tablenames ----

#' Assert that all names are among names of a `list` of `data.frame`.
#'
#' @param db (`list` of `data.frame`) input to check for the presence of tables.
#' @param tab (`character`) the names of the tables to be checked.
#' @param null_ok (`flag`) can `x` be NULL.
#' @param qualifier (`string`) to be returned if the check fails.
#' @returns invisible `TRUE` or an error message if the criteria are not fulfilled.
#'
#' @export
#'
#' @examples
#' lsd <- list(
#'   mtcars = mtcars,
#'   iris = iris
#' )
#' assert_all_tablenames(lsd, c("mtcars", "iris"), qualifier = "first test:")
assert_all_tablenames <- function(db, tab, null_ok = TRUE, qualifier = NULL) {
  checkmate::assert_list(db, types = "data.frame", names = "unique")
  checkmate::assert_character(tab, null.ok = null_ok)
  checkmate::assert_string(qualifier, null.ok = TRUE)

  diff <- setdiff(tab, names(db))

  if (length(diff) == 0) {
    invisible(NULL)
  } else {
    stop(
      paste(qualifier, "Expected table names:", toString(diff), "not in", deparse(substitute(db)))
    )
  }
}

# assert_one_tablenames ----

#' Assert that at least one name is among table names of a `list` of `data.frame`.
#'
#' @param db (`list` of `data.frame`) input to check for the presence or tables.
#' @param tab (`character`) the names of the tables to be checked.
#' @param null_ok (`flag`) can `x` be NULL.
#' @param qualifier (`string`) to be returned if the check fails.
#' @returns invisible `TRUE` or an error message if the criteria are not fulfilled.
#'
#' @keywords internal
assert_one_tablenames <- function(db, tab, null_ok = TRUE, qualifier = NULL) {
  checkmate::assert_list(db, types = "data.frame", names = "unique")
  checkmate::assert_character(tab, null.ok = null_ok)
  checkmate::assert_string(qualifier, null.ok = TRUE)

  diff <- setdiff(tab, names(db))

  common <- intersect(tab, names(db))

  if (length(common) > 0) {
    invisible(NULL)
  } else {
    stop(
      paste(qualifier, "At least one of:", toString(tab), "is expected to be a table name of", deparse(substitute(db)))
    )
  }
}
