#' Assert List can be used as Format.
#'
#' @param object (`list`) to assert.
#' @return invisible `TRUE` or an error message if the criteria are not fulfilled.
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
#' assert_valid_format(format)
assert_valid_format <- function(object) {
  coll <- checkmate::makeAssertCollection()

  # Check object.
  checkmate::assert_list(object, names = "unique", type = "list", add = coll)

  # Check table level.
  mapply(
    function(x, xtable) {
      checkmate::assert_list(
        x,
        names = "unique",
        types = "list",
        any.missing = FALSE,
        .var.name = paste("in table:", xtable),
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
            type = c("character", "numeric", "logical"),
            .var.name = paste("in table:", xtable, "variable:", xvar),
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
