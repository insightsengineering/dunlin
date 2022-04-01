#' @include AdamDB-validate.R
NULL

# AdamDB-class ----

#' `AdamDB`
#'
#' The `AdamDB` class is an extension of `list` with additional validation criteria.
#'
#' @details The additional criteria are:
#'  - The list elements must be of class `data.frame`.
#'
#' @aliases AdamDB
#' @exportClass AdamDB
.AdamDB <- setClass(
  "AdamDB",
  contains = "list"
)

# Data requirements ----
# Names of the tables required in `AdamDB` with their mandatory columns
.tables_AdamDB <- list(
  "adsl" = c("USUBJID", "STUDYID")
)

# Validation ----
S4Vectors::setValidity2("AdamDB", function(object) {
  msg <- NULL

  msg <- c(msg, validate_tables(object, .tables_AdamDB))
  msg <- c(msg, validate_primary_key(object, "adsl", "USUBJID"))

  if (is.null(msg)) TRUE else msg
})

# AdamDB-constructor ----

#' @rdname AdamDB-class
#'
#' @param object (`list` of `data.frame`) to convert to [`AdamDB`].
#'
#' @export
#' @examples
#' library(scda.2021)
#' dat <- rcd_2021_03_22
#' ad <- AdamDB(dat)
AdamDB <- function(object) {
  assert_class(object, "list")
  lapply(object, function(table) assert_class(table, "data.frame"))

  .AdamDB(object)
}
