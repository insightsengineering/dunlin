#' Internal Helper Functions for Validation of `AdamDB` Objects
#'
#' These functions are only used internally to validate the format of an input `list` object and therefore
#' not exported.
#'
#' @name validate_AdamDB
#' @param object (`list`) object to validate.
#' @return A character vector with the validation failure messages,
#'   or `NULL` in case validation passes.
NULL

#' @describeIn validate_AdamDB validate the presence of mandatory tables and columns.
#'
#' @param object (`list` of `data.frame`) to check for validity.
#' @param criteria (`list`) with the name of the mandatory tables and their required columns.
#'
validate_tables <- function(object, criteria) {
  assert_class(criteria, "list")

  res <- NULL

  # check the presence of required tables.
  tab_ok <- names(criteria) %in% names(object)

  if (!all(tab_ok)) {
    msg_tab <- paste("Required tables are missing:", toString(names(criteria)[!tab_ok]), "\n")
    res <- c(res, msg_tab)
  }

  # check the required column in the required tables that are present.
  for (tab in names(criteria)[tab_ok]) {
    col_ok <- criteria[[tab]] %in% colnames(object[[tab]])

    if (!all(col_ok)) {
      msg_col <- paste("In table", tab, "required columns are missing: ", toString(criteria[[tab]][!col_ok]), "\n")
      res <- c(res, msg_col)
    }
  }

  res
}

#' @describeIn validate_AdamDB validate that key values are present in the primary table and not duplicated.
#'
#' @param object (`list` of `data.frame`) to check for validity.
#' @param primary_table (`string`) the name of the table used as primary table.
#' @param primary_key (`string`) the name of the column used as key.
#'
validate_primary_key <- function(object, primary_table, primary_key) {
  assert_character(primary_table, len = 1)
  assert_character(primary_key, len = 1)

  assert_subset(primary_table, names(object))
  assert_subset(primary_key, colnames(object[[primary_table]]))

  key <- object[[primary_table]][[primary_key]]

  msg <- NULL

  # check for duplication
  dup <- duplicated(key)
  dup_key <- unique(key[dup])

  if (length(dup_key) > 0) {
    msg <- paste("Duplicated keys", primary_key, "in primary table", primary_table, ":", toString(dup_key), "\n")
  }

  foreign_nam <- setdiff(names(object), primary_table)

  foreign_tab <- object[foreign_nam]

  key_val <- lapply(foreign_tab, function(x) if (primary_key %in% colnames(x)) unique(x[[primary_key]]))

  key_val <- unlist(key_val)

  res <- setdiff(key_val, key)

  if (!identical(res, character(0))) {
    msg <- c(
      msg,
      paste("The following key values for", primary_key, "are missing in table", primary_table, ":", toString(res), "\n")
    )
  }

  msg
}
