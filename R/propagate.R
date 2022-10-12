
#' Propagate Column
#'
#' `propagate`copy columns from a given table of a `dm` object to all tables based on other common columns. If several
#' rows are associated with the same key, the rows will be duplicated in the receiving tables. In safe mode, the key
#' must be unique in the original table.
#'
#' @param db (`dm`) object for which some variable need to be propagated.
#' @param from (`string`) the name of the table where the variables to propagate are stored.
#' @param add (`character`) the names of the variables to propagate.
#' @param by (`character`) the key binding the `from` table to the other tables.
#' @param safe (`flag`) should the key be checked for uniqueness in the `from` table.
#'
#' @return updates `dm` object.
#' @export
#' @examples
#' db <- dm::dm_nycflights13()
#' res <- propagate(db, "airlines", "name", "carrier", safe = TRUE)
#'
propagate <- function(db, from, add, by, safe = FALSE) {
  checkmate::assert_class(db, "dm")
  checkmate::assert_subset(from, names(db))
  checkmate::assert_subset(add, colnames(db[[from]]))
  checkmate::assert_subset(by, colnames(db[[from]]))
  checkmate::assert_flag(safe)

  if (safe) {
    keys <- db[[from]][, by]
    if (anyDuplicated(keys)) stop(paste("Duplicated key"))
  }

  for (tab_name in names(db)) {
    tab_colnames <- colnames(db[[tab_name]])

    if (!all(add %in% tab_colnames) & all(by %in% tab_colnames)) {
      missing_var <- setdiff(add, tab_colnames)
      sel_var <- c(missing_var, by)

      cat(paste0("\nUpdating: ", tab_name, " with: ", toString(missing_var)))

      db <-
        db %>%
        dm::dm_zoom_to(!!tab_name) %>%
        dm::left_join(!!from, select = dplyr::all_of(sel_var), by = by) %>%
        dm::dm_update_zoomed()
    } else {
      cat(paste0("\nSkipping: ", tab_name))
    }
  }
  cat("\n")
  return(db)
}
