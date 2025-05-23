#' Propagate Column
#'
#' `propagate`copy columns from a given table of a `list` of `data.frame` to all tables based on other
#' common columns. If several rows are associated with the same key, the rows will be duplicated in the receiving
#' tables. In safe mode, the key must be unique in the original table.
#'
#' @param db (`list` of `data.frame`) object for which some variable need to be propagated.
#' @param from (`string`) the name of the table where the variables to propagate are stored.
#' @param add (`character`) the names of the variables to propagate.
#' @param by (`character`) the key binding the `from` table to the other tables.
#' @param safe (`flag`) should the key be checked for uniqueness in the `from` table.
#'
#' @returns updated `list` of `data.frame`.
#'
#' @rdname propagate
#' @export
#'
propagate <- function(db, from, add, by, safe = TRUE) {
  UseMethod("propagate")
}

#' @rdname propagate
#' @export
#'
#'
#' @examples
#' df1 <- data.frame(
#'   id1 = c("a", "a", "c", "d", "e", "f"),
#'   id2 = c("A", "B", "A", "A", "A", "A"),
#'   int = c(1, 2, 3, 4, 5, 6),
#'   bool = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' df2 <- data.frame(
#'   id1 = c("a", "a", "d", "e", "f", "g"),
#'   id2 = c("A", "B", "A", "A", "A", "A")
#' )
#'
#' df3 <- data.frame(
#'   id1 = c("a", "c", "d", "e", "f", "x"),
#'   id2 = c("A", "A", "A", "A", "B", "A"),
#'   int = c(11, 22, 33, 44, 55, 66)
#' )
#'
#' db <- list(df1 = df1, fd2 = df2, df3 = df3)
#' propagate(db, from = "df1", add = c("int", "bool"), by = c("id1", "id2"))
#'
propagate.list <- function(db, from, add, by, safe = TRUE) {
  checkmate::assert_list(db, types = "data.frame", names = "unique")
  checkmate::assert_names(names(db), must.include = from)
  checkmate::assert_names(colnames(db[[from]]), must.include = add)
  checkmate::assert_names(colnames(db[[from]]), must.include = by)
  checkmate::assert_flag(safe)

  if (safe) {
    keys <- db[[from]][, by]
    if (anyDuplicated(keys)) rlang::abort(paste("Duplicated key"))
  }
  toJoin <- db[[from]]

  for (tab_name in setdiff(names(db), from)) {
    tab_colnames <- colnames(db[[tab_name]])
    if (!all(add %in% tab_colnames) && all(by %in% tab_colnames)) {
      missing_var <- setdiff(add, tab_colnames)
      sel_var <- c(missing_var, by)
      sel_tab <- toJoin[, sel_var]

      cat(paste0("\nUpdating: ", tab_name, " with: ", toString(missing_var)))

      db[[tab_name]] <- db[[tab_name]] %>%
        dplyr::left_join(sel_tab, by = by, multiple = "all")
    } else {
      cat(paste0("\nSkipping: ", tab_name))
    }
  }
  cat("\n")
  db
}
