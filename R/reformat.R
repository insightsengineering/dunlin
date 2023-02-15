#' Reformat Values
#' @param obj object to reformat
#' @param format (`rule`) or (`list`) of `rules` depending on the class of obj
#' @export
#' @details
#' For character values, reformatting will only change those should be changed.
#' NA values are not changed, if the rule does not contain `NA_character_`.
#' Factors works similarly to characters.
#' For `dm` objects, see `apply_reformat` for more details.
#' `empty_rule` will do nothing to the data.
reformat <- function(obj, format) {
  UseMethod("reformat")
}
#' @export
reformat.default <- function(obj, format) {
  stop("Not implemented!")
}
#' @export
reformat.character <- function(obj, format) {
  if (is(format, "empty_rule")) {
    return(obj)
  }
  checkmate::assert_class(format, "rule")
  value_match <- unlist(format)
  m <- match(obj, value_match)
  obj[!is.na(m)] <- names(format)[m[!is.na(m)]]
  return(obj)
}
#' @export
reformat.factor <- function(obj, format) {
  if (is(format, "empty_rule")) {
    return(obj)
  }
  checkmate::assert_class(format, "rule")
  if (any(is.na(format))) {
    obj <- forcats::fct_na_value_to_level(obj)
  }
  forcats::fct_recode(obj, !!!format)
}
#' @export
reformat.dm <- function(obj,
                        format) {
  checkmate::assert_list(format, names = "unique", types = "list")
  lapply(format, function(x) {
    checkmate::assert_list(x, names = "unique", types = "rule")
  })
  if (length(format) == 0) {
    return(obj)
  }
  apply_reformat(obj, format)
}

#' Reformat values
#'
#' @param db (`dm`) object input.
#' @param format (`list`) in a specific format.
#'
#' @note Using the keyword `All` as a table name will change the corresponding variable in every table where it appears.
#' @return a `dm` object. If not reformatted, value remain unchanged.
#'
#' @examples
#' library(dm)
#'
#' df1 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
#' )
#' df2 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"), levels = c("f1", "f2", "fx"))
#' )
#'
#' db <- dm(df1, df2)
#'
#' new_format <- list(
#'   df1 = list(
#'     char = rule(
#'       "A" = c("a", "k"),
#'       "B" = "b"
#'     )
#'   ),
#'   ALL = list(
#'     fact = rule(
#'       "F1" = "f1",
#'       "F2" = "f2",
#'       "<Missing>" = NA
#'     ),
#'     other = rule(
#'       "x" = "X"
#'     )
#'   )
#' )
#'
#' res <- apply_reformat(db, new_format)
apply_reformat <- function(db, format = NULL) {
  if (is.null(format)) {
    return(db)
  }
  remap_tab <- intersect(names(format), names(db))
  if ("ALL" %in% toupper(names(format))) {
    remap_tab <- c("All", remap_tab)
    names(format)[toupper(names(format)) == "ALL"] <- "All"
  }

  # iterate over highest map level (tab).
  for (tab in remap_tab) {
    local_map <- format[[tab]]

    # iterate over variables
    for (col in names(local_map)) {
      key_val <- local_map[[col]]

      # if no mapping is provided for a variable, skip this remapping.
      if (is.null(key_val) || is(key_val, "empty_rule")) {
        next
      }

      if (tab == "All") {
        for (sel_tab in names(db)) {
          db <- h_reformat_tab(db, sel_tab, col, key_val)
        }
      } else {
        db <- h_reformat_tab(db, tab, col, key_val)
      }
    }
  }
  db
}

#' Reformat a Variable in a Specific Column and Table
#'
#' @param db (`dm`) object input.
#' @param tab (`string`) the name of a table.
#' @param col (`string`) the name of a variable in a table.
#' @param format (`rule`) object.
#'
#' @note If `tab` is not a valid table name of the `db` object, the original object is returned. Similarly, if `col` is
#'   not a valid column of the selected `tab` in the object, the original object is returned. This behavior is desirable
#'   when a variable that exists in most but not all tables has to be re coded.
#'
#' @note Both empty string and `NAs` can be re coded if needed.
#'
#' @note The `label` attribute of the column is preserved.
#'
#' @return a `dm` object with re coded variables respecting its original data type.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dm)
#'
#' df1 <- data.frame(
#'   "char" = c("", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
#' )
#' df2 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"))
#' )
#'
#' db <- dm(df1, df2)
#'
#' dic_map <- rule(A = "a", B = "b", Missing = NA, Empty = "")
#' res <- h_reformat_tab(db, "df1", "char", dic_map)
#' }
h_reformat_tab <- function(db, tab, col, format) {
  checkmate::assert_class(db, "dm")
  checkmate::assert_string(tab)
  checkmate::assert_string(col)
  checkmate::assert_class(format, "rule")

  if (!tab %in% names(db)) {
    return(db)
  }
  if (!col %in% colnames(db[[tab]])) {
    return(db)
  }

  ori <- db[[tab]][[col]]

  new <- reformat(ori, format)
  db <- db %>%
    dm_zoom_to(!!tab) %>%
    mutate(!!col := .env$new) %>%
    dm_update_zoomed()

  db
}
