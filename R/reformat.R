#' Reformat Values
#' @param obj object to reformat
#' @param format (`rule`) or (`list`) of `rules` depending on the class of obj
#' @export
#' @details
#' For character values, reformating will only change those should be changed.
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
#' @return a `dm` object with re coded variables as factor. If not reformatted, original levels are preserved.
#'
#' @examples
#' library(dm)
#'
#' df1 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
#' )
#' df2 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1"), levels = c("f1", "f2", "fx")),
#'   "num" = 1:6
#' )
#'
#' db <- dm(df1, df2)
#'
#' new_format <- list(
#'   df1 = list(
#'     char = list(
#'       "A" = c("a", "k"),
#'       "B" = "b"
#'     )
#'   ),
#'   df2 = list(
#'     num = list(
#'       "11" = "1",
#'       "22" = "2"
#'     )
#'   ),
#'   ALL = list(
#'     fact = list(
#'       "F1" = "f1",
#'       "F2" = "f2",
#'       "FX" = "fx",
#'       "<Missing>" = NA
#'     ),
#'     other = list(
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
      if (is.null(key_val)) {
        next
      }

      key_len <- unlist(lapply(key_val, length))
      val_nam <- rep(names(key_val), key_len)
      dic_map <- setNames(val_nam, unlist(key_val))

      if (tab == "All") {
        for (sel_tab in names(db)) {
          db <- h_reformat_tab(db, sel_tab, col, dic_map)
        }
      } else {
        db <- h_reformat_tab(db, tab, col, dic_map)
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
#' @param dic_map (named `vector`) a dictionary with the mapping values, with the format `c(new = old)` sorted according
#'   to the desired order of factor levels. Existing values not present in the dictionary are preserved, but their
#'   corresponding levels will be placed after the levels of the remapped values. If `NAs` or empty string are mapped,
#'   their corresponding level will be last. If `dic_map` is `NULL`, the selected column will be converted into factor
#'   without further modifications.
#'
#' @note If `tab` is not a valid table name of the `db` object, the original object is returned. Similarly, if `col` is
#'   not a valid column of the selected `tab` in the object, the original object is returned. This behavior is desirable
#'   when a variable that exists in most but not all tables has to be re coded.
#'
#' @note Both empty string and `NAs` can be re coded if needed.
#'
#' @note The `label` attribute of the column is preserved.
#'
#' @return a `dm` object with re coded variables as factor.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dm)
#'
#' df1 <- data.frame(
#'   "char" = c("", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
#' )
#' df2 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "num" = 1:6
#' )
#'
#' db <- dm(df1, df2)
#'
#' dic_map <- setNames(c("A", "B", "Missing", "Empty"), c("a", "b", NA, ""))
#' res <- h_reformat_tab(db, "df1", "char", dic_map)
#' }
h_reformat_tab <- function(db, tab, col, dic_map) {
  checkmate::assert_class(db, "dm")
  checkmate::assert_string(tab)
  checkmate::assert_string(col)
  checkmate::assert_character(dic_map, null.ok = TRUE)
  checkmate::assert_character(names(dic_map), unique = TRUE, null.ok = TRUE)

  if (!tab %in% names(db)) {
    return(db)
  }
  if (!col %in% colnames(db[[tab]])) {
    return(db)
  }

  ori <- db[[tab]][[col]]

  if (is.null(dic_map)) {
    db <- db %>%
      dm_zoom_to(!!tab) %>%
      mutate(!!col := as.factor(.env$ori)) %>%
      dm_update_zoomed()

    return(db)
  }

  ori_char <- as.character(ori)
  new <- dic_map[ori_char]

  # Preserve all levels.
  # if factor: capture levels. if other: capture unique values.
  ori_lvl <- levels(as.factor(ori))
  unknow_lvl <- setdiff(ori_lvl, names(dic_map))
  new_level <- c(unique(dic_map), unknow_lvl)
  new_level <- unique(new_level)

  # Replace NA if necessary and put NA level at the end.
  is_na <- which(is.na(new))
  new[is_na] <- ori_char[is_na]

  if (any(is.na(names(dic_map)))) {
    na_replacement <- dic_map[is.na(names(dic_map))][1]
    new[is.na(new)] <- na_replacement
    new_level <- c(setdiff(new_level, na_replacement), na_replacement)
  }

  # Replace Empty String if necessary and put empty string level at the end.
  if ("" %in% names(dic_map)) {
    empty_replacement <- dic_map[which(names(dic_map) == "")]
    new[which(ori_char == "")] <- empty_replacement
    new_level <- c(setdiff(new_level, empty_replacement), empty_replacement)
  }

  new <- factor(new, levels = new_level)
  new <- unname(new)

  # Preserve label attribute
  lab <- attr(ori, "label")
  if (!is.null(lab)) {
    attr(new, "label") <- lab
  }

  db <- db %>%
    dm_zoom_to(!!tab) %>%
    mutate(!!col := .env$new) %>%
    dm_update_zoomed()

  db
}
