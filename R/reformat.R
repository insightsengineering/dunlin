#' Reformat Values
#' @param obj object to reformat
#' @param format (`rule`) or (`list`) of `rules` depending on the class of obj
#' @export
#' @details
#' For character values, reformatting will only change those should be changed.
#' NA values are not changed, if the rule does not contain `NA_character_`.
#' Factors works similarly to characters.
#' For `dm` objects, see `reformat` for more details.
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
reformat.logical <- function(obj, format) {
  if (is(format, "empty_rule")) {
    return(obj)
  }
  stop("Not implemented, only empty_rule is available for logical!")
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

#' @export
reformat.list <- function(obj,
                        format) {
  checkmate::assert_list(obj, type = c("data.frame", "tibble"))
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
#' @param db (`dm`  or `list` of `data.frame`) object input.
#' @param format (`list`) in a specific format.
#'
#' @note Using the keyword `All` as a table name will change the corresponding variable in every table where it appears.
#' @return a `dm` object. If not reformatted, value remain unchanged.
#'
#' @keywords internal
apply_reformat <- function(db, format = NULL) {
  UseMethod("apply_reformat")
}

#' @export
#' 
apply_reformat.list <- function(db, format = NULL) {
  
  checkmate::assert_list(db, types = c("data.frame", "tibble"))
  
  remap_tab <- names(format)
  
  if (is.null(format)) {
    return(db)
  }
  
  # Propagate ALL
  if ("ALL" %in% toupper(names(format))) {
    remap_tab <- c("All", remap_tab)
    names(format)[toupper(names(format)) == "ALL"] <- "All"
  }
  
  full_format <- lapply(names(db), function(tab) fuse_sequentially(format[[tab]], format[["All"]]))
  names(full_format) <- names(db)
  
  for(tab in names(full_format)) {
    local_map <- full_format[[tab]]
    local_map <- local_map[names(local_map) %in% names(db[[tab]])]
    
    
    db[[tab]][names(local_map)] <- mapply(
      function(rl, col) reformat(db[[tab]][[col]], format = rl), local_map, names(local_map), SIMPLIFY = FALSE
    )
  }
  
  db
}


#' @export
#' 
apply_reformat.dm <- function(db, format = NULL) {
  ls_df <- as.list(db)
  ls_res <- apply_reformat(ls_df, format = format)
  res <- as_dm(ls_res)
  
  pk <- dm::dm_get_all_pks(db)
  
  for (i in seq_len(nrow(pk))) {
    res <- dm_add_pk(res, !!pk[["table"]][i], !!pk[["pk_col"]][[i]])
  }
  
  fk <- dm::dm_get_all_fks(db)
  
  for (i in seq_len(nrow(fk))) {
    res <- dm::dm_add_fk(
      res,
      !!fk[["child_table"]][i],
      !!fk[["child_fk_cols"]][[i]],
      !!fk[["parent_table"]][i],
      !!fk[["parent_key_cols"]][[i]]
    )
  }
  
  res
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
#' 
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
