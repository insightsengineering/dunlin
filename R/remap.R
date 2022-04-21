
#' Remap values
#'
#' @param db (`dm`) object input.
#' @param map (`list`) in a specific format.
#'
#' @note Using the keyword `All` as a table name will change the corresponding variable in every table where it appears.
#'
#' @return a `dm` object with re coded variables as factor. If not re mapped, original levels are preserved.
#' @export
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
#' my_map <- list(
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
#'   All = list(
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
#' res <- remap(db, my_map)
remap <- function(db, map) {
  assert_remap(map)

  remap_tab <- intersect(names(map), names(db))
  if ("All" %in% names(map)) remap_tab <- c("All", remap_tab)

  # iterate over highest map level (tab)
  for (tab in remap_tab) {
    local_map <- map[[tab]]

    # iterate over variables
    for (col in names(local_map)) {
      key_val <- local_map[[col]]

      key_len <- unlist(lapply(key_val, length))
      val_nam <- rep(names(key_val), key_len)
      dic_map <- setNames(val_nam, unlist(key_val))

      if (tab == "All") {
        for (sel_tab in names(db)) {
          db <- h_remap_tab(db, sel_tab, col, dic_map)
        }
      } else {
        db <- h_remap_tab(db, tab, col, dic_map)
      }
    }
  }
  db
}

#' Remap a Specific Variable in a Specific Column
#'
#' @param db (`dm`) object input.
#' @param tab (`string`) the name of a table.
#' @param col (`string`) the name of a variable in a table.
#' @param dic_map (named `vector`) a dictionary with the mapping values, with the format `c(new = old)`.
#'
#' @note If `tab` is not a valid table name of the `db` object, the original object is returned. Similarly, if `col` is
#'   not a valid column of the selected `tab` in the object, the original object is returned. This behavior is desirable
#'   when a variable that exists in most but not all tables has to be re coded.
#'
#'
#' @return a `dm` object with re coded variables as factor.
#' @export
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
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "num" = 1:6
#' )
#'
#' db <- dm(df1, df2)
#'
#' dic_map <- setNames(c("A", "B", "Missing"), c("a", "b", NA))
#' res <- h_remap_tab(db, "df1", "char", dic_map)
h_remap_tab <- function(db, tab, col, dic_map) {
  if (!tab %in% names(db)) {
    return(db)
  }
  if (!col %in% colnames(db[[tab]])) {
    return(db)
  }

  ori <- db[[tab]][[col]]
  ori_char <- as.character(ori)
  new <- dic_map[ori_char]

  # capture levels if factor and unique values otherwise to preserve all levels.
  ori_lvl <- levels(as.factor(ori))
  unknow_lvl <- setdiff(ori_lvl, names(dic_map))
  new_level <- c(unique(dic_map), unknow_lvl)

  is_na <- which(is.na(new))
  new[is_na] <- ori_char[is_na]

  if (any(is.na(names(dic_map)))) {
    na_replacement <- dic_map[is.na(names(dic_map))][1]
    new[is.na(new)] <- na_replacement
    new_level <- c(setdiff(new_level, na_replacement), na_replacement)
  }

  new <- factor(new, levels = new_level)
  new <- unname(new)

  db <- db %>%
    dm_zoom_to(!!tab) %>%
    mutate(!!col := new) %>%
    dm_update_zoomed()

  db
}



#' Assert the Mapping Object
#'
#' @param map (`list`)
#'
#' @return `NULL` if the `map` object fits the criteria for a mapping list.
#' @export
#'
#' @examples
#' my_map <- list(
#'   df1 = list(
#'     char = list(
#'       "A" = c("a", "k"),
#'       "B" = "b"
#'     ),
#'     char2 = list(
#'       "A" = c("a", "k"),
#'       "B" = "b"
#'     )
#'   ),
#'   df2 = list(
#'     num = list(
#'       "11" = "1",
#'       "22" = NA
#'     )
#'   ),
#'   All = list(
#'     fact = list(
#'       "F1" = "f1",
#'       "F2" = "f2"
#'     ),
#'     other = list(
#'       "x" = "X"
#'     )
#'   )
#' )
#'
#' assert_remap(my_map)
assert_remap <- function(map) {
  msg <- NULL

  # assert unique table names
  assert_list(map)
  res <- duplicated(names(map))
  if (any(res)) {
    msg <- paste("\nDuplicated table names:", toString(unique(names(map)[res])))
  }

  # assert unique variable name in each table
  var_remap <- lapply(map, names)
  res <- unlist(lapply(var_remap, function(x) test_character(x, unique = TRUE)))
  if (!all(res)) {
    msg <- c(msg, paste("\nDuplicated Variable name inside table:", toString(unique(names(var_remap)[!res]))))
  }

  # assert 1:1 remapping
  tab_remap <- lapply(map, function(x) lapply(x, unlist))
  col_remap <- unlist(tab_remap, recursive = FALSE)
  res <- unlist(lapply(col_remap, function(x) test_character(x, unique = TRUE)))
  if (!all(res)) {
    msg <- c(msg, paste("\nDuplicated mapping inside:", toString(names(col_remap)[!res])))
  }

  if (is.null(msg)) {
    invisible(NULL)
  } else {
    stop(msg)
  }
}
