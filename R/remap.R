
#' Remap values
#'
#' @param db (`dm`) object input.
#' @param map (`list`) in a specific format.
#'
#' @return a `dm` object with re coded variables as factor.
#' @export
#'
#' @examples
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
#' my_map <- list(
#'   df1 = list(
#'     char = list(
#'       "A" = c("a", "k"), 
#'       "B" = "b"
#'     ),
#'     fact = list(
#'       "F1" = "f1",
#'       "F2" = "f2"
#'     )
#'   ),
#'   df2 = list(
#'     num = list(
#'       "11" = 1,
#'       "22" = 2
#'     )
#'   )
#' )
#' 
#' res <- remap(db, my_map)
#' 
#' 
remap <- function(db, map) {
  
  remap_tab <- intersect(names(map), names(db))
  
  for (tab in remap_tab) {
    
    local_map <- map[[tab]]
    
    for (col in names(local_map)) {

      key_val <- local_map[[col]]
      
      key_len <- unlist(lapply(key_val, length))
      val_nam <- rep(names(key_val), key_len)
      dic_map <- setNames(val_nam, unlist(key_val))
      
    
      ori <- as.character(db[[tab]][[col]])
      new <- dic_map[ori]
      
      unknow_lvl <- setdiff(ori, names(dic_map))
      new_level <- c(unique(dic_map), unknow_lvl)

      is_na <- which(is.na(new))
      new[is_na] <- ori[is_na]
      
      new <- factor(new, levels = new_level)
      
      db <-
      db %>% 
        dm_zoom_to(!!tab) %>% 
        mutate(!!col := new) %>% 
        dm_update_zoomed()
    }
  }
  db
}
