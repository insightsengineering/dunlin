#' Transforming Empty Strings and White Spaces to NAs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' SAS imports missing data as empty strings or white spaces. This helper function replaces the empty strings and white
#' space-only character and levels by `NAs`.
#' 
#' @param x (`character` or `factor`) where empty of white space should be transformed to NAs.
#'
#' @return `character` or `factor` without explicit NA
#'
#' @export
#' @examples
#' 
#' char1 = c(" ", "    ", "a", "b", "", "")
#' h_ws_to_na(char1)
#' 
#' fact1 <- as.factor(char1)
#' h_ws_to_na(fact1)
#' 
h_ws_to_na <- function(x) {
  
  assert_multi_class(x, c("character", "factor"))
  
  if (is.factor(x)) {
    
    levels_x <- levels(x)
    
    ws_levels <- grepl("^\\s*$", levels_x) | levels_x == ""
     
    levels(x)[ws_levels] <- NA
    
    x
  } 
  else if (is.character(x)) {
    
    ws_char <- grepl("^\\s*$", x) | x == ""
    
    x[ws_char] <- NA
    
    x
  }
}

#' Transforming Empty Strings and White Spaces to Explicit NAs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' SAS imports missing data as empty strings or white spaces. This helper function is a thin wrapper around
#' [dunlin::h_ws_to_na] which replaces them with explicit missing level.
#' 
#' @param x (`character` or `factor`) where empty of white space should be transformed to NAs.
#'
#' @return `factor` with explicit NA
#'
#' @export
#' @examples
#' 
#' char1 = c(" ", "    ", "a", "b", "", "")
#' h_ws_to_explicit_na(char1)
#' 
#' fact1 <- as.factor(char1)
#' h_ws_to_explicit_na(fact1)
#' 
h_ws_to_explicit_na <- function(x, na_level = "<Missing>") {
  
  assert_multi_class(x, c("character", "factor"))
  
  forcats::fct_explicit_na(h_ws_to_na(x), na_level = na_level)
}
