#' Transforming Empty Strings and White Spaces to NAs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' SAS imports missing data as empty strings or white spaces. This helper function replaces the empty strings and white
#' space-only character and levels by `NAs`.
#' 
#' @param x (`vector`) where empty of white space should be transformed to `NAs`.
#'
#' @return `character` or `factor` without explicit NA. `logical` and `numeric` are returned as `character`.
#'
#' @export
#' @examples
#' char1 <- c(" ", "    ", "a", "b", "", "")
#' h_ws_to_na(char1)
#' 
#' fact1 <- as.factor(char1)
#' h_ws_to_na(fact1)
#' 
#' num1 <- c(1:10) 
#' h_ws_to_na(num1)
#' 
#' logi1 <- c(TRUE, FALSE, NA) 
#' h_ws_to_na(logi1)
#' 
h_ws_to_na <- function(x) {
  
  if (is.factor(x)) {
    
    levels_x <- levels(x)
    
    ws_levels <- grepl("^\\s*$", levels_x) | levels_x == ""
    
    levels(x)[ws_levels] <- NA
  } 
  else if (is.character(x)) {
    
    ws_char <- grepl("^\\s*$", x) | x == ""
    
    x[ws_char] <- NA
  }
  else {x <- as.character(x)}
  x
}

#' Transforming Empty Strings and White Spaces to Explicit NAs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' SAS imports missing data as empty strings or white spaces. This helper function is a thin wrapper around
#' [dunlin::h_ws_to_na] which replaces them with explicit missing level.
#' 
#' @param x (`vector`) where empty of white space should be transformed to `NAs`.
#' @param na_level (`character`) replacement of the missing levels.
#'
#' @return `factor` with explicit NA
#'
#' @export
#' @examples
#' char1 = c(" ", "    ", "a", "b", "", "")
#' h_ws_to_explicit_na(char1)
#' 
#' fact1 <- as.factor(char1)
#' h_ws_to_explicit_na(fact1)
#' 
#' num1 <- c(1, 2, NA)
#' h_ws_to_explicit_na(num1)
#' 
#' logi1 <- c(TRUE, FALSE, NA)
#' h_ws_to_explicit_na(logi1)
#' 
h_ws_to_explicit_na <- function(x, na_level = "<Missing>") {
  
  assert_multi_class(x, c("character", "factor", "logical", "numeric"))
  assert_character(na_level)
  
  res <- forcats::fct_explicit_na(h_ws_to_na(x), na_level = na_level)
  forcats::fct_relevel(res, na_level, after = Inf)
}

#' Transforming Empty Strings and White Spaces to Explicit NAs while Preserving Label
#'
#' @details This function preserves the label attribute.
#'
#' @param x (`character` or `factor` or `logical`) input to be turned into factor with explicit missing level.
#' @param na_level (`character`) the label to encode missing levels.
#'
#' @return `factor` with explicit NA and the same label as the input.
#' 
#' @export
#' @examples
#' char1 = c(" ", "    ", "a", "b", "", "", NA)
#' attr(char1, "label") <- "my_label"
#' 
#' h_as_factor(char1)
#'
h_as_factor <- function(x, na_level = "<Missing>") {

  assert_multi_class(x, c("character", "factor", "logical", "numeric"))

  init_lab <- attr(x, "label")

  res <-  h_ws_to_explicit_na(x, na_level = na_level)

  attr(res, "label") <- init_lab
  
  res
}

#' Setting the Label Attribute
#'
#' @param var (`object`) whose label attribute can be set.
#' @param label (`character`) the label to add.
#'
#' @return `object` with label attribute.
#' 
#' @export
#' @examples
#' x <- c(1:10)
#' attr(x, "label")
#' 
#' y <- attr_label(x, "my_label")
#' attr(y, "label")
#'
attr_label <- function(var, label) {
  
  assert_character(label)

  x <- var
  attr(x, "label") <- label
  
  x
}

#' Setting the Label Attribute to Data Frame Columns
#'
#' @param df (`data.frame`).
#' @param label (`character`) the labels to add.
#'
#' @return `data.frame` with label attributes.
#' 
#' @export
#' @examples
#' res <- attr_label_df(mtcars, letters[1:11])
#' res
#' lapply(res, attr, "label")
#' 
attr_label_df <- function(df, label) {
  
  assert_data_frame(df)
  assert_character(label, len = ncol(df))
  
  res <- mapply(attr_label, var = df, label = as.list(label),  SIMPLIFY = FALSE)
  as.data.frame(res)
}

