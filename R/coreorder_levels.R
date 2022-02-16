#' Reorder Two Columns Levels Simultaneously
#'
#' @param df (`data.frame`) with PARAM and PARAMCD variables.
#' @param paramcd_levels (`character`) ordered levels of PARAMCD.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
#' coreorder_levels(df, "PARAMCD", "PARAM", levels_primary = c("C", "A", "B"))
#' 
coreorder_levels <- function(df, primary, secondary, levels_primary) {

  assert_data_frame(df)
  assert_subset(c(primary, secondary), colnames(df))
  
  # check unique relationship
  df_key <- df[, c(primary, secondary)]
  df_key <- unique(df_key)
  
  if (any(duplicated(df_key[, primary])) || any(duplicated(df_key[, secondary]))) {
    print(df_key[duplicated(df_key), ])
    stop("non univoque relation between values in primary and secondary column")
  }
  
  keys <- setNames(as.character(df_key[[secondary]]), as.character(df_key[[primary]]))

  all_levels_primary <- c(levels_primary, setdiff(levels(df[, primary]), levels_primary))
  all_levels_secondary <- keys[all_levels_primary]
  
  
  df[, primary] <- factor(df[, primary], all_levels_primary)
  df[, secondary] <- factor(df[, secondary], all_levels_secondary)

  df
}