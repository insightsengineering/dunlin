#' Reorder Two Columns Levels Simultaneously
#'
#' @details The function expect a 1:1 matching between the elements of the two selected column.
#'
#' @param df (`data.frame`) with two column whose factors should be reordered.
#' @param primary (`string`) the name of the column on which the levels reordering should be based.
#' @param secondary (`string`) the name of the column whose levels should be reordered following the levels of the
#'   primary column.
#' @param levels_primary (`character`) the levels in the desired order. Existing levels that are not included will be
#'   placed afterward in their current order.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   SUBJID = 1:3,
#'   PARAMCD = factor(c("A", "B", "C")),
#'   PARAM = factor(paste("letter", LETTERS[1:3]))
#' )
#' co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("C", "A", "B"))
co_relevels <- function(df, primary, secondary, levels_primary) {
  checkmate::assert_data_frame(df, min.rows = 1)
  checkmate::assert_subset(c(primary, secondary), colnames(df))
  checkmate::assert_character(levels_primary, min.len = 1)
  checkmate::assert_vector(df[[primary]], any.missing = FALSE)
  checkmate::assert_vector(df[[secondary]], any.missing = FALSE)

  df[, primary] <- as.factor(df[[primary]])
  df[, secondary] <- as.factor(df[[secondary]])

  # check unique relationship
  df_key <- df[, c(primary, secondary)]
  df_key <- unique(df_key)

  if (any(duplicated(df_key[[primary]])) || any(duplicated(df_key[[secondary]]))) {
    rlang::abort("non univoque relation between values in primary and secondary column")
  }

  keys <- setNames(as.character(df_key[[secondary]]), as.character(df_key[[primary]]))

  all_levels_primary <- c(levels_primary, setdiff(levels(df[[primary]]), levels_primary))
  all_levels_secondary <- keys[all_levels_primary]

  df[, primary] <- factor(df[[primary]], all_levels_primary)
  df[, secondary] <- factor(df[[secondary]], all_levels_secondary)

  df
}
