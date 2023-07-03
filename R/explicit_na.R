#' Encode Categorical Missing Values in a `list` of `data.frame`
#'
#' @details This is a helper function to encode missing values (i.e `NA` and `empty string`) of every `character` and
#'   `factor` variable found in a `list` of `data.frame`. The `label` attribute of the columns is preserved.
#'
#' @param data (`list` of `data.frame`) to be transformed.
#' @param omit_tables (`character`) the names of the tables to omit from processing.
#' @param omit_columns (`character`) the names of the columns to omit from processing.
#' @param char_as_factor (`logical`) should character columns be converted into factor.
#' @param na_level (`string`) the label to encode missing levels.
#'
#' @return `list` of `data.frame` object with explicit missing levels.
#' @export
#'
#' @examples
#'
#' df1 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "char2" = c("A", "B", NA, "A", "K", "X"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
#' )
#' df2 <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "num" = c(1:5, NA)
#' )
#' df3 <- data.frame(
#'   "char" = c(NA, NA, "A")
#' )
#'
#' db <- list(df1 = df1, df2 = df2, df3 = df3)
#'
#' ls_explicit_na(db)
#' ls_explicit_na(db, omit_tables = "df3", omit_columns = "char2")
#'
ls_explicit_na <- function(data,
                           omit_tables = NULL,
                           omit_columns = NULL,
                           char_as_factor = TRUE,
                           na_level = "<Missing>") {
  checkmate::assert_list(data, types = "data.frame", names = "unique")
  checkmate::assert_character(omit_tables, null.ok = TRUE)
  checkmate::assert_character(omit_columns, null.ok = TRUE)
  checkmate::assert_flag(char_as_factor)
  checkmate::assert_string(na_level)

  modif_tab <- setdiff(names(data), omit_tables)
  if (length(modif_tab) < 1) {
    return(data)
  }

  data[modif_tab] <- lapply(
    data[modif_tab],
    h_df_explicit,
    omit_columns = omit_columns,
    char_as_factor = char_as_factor,
    na_level = na_level
  )

  data
}

#' Encode Categorical Missing Values in a `data.frame`.
#'
#' @inheritParams ls_explicit_na
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   "char" = c("a", "b", NA, "a", "k", "x"),
#'   "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
#'   "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA),
#'   "num" = c(1:5, NA)
#' )
#'
#' h_df_explicit(df)
#' h_df_explicit(df, omit_columns = c("fact", "x"))
#' }
h_df_explicit <- function(df,
                          omit_columns = NULL,
                          char_as_factor = TRUE,
                          na_level = "<Missing>") {
  na_list <- list(x = c("", NA))
  names(na_list) <- na_level
  na_rule <- rule(.lst = na_list)

  df %>%
    mutate(
      across(
        where(~ is.character(.x) | is.factor(.x)) & !any_of(.env$omit_columns),
        ~ reformat(.x, format = .env$na_rule, string_as_fct = .env$char_as_factor, .na_last = TRUE)
      )
    )
}
