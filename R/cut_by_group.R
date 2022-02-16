#' Cutting data by group
#' 
#' @details Function used to categorize numeric data stored in long format depending on their group. Intervals are
#'   closed on the right (and open on the left).
#'
#' @param df (`dataframe`) with a column of data to be cut and a column specifying the group of each observation.
#' @param col_data (`character`) the column containing the data to be cut.
#' @param col_group (`character`) the column containing the names of the groups according to which the data should be
#'   split.
#' @param group  (`nested list`) providing for each parameter value that should be analyzed in a categorical way: the
#'   name of the parameter (`character`), a series of breakpoints (`vector`) where the first breakpoints is typically
#'   `-Inf` and the last `Inf`, and a series of name which will describe each category (`vector`).
#' @param cat_col (`character`) the name of the new column in which the cut label should he stored.
#'
#' @export
#'
#' @return `data.frame` with a column containing categorical values.
#' @examples
#' group <- list(
#'   list(
#'     "Height",
#'     c(-Inf, 150, 170, Inf),
#'     c("=<150", "150-170", ">170")
#'     ),
#'  list(
#'    "Weight",
#'    c(-Inf, 65, Inf),
#'    c("=<65", ">65")
#'  ),
#'  list(
#'    "Age",
#'    c(-Inf, 31, Inf),
#'    c("=<31", ">31")
#'  ),
#'  list(
#'    "PreCondition",
#'    c(-Inf, 1, Inf),
#'    c("=<1", "<1")
#'  )
#')
#' 
#' data <- data.frame(
#'   SUBJECT = rep(letters[1:10], 4),
#'   PARAM = rep(c("Height", "Weight", "Age", "other"),  each = 10),
#'   AVAL = c(rnorm(10, 165, 15), rnorm(10, 65, 5), runif(10, 18, 65), rnorm(10, 0, 1)),
#'   index = 1:40
#' )
#' 
#' cut_by_group(data, "AVAL", "PARAM", group, "my_new_categories")
#' 
#' 
cut_by_group <- function(df,
                         col_data,
                         col_group,
                         group,
                         cat_col) {
  
  assert_data_frame(df)
  assert_subset(c(col_data, col_group), colnames(df))
  assert_numeric(df[, col_data])
  assert_list(group)
  
  df[cat_col] <- NA

  for (g in group) {
    selected_row <- df[[col_group]] == g[[1]]

    df[selected_row, cat_col] <- as.character(cut(df[[col_data]][selected_row], breaks = g[[2]], labels = g[[3]]))
  }
  df
}
