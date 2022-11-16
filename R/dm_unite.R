
#' Unite Columns of a Table in a `dm` object.
#'
#' @param adam_db (`dm`) object to be transformed.
#' @param dataset (`string`) the name of a table in the `adam_db` object.
#' @param cols (`character`) the name of the columns to unite.
#' @param sep (`string`) the separator for the new column name.
#' @param new (`string`) the name of the new column. If `NULL` the concatenation of `cols` separated by `sep` is used.
#'
#' @return `dm` object with a united column.
#' @export
#'
#' @examples
#' x <- dm_unite(dm::dm_nycflights13(), "airlines", c("carrier", "name"), new = "FUSION")
#' x$airlines
dm_unite <- function(adam_db, dataset, cols, sep = ".", new = NULL) {
  checkmate::assert_class(adam_db, "dm")
  checkmate::assert_string(dataset)
  checkmate::assert_subset(dataset, names(adam_db))
  checkmate::assert_character(cols, min.len = 1)
  checkmate::assert_subset(cols, colnames(adam_db[[dataset]]))
  checkmate::assert_string(sep)
  checkmate::assert_string(new, null.ok = TRUE)

  int_df <- adam_db %>%
    dm_zoom_to(!!dataset)

  x_interaction <- paste(cols, collapse = sep)

  x_df <- int_df %>%
    select(all_of(cols)) %>%
    pull_tbl()

  lvl <- lapply(x_df, function(y) {
    uni <- if (is.factor(y)) levels(y) else unique(y)
    factor(uni, levels = uni)
  })

  # Create alll possible factor combination.
  all_lvl_df <- expand.grid(lvl)

  # Arrange new factors depending on the original factors order to create new levels in the correct order.
  all_lvl <- all_lvl_df %>%
    arrange(across(all_of(cols))) %>%
    unite("res", all_of(cols), sep = sep) %>%
    pull("res")

  # Retrieve existing factors to filter the possible levels
  x_vec <- x_df %>%
    unite("res", all_of(cols), sep = sep) %>%
    pull(.data$res)

  # Filter factors and retrieve existing levels.
  existing_lvl <- intersect(all_lvl, x_vec)
  x_fact <- factor(x_vec, existing_lvl)

  x_interaction <- if (!is.null(new)) new else x_interaction

  int_df %>%
    mutate(!!x_interaction := .env$x_fact) %>%
    dm_update_zoomed()
}
