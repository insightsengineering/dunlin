
#' Unite Columns of a Table in a `dm` object.
#'
#' @param adam_db (`dm`) object to be transformed.
#' @param dataset (`string`) the name of a table in the `adam_db` object.
#' @param cols (`character`) the name of the columns to unite.
#' @param sep (`string`) the separator for the new column name.
#' @param new (`string`) the name of the new column. If `NULL` the concatenation of `cols` separated by `sep` is used.
#'
#' @importFrom checkmate assert_class assert_string assert_character
#' @importFrom dm dm_zoom_to dm_update_zoomed pull_tbl select unite
#' @importFrom dplyr all_of mutate pull
#' @importFrom magrittr %>%
#' @importFrom rlang sym !! .data .env
#'
#' @return `dm` object with a united column.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- dm_unite(dm::dm_nycflights13(), "airlines", c("carrier", "name"), new = "FUSION")
#' x$airlines
#' }
dm_unite <- function(adam_db, dataset, cols, sep = ".", new = NULL) {
  checkmate::assert_class(adam_db, "dm")
  checkmate::assert_string(dataset)
  checkmate::assert_subset(dataset, names(adam_db))
  checkmate::assert_character(cols, min.len = 1)
  checkmate::assert_subset(cols, colnames(adam_db[[dataset]]))
  checkmate::assert_string(sep)
  checkmate::assert(checkmate::check_null(new) || checkmate::check_string(new))

  int_df <- adam_db %>%
    dm_zoom_to(!!dataset)

  x_interaction <- paste(cols, collapse = sep)

  x_df <- int_df %>%
    select(all_of(cols)) %>%
    pull_tbl()

  lvl <- lapply(x_df, function(y) if (is.factor(y)) levels(y) else unique(y))
  all_lvl_df <- as.data.frame(Reduce(expand.grid, lvl))
  colnames(all_lvl_df) <- cols

  all_lvl <- all_lvl_df %>%
    unite("res", all_of(cols), sep = sep) %>%
    pull("res")

  x_vec <- x_df %>%
    unite("res", all_of(cols), sep = sep) %>%
    pull(.data$res)

  existing_lvl <- intersect(all_lvl, x_vec)
  levels(x_vec) <- existing_lvl

  x_interaction <- if (!is.null(new)) new else x_interaction

  int_df %>%
    mutate(!!x_interaction := .env$x_vec) %>%
    dm_update_zoomed()
}
