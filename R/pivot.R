#' Transforming data.frame into Wide Format
#'
#' @details instead of nesting duplicated values, the function will throw an error if the same parameter is
#'   provided twice for the same observation.
#'
#' @param data (`data.frame`) to be pivoted.
#' @param id (`character`) the name of the column identifying the observations. It will correspond to the row names
#'   of the output.
#' @param param_from (`character`) the name of the column containing the names of the parameters to be pivoted. The
#'   unique values in this column will become column names in the output.
#' @param value_from (`character`) the name of the column containing the values that will populate the output.
#'
#' @return `data.frame` in a wide format.
#'
#' @export
#' @examples
#' test_data <- data.frame(
#'   the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
#'   the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
#'   the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "other"),
#'   the_val = c(65, 165, "M", 66, "F", 166, 155, TRUE)
#' )
#'
#' mini_pivot_wider(test_data, "the_obs", "the_param", "the_val")
mini_pivot_wider <- function(data,
                             id,
                             param_from,
                             value_from) {
  # check for duplication of observation-parameter
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 3)
  checkmate::assert_character(id, len = 1)
  checkmate::assert_character(param_from, len = 1)
  checkmate::assert_character(value_from, len = 1)
  checkmate::assert_subset(c(id, param_from, value_from), colnames(data))
  checkmate::assert_false(any(duplicated(data[, c(id, param_from)])))

  unique_id <- sort(unique(data[[id]]))
  param <- data[[param_from]]

  mini_data <- data[, c(id, param_from, value_from)]
  data_ls <- split(mini_data, param)

  # transform to named vector
  data_vec <-
    lapply(
      data_ls,
      function(x) setNames(x[[value_from]], x[[id]])
    )

  # query each id in each param
  all_vec <- lapply(data_vec, function(x) setNames(x[unique_id], unique_id))

  bind_data <- as.data.frame(all_vec)

  res <- cbind(id = unique_id, bind_data)
  rownames(res) <- NULL

  res
}

#' Transforming data.frame with Complex Identifiers into Wide Format
#'
#' @details This function allows to identify observations on the basis of several columns. Warning: Instead of nesting
#'   duplicated values, the function will throw an error if the same parameter is provided twice for the same
#'   observation.
#'
#' @param data (`data.frame`) to be pivoted.
#' @param id (`character`) the name of the columns whose combination uniquely identify the observations.
#' @param param_from (`character`) the name of the column containing the names of the parameters to be pivoted. The
#'   unique values in this column will become column names in the output.
#' @param value_from (`character`) the name of the column containing the values that will populate the output.
#' @param drop_na (`logical`) should column containing only `NAs` be dropped.
#'
#' @return `data.frame` in a wide format.
#'
#' @export
#' @examples
#' test_data <- data.frame(
#'   the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
#'   the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
#'   the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "other"),
#'   the_val = c(65, 165, "M", 66, "F", 166, 155, TRUE)
#' )
#'
#' multi_pivot_wider(test_data, c("the_obs", "the_obs2"), "the_param", "the_val")
#' multi_pivot_wider(test_data, "the_obs2", "the_param", "the_val")
multi_pivot_wider <- function(data,
                              id,
                              param_from,
                              value_from,
                              drop_na = FALSE) {
  # check for duplication of observation-parameter
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 3)
  checkmate::assert_character(id)
  checkmate::assert_character(param_from, len = 1)
  checkmate::assert_character(value_from, len = 1)
  checkmate::assert_false(any(duplicated(data[, c(id, param_from)])))
  checkmate::assert_subset(c(id, param_from, value_from), colnames(data))

  # find a way to sort
  unique_id <- unique(data[id])
  key <- apply(unique_id[id], 1, paste, collapse = "-")
  unique_id <- cbind(key, unique_id)

  param <- data[[param_from]]

  mini_data <- data[, c(param_from, value_from)]
  f_key <- apply(data[id], 1, paste, collapse = "-")
  mini_data <- cbind(f_key, mini_data)

  data_ls <- split(mini_data, param)

  # Transform to named vector, the first column is the key.
  data_vec <-
    lapply(
      data_ls,
      function(x) setNames(x[[value_from]], x[, 1])
    )

  # query each id in each param
  all_vec <- lapply(data_vec, function(x) x[unique_id[, 1]])

  if (drop_na) all_vec <- Filter(function(x) !all(is.na(x)), all_vec)

  bind_data <- do.call(cbind, all_vec)

  res <- cbind(unique_id[, -1, drop = FALSE], bind_data)

  rownames(res) <- NULL
  res
}

#' Transforming data.frame with multiple Data Column into Wide Format
#'
#' @details This function is adapted to cases where the data are distributed in several columns while the name of the
#'   parameter is in one. Typical example is `adsub` where numeric data are stored in `AVAL` while categorical data are
#'   in `AVALC`.
#'
#' @param data (`data.frame`) to be pivoted.
#' @param id (`character`) the name of the columns whose combination uniquely identify the observations.
#' @param param_from (`character`) the name of the columns containing the names of the parameters to be pivoted. The
#'   unique values in this column will become column names in the output.
#' @param value_from (`character`) the name of the column containing the values that will populate the output.
#' @param labels_from (`character`) the name of the column congaing the labels of the new columns. from. If not
#'   provided, the labels will be equal to the column names. When several labels are available for the same column, the
#'   first one will be selected.
#'
#' @return `list` of `data.frame` in a wide format with label attribute attached to each columns.
#'
#' @export
#' @examples
#' test_data <- data.frame(
#'   the_obs = c("A", "A", "A", "B", "B", "B", "C", "D"),
#'   the_obs2 = c("Ax", "Ax", "Ax", "Bx", "Bx", "Bx", "Cx", "Dx"),
#'   the_param = c("weight", "height", "gender", "weight", "gender", "height", "height", "other"),
#'   the_label = c(
#'     "Weight (Kg)", "Height (cm)", "Gender", "Weight (Kg)",
#'     "Gender", "Height (cm)", "Height (cm)", "Pre-condition"
#'   ),
#'   the_val = c(65, 165, NA, 66, NA, 166, 155, NA),
#'   the_val2 = c(65, 165, "M", 66, "F", 166, 155, TRUE)
#' )
#'
#' x <- poly_pivot_wider(
#'   test_data,
#'   c("the_obs", "the_obs2"),
#'   "the_param",
#'   c("the_val", "the_val2"),
#'   "the_label"
#' )
#' x
#' Reduce(function(u, v) merge(u, v, all = TRUE), x)
poly_pivot_wider <- function(data,
                             id,
                             param_from,
                             value_from,
                             labels_from = NULL) {
  # other tests are performed at lower levels.
  checkmate::assert_character(value_from, unique = TRUE)

  # Create new labels for new columns.
  if (is.null(labels_from) || labels_from == param_from) {
    new_labels <- unique(data[[param_from]])
    names(new_labels) <- new_labels
  } else {
    checkmate::assert_character(labels_from, len = 1)
    checkmate::assert_subset(labels_from, colnames(data))

    new_labels_df <- data[, c(labels_from, param_from)]
    new_labels_df <- unique(new_labels_df)

    new_labels <- as.character(new_labels_df[[labels_from]])
    names(new_labels) <- as.character(new_labels_df[[param_from]])
  }

  # Retrieve old labels.
  old_labels <- lapply(data, attr, "label")
  n_old_label <- names(old_labels)
  null_label <- unlist(lapply(old_labels, is.null))
  old_labels[null_label] <- n_old_label[null_label]
  old_labels <- unlist(old_labels)

  all_labels <- c(new_labels, old_labels)

  res_ls <- list()
  for (n_value_from in value_from) {
    res <- multi_pivot_wider(
      data = data,
      id = id,
      param_from = param_from,
      value_from = n_value_from,
      drop_na = TRUE
    )

    res <- attr_label_df(res, all_labels[colnames(res)])
    res_ls[[n_value_from]] <- res
  }
  res_ls
}
