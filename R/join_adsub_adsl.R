#' Join `adsub` to `adsl`
#'
#' @param adam_db (`list` of `data.frame`) object input with an `adsl` and `adsub` table.
#' @param keys (`character`) the name of the columns in `adsl` uniquely identifying a row.
#' @param continuous_var (`character`) the value of a parameter in the `PARAMCD` column of the `adsub` table from which
#'   columns containing continuous values should be created. If `"all"`, all parameter values are selected, if `NULL`,
#'   none are selected.
#' @param categorical_var (`character`) the value of a parameter in the `PARAMCD` column of the `adsub` table from which
#'   columns containing categorical values should be created. If `"all"`, all parameter values are selected, if `NULL`,
#'   none are selected.
#' @param continuous_suffix (`string`) the suffixes to add to the newly generated columns containing continuous values.
#' @param categorical_suffix (`string`) the suffixes to add to the newly generated columns containing categorical
#'   values.
#' @param drop_na (`logical`) whether resulting columns containing only `NAs` should be dropped.
#' @param drop_lvl (`logical`) should missing levels be dropped in the resulting columns.
#'
#' @returns a `list` of `data.frame` with new columns in the `adsl` table.
#'
#' @rdname join_adsub_adsl
#' @export
#'
join_adsub_adsl <- function(adam_db,
                            keys,
                            continuous_var,
                            categorical_var,
                            continuous_suffix,
                            categorical_suffix,
                            drop_na = TRUE,
                            drop_lvl = TRUE) {
  UseMethod("join_adsub_adsl")
}

#' @rdname join_adsub_adsl
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c("S1", "S2", "S3", "S4"),
#'   STUDYID = "My_study",
#'   AGE = c(60, 44, 23, 31)
#' )
#'
#' adsub <- data.frame(
#'   USUBJID = c("S1", "S2", "S3", "S4", "S1", "S2", "S3"),
#'   STUDYID = "My_study",
#'   PARAM = c("weight", "weight", "weight", "weight", "height", "height", "height"),
#'   PARAMCD = c("w", "w", "w", "w", "h", "h", "h"),
#'   AVAL = c(98, 75, 70, 71, 182, 155, 152),
#'   AVALC = c(">80", "<=80", "<=80", "<=80", ">180", "<=180", "<=180")
#' )
#'
#' db <- list(adsl = adsl, adsub = adsub)
#'
#' x <- join_adsub_adsl(adam_db = db)
#' x <- join_adsub_adsl(adam_db = db, continuous_var = c("w", "h"), categorical_var = "h")
join_adsub_adsl.list <- function(adam_db,
                                 keys = c("USUBJID", "STUDYID"),
                                 continuous_var = "all",
                                 categorical_var = "all",
                                 continuous_suffix = "",
                                 categorical_suffix = "_CAT",
                                 drop_na = TRUE,
                                 drop_lvl = FALSE) {
  checkmate::assert_list(adam_db, types = "data.frame")
  checkmate::assert_names(names(adam_db), must.include = c("adsl", "adsub"))
  checkmate::assert_names(names(adam_db$adsub), must.include = c("PARAM", "PARAMCD", "AVAL", "AVALC", keys))
  checkmate::assert_names(names(adam_db$adsl), must.include = keys)
  checkmate::assert_numeric(adam_db$adsub$AVAL)
  checkmate::assert_multi_class(adam_db$adsub$AVALC, c("character", "factor"))
  checkmate::assert_string(continuous_suffix)
  checkmate::assert_string(categorical_suffix)
  checkmate::assert_flag(drop_na)
  checkmate::assert_flag(drop_lvl)

  # Empty strings in AVALC are treated as NA.
  adam_db$adsub$AVALC[adam_db$adsub$AVALC == ""] <- NA

  value_col <- c("AVAL", "AVALC")
  vars_ls <- list(continuous_var, categorical_var)
  suffix_ls <- list(continuous_suffix, categorical_suffix)

  # Select variables names.
  vars_ls <- lapply(vars_ls, function(x) {
    if (identical(x, "all")) {
      unique(adam_db$adsub$PARAMCD)
    } else {
      x
    }
  })

  # Create new variable names.
  vars_nam <- mapply(
    function(x, y) {
      if (!is.null(x)) {
        names(x) <- paste0(x, y)
        x
      } else {
        NULL
      }
    },
    vars_ls,
    suffix_ls,
    SIMPLIFY = FALSE
  )

  # Test if new columns already exist in adsl.
  assert_names_notadsl(vars_nam, adam_db$adsl)

  # Test if categorical and continuous column will result in the same column name.
  assert_names_collision(vars_nam)

  # Pivot and keep labels.
  adsub_wide_ls <-
    adam_db$adsub %>%
    poly_pivot_wider(
      id = keys,
      param_from = "PARAMCD",
      value_from = value_col,
      labels_from = "PARAM",
      drop_na = drop_na,
      drop_lvl = drop_lvl
    )

  # Merge categorical and continuous variables.
  for (i in seq_along(value_col)) {
    adsub_df <- adsub_wide_ls[[value_col[i]]]

    # Warning if some columns are entirely NA, hence discarded.
    not_cols <- setdiff(vars_nam[[i]], colnames(adsub_df))
    if (length(not_cols) > 0) {
      type <- ifelse(value_col[i] == "AVALC", "Categorical", "Continuous")
      arg_type <- ifelse(value_col[i] == "AVALC", "categorical_var", "continuous_var")
      warning(
        sprintf(
          "Dropping %s for %s type, No data available. Adjust `%s` argument to silence this warning or set `drop_na = FALSE`", # nolint
          toString(not_cols),
          type,
          arg_type
        )
      )
    }

    # Preserving names.
    common_cols_id <- c(vars_nam[[i]]) %in% colnames(adsub_df)
    common_cols <- vars_nam[[i]][common_cols_id]

    adsub_df <- adsub_df[, c(keys, as.character(common_cols)), drop = FALSE]
    colnames(adsub_df) <- c(keys, names(common_cols))

    adam_db$adsl <- dplyr::left_join(
      x = adam_db$adsl,
      y = adsub_df,
      by = keys
    )
  }

  adam_db
}

# Utility functions ----

assert_names_collision <- function(vars_nam) {
  final_names_ls <- lapply(vars_nam, names)
  in_both <- final_names_ls[[1]] %in% final_names_ls[[2]]
  if (any(in_both)) {
    rlang::warn(
      paste(
        toString(final_names_ls[[1]][in_both]),
        "are new columns for continuous and categorical variable,
Please set different `continuous_suffix` or `categorical_suffix`
or select different columns to avoid automatic renaming."
      )
    )
  }
}

assert_names_notadsl <- function(vars_nam, df) {
  final_names <- unique(sapply(vars_nam, names))
  already_in_adsl <- final_names %in% colnames(df)
  if (any(already_in_adsl)) {
    rlang::warn(
      paste(
        toString(final_names[already_in_adsl]),
        "already exist in adsl, the name will default to another values.
Please change `continuous_suffix` or `categorical_suffix` to avoid automatic renaming"
      )
    )
  }
}
