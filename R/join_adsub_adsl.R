#' Join `adsub` to `adsl`
#'
#' @param adam_db (`dm`) object input with an `adsl` and `adsub` table.
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
#'
#' @return a `dm` object with new columns in the `adsl` table.
#'
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
#' db <- dm::dm(adsl, adsub)
#'
#' db <- dm::dm_add_pk(db, adsl, STUDYID)
#' db <- dm::dm_add_fk(db, adsub, STUDYID, adsl)
#'
#' x <- join_adsub_adsl(adam_db = db)
#' x <- join_adsub_adsl(adam_db = db, continuous_var = c("w", "h"), categorical_var = "h")
join_adsub_adsl <- function(adam_db,
                            keys = c("USUBJID", "STUDYID"),
                            continuous_var = "all",
                            categorical_var = "all",
                            continuous_suffix = "",
                            categorical_suffix = "_CAT") {
  checkmate::assert_class(adam_db, "dm")
  checkmate::assert_names(names(adam_db), must.include = c("adsl", "adsub"))
  checkmate::assert_names(names(adam_db$adsub), must.include = c("PARAM", "PARAMCD", "AVAL", "AVALC", keys))
  checkmate::assert_names(names(adam_db$adsl), must.include = keys)
  checkmate::assert_string(continuous_suffix)
  checkmate::assert_string(categorical_suffix)

  value_col <- c("AVAL", "AVALC")
  vars_ls <- list(continuous_var, categorical_var)
  suffix_ls <- list(continuous_suffix, categorical_suffix)

  vars_ls <- lapply(vars_ls, function(x) {
    if (identical(x, "all")) {
      unique(adam_db$adsub$PARAMCD)
    } else {
      x
    }
  })

  # Create new names.
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
  final_names <- unique(sapply(vars_nam, names))
  already_in_adsl <- final_names %in% colnames(adam_db$adsl)
  if (any(already_in_adsl)) {
    warning(
      paste(
        toString(final_names[already_in_adsl]),
        "already exist in adsl, the name will default to another values.
Please change `continuous_suffix` or `categorical_suffix` to avoid automatic reneaming"
      )
    )
  }

  # Test if categorical and continuous column will result in the same column name.
  final_names_ls <- lapply(vars_nam, names)
  in_both <- final_names_ls[[1]] %in% final_names_ls[[2]]
  if (any(in_both)) {
    warning(
      paste(
        toString(final_names_ls[[1]][in_both]),
        "are new columns for continuous and categorical variable,
Please set different `continuous_suffix` or `categorical_suffix`
or select different columns to avoid automatic renaming."
      )
    )
  }

  adsub_wide_ls <-
    adam_db$adsub %>%
    poly_pivot_wider(id = keys, param_from = "PARAMCD", value_from = value_col, labels_from = "PARAM")

  # Generate a non-existing table name.
  new_tab_nam <- "adsub_1"
  i <- 1
  while (new_tab_nam %in% names(adam_db)) {
    new_tab_nam <- gsub("_[0-9]*$", paste0("_", i), new_tab_nam)
    i <- i + 1
  }

  for (i in seq_along(value_col)) {
    adsub_df <- adsub_wide_ls[[value_col[i]]]

    nam_vec <- vars_nam[[i]]

    adam_db <-
      adam_db %>%
      dm(!!new_tab_nam := adsub_df, .name_repair = "unique")

    adam_db <-
      adam_db %>%
      dm_zoom_to("adsl") %>%
      left_join(!!new_tab_nam, by = keys, select = any_of(nam_vec)) %>%
      dm_update_zoomed() %>%
      dm_select_tbl(-!!new_tab_nam)
  }

  adam_db
}
