#' Join `adsub` to `adsl`
#'
#' @param db (`dm`) object input with an `adsl` and `adsub` table.
#' @param keys (`character`) the name of the columns in `adsl` uniquely identifying a row.
#' @param continuous_var (`character`) the value of a parameter in the `PARAMCD` column of the `adsub` table from which
#'   columns containing continuous values should be created.
#' @param categorial_var (`character`) the value of a parameter in the `PARAMCD` column of the `adsub` table from which
#'   columns containing categorical values should be created.
#' @param continuous_suffix (`string`) the suffixes to add to the newly generated columns containing continuous values.
#' @param categorial_suffix (`string`) the suffixes to add to the newly generated columns containing categorical values.
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
#' db <- dm_add_pk(db, adsl, STUDYID)
#' db <- dm_add_fk(db, adsub, STUDYID, adsl)
#'
#' x <- join_adsub_adsl(adam_db = db)
#' x <- join_adsub_adsl(adam_db = db, continuous_var = c("w", "h"), categorial_var = "h")
join_adsub_adsl <- function(adam_db,
                            keys = c("USUBJID", "STUDYID"),
                            continuous_var = "all",
                            categorial_var = "all",
                            continuous_suffix = "",
                            categorial_suffix = "_cat") {
  checkmate::assert_class(adam_db, "dm")
  checkmate::assert_names(names(adam_db), must.include = c("adsl", "adsub"))
  checkmate::assert_names(names(adam_db$adsub), must.include = c("PARAM", "PARAMCD", "AVAL", "AVALC", keys))
  checkmate::assert_string(continuous_suffix)
  checkmate::assert_string(categorial_suffix)

  value_col <- c("AVAL", "AVALC")
  vars_ls <- list(continuous_var, categorial_var)
  suffix_ls <- list(continuous_suffix, categorial_suffix)

  adsub_wide_ls <-
    adam_db$adsub %>%
    poly_pivot_wider(id = keys, param_from = "PARAMCD", value_from = value_col, labels_from = "PARAM")


  for (i in seq_along(value_col)) {
    adsub_df <- adsub_wide_ls[[value_col[i]]]

    nam_vec <- vars_ls[[i]]

    # Enable selection of all parameters.
    nam_vec <- if (identical(nam_vec, "all")) {
      unique(adam_db$adsub$PARAMCD)
    } else {
      nam_vec
    }

    if (!is.null(nam_vec)) {
      names(nam_vec) <- paste0(nam_vec, suffix_ls[[i]])
    }

    adam_db <-
      adam_db %>%
      dm(adsub_df, .name_repair = "unique")

    adam_db <-
      adam_db %>%
      dm_zoom_to(adsl) %>%
      left_join(adsub_df, by = keys, select = any_of(nam_vec)) %>%
      dm_update_zoomed() %>%
      dm_select_tbl(-adsub_df)
  }

  adam_db
}
