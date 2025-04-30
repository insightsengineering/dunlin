#' Create Subject-level Flag from Long Data
#'
#' @description
#' Utility for creating subject-level flags from data frames that are
#' more than one line per subject.
#' For example, use this function to create a flag indicating whether a
#' subject experienced any serious adverse events.
#' 
#' The function works by first creating a logical variable in `data_long`
#' indicating whether the condition passed in the `...` argument is met.
#' If a subject has true on any row, then the new variable is added
#' to `data` as `TRUE`, otherwise that subject's value is populated with a `FALSE`.
#'
#' @param data (`data.frame`)\cr
#'   a subject-level data frame, e.g. `adsl`
#' @param data_long (`data.frame`)\cr
#'   a long data frame that is more than one line per subject, e.g. `adae`.
#'   The expressions passed in `...` will be evaluated in this data frame.
#' @param ... ([`data-masking`][rlang::args_data_masking])\cr
#'   Name-flag pairs. The name gives the name of the new subject-level
#'   flag column. The value is a condition that results in a logical vector.
#'   These name-flag pairs are passed directly to `dplyr::mutate(...)`
#' @param .key (`character`)\cr
#'   Key columns create flags within and to merge by.
#'   Default is `'USUBJID'`
#'
#' @returns Subject-level data frame
#' @export
#'
#' @examples
#' adsl <- tibble::tribble(
#'   ~USUBJID,      ~SEX,
#'   "01-701-1015", "F",
#'   "01-701-1023", "M",
#'   "01-701-1028", "M"
#' )
#'
#' adae <- tibble::tribble(
#'   ~USUBJID,      ~AESER, ~AEACN,
#'   "01-701-1015", "Y",    "DOSE NOT CHANGED",
#'   "01-701-1015", "N",    "DOSE NOT CHANGED",
#'   "01-701-1028", "N",    "DRUG WITHDRAWN"
#' )
#'
#' subject_level_flag(
#'   data = adsl,
#'   data_long = adae,
#'   ANY_AESER = AESER == "Y",
#'   ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN"
#' )
subject_level_flag <- function(data, data_long, ..., .key = "USUBJID") {
  # check inputs ---------------------------------------------------------------
  dots <- rlang::enquos(...)
  checkmate::assert_class(data, "data.frame")
  checkmate::assert_class(data_long, "data.frame")
  if (!is_named(dots)) {
    rlang::abort("Arguments passed in `...` must be named.")
  }
  if (any(names(dots) %in% c(names(data), names(data_long)))) {
    rlang::abort("Named arguments in `...` cannot match existing names in `data` or `data_long`.")
  }
  if (!all(.key %in% names(data)) || !all(.key %in% names(data_long))) {
    rlang::abort("Names specified in `.key` argument must appear in both `data` and `data_long`.")
  }
  if (nrow(data) != nrow(dplyr::distinct(data[.key]))) {
    rlang::abort("The `.key` columns must uniquely identify each row in `data`.")
  }

  # add new flags --------------------------------------------------------------
  data_flags <-
    data_long |>
    # add new variables
    dplyr::mutate(!!!dots) |>
    dplyr::select(all_of(c(.key, names(dots))))

  # check all new columns are class logical ------------------------------------
  lapply(
    names(dots),
    FUN = \(x) {
      if (!inherits(data_flags[[x]], "logical")) {
        rlang::abort(glue::glue("New variable '{x}' must be class <logical>."))
      }
    }
  )

  # if any TRUE for a subject, make all values TRUE for the subject ------------
  data_flags <-
    data_flags |>
    dplyr::mutate(
      .by = all_of(.key),
      dplyr::across(
        .cols = dplyr::all_of(names(dots)),
        .fns = \(x) max(x, na.rm = TRUE) |> as.logical()
      )
    ) |>
    dplyr::distinct()

  # add new flags to `data` ----------------------------------------------------
  data |>
    # merge in flag variables
    dplyr::left_join(
      data_flags,
      by = .key
    ) |>
    # fill in FALSE for IDs in subject-level data frame that are not in data_long
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(names(dots)),
        .fns = \(x) ifelse(is.na(x), FALSE, x)
      )
    )
}
