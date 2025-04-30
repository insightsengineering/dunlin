adsl <- tibble::tribble(
  ~USUBJID,      ~SEX,
  "01-701-1015", "F",
  "01-701-1023", "M",
  "01-701-1028", "M"
)

adae <- tibble::tribble(
  ~USUBJID,      ~AESER, ~AEACN,
  "01-701-1015", "Y",    "DOSE NOT CHANGED",
  "01-701-1015", "N",    "DOSE NOT CHANGED",
  "01-701-1028", "N",    "DRUG WITHDRAWN",
  "01-701-1023",  NA,    NA
)

test_that("subject_level_flag() works", {
  # check function runs without error/warnings/messages
  expect_silent(
    df_flags <-
      subject_level_flag(
        data = adsl,
        data_long = adae,
        ANY_AESER = AESER == "Y",
        ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN"
      )
  )

  # check AESER derivation
  expect_equal(
    df_flags[df_flags$ANY_AESER, "USUBJID"][[1]],
    adae[adae$AESER %in% "Y", "USUBJID"][[1]]
  )

  # check ANY_DRUG_WITHDRAWN derivation
  expect_equal(
    df_flags[df_flags$ANY_DRUG_WITHDRAWN, "USUBJID"][[1]],
    adae[adae$AEACN %in% "DRUG WITHDRAWN", "USUBJID"][[1]]
  )

  # check no NAs in subject level flags
  expect_equal(
    df_flags[c("ANY_AESER", "ANY_DRUG_WITHDRAWN")] |> is.na() |> sum(),
    0L
  )
})

test_that("subject_level_flag() messaging", {
  # check for error when inputs are not named
  expect_error(
    subject_level_flag(
      data = adsl,
      data_long = adae,
      AESER == "Y",
      ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN"
    ),
    "must be named"
  )

  # check error message with matching column names
  expect_error(
    subject_level_flag(
      data = adsl,
      data_long = adae,
      SEX = AESER == "Y",
      ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN"
    ),
    "cannot match existing names in"
  )

  # message when key not present in both data frames
  expect_error(
    subject_level_flag(
      data = adsl,
      data_long = adae,
      ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN",
      .key = "NOT_PRESENT"
    ),
    "must appear in both"
  )

  # message when key doesn't uniquely identify rows in `data`
  expect_error(
    subject_level_flag(
      data = mtcars,
      data_long = mtcars,
      ANY_DRUG_WITHDRAWN = AEACN == "DRUG WITHDRAWN",
      .key = "gear"
    ),
    "must uniquely identify each row"
  )

  # message when a non-logical is created
  expect_error(
    subject_level_flag(
      data = adsl,
      data_long = adae,
      ANY_AESER = AESER
    ),
    "must be class <logical>"
  )
})
