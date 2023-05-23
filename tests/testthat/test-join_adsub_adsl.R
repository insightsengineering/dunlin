adsl <- tibble::tibble(
  USUBJID = c("S1", "S2", "S3", "S4"),
  STUDYID = "My_study",
  AGE = c(60, 44, 23, 31)
)

adsub <- tibble::tibble(
  USUBJID = c("S1", "S2", "S3", "S4", "S1", "S2", "S3", "S99"),
  STUDYID = "My_study",
  PARAM = c("weight", "weight", "weight", "weight", "height", "height", "height", "weight"),
  PARAMCD = c("w", "w", "w", "w", "h", "h", "h", "w"),
  AVAL = c(98, 75, 70, 71, 182, 155, 152, 50),
  AVALC = c(">80", "<=80", "<=80", "<=80", ">180", "<=180", "<=180", "<=80")
)

ldb <- list(adsl = adsl, adsub = adsub)

# join_adsub_adsl.list ---

test_that("join_adsub_adsl.list works as expected with default values", {
  res <- expect_silent(join_adsub_adsl(adam_db = ldb))
  checkmate::expect_list(res, types = "data.frame", len = 2)
  checkmate::expect_names(names(res$adsl), identical.to = c("USUBJID", "STUDYID", "AGE", "w", "h", "w_CAT", "h_CAT"))
  expect_identical(attr(res$adsl$w, "label"), "weight")
  expect_identical(attr(res$adsl$w_CAT, "label"), "weight")
})

test_that("join_adsub_adsl.list works as expected when no column is selected", {
  res <- expect_silent(join_adsub_adsl(adam_db = ldb, continuous_var = NULL))
  checkmate::expect_list(res, types = "data.frame", len = 2)
  checkmate::expect_names(names(res$adsl), identical.to = c("USUBJID", "STUDYID", "AGE", "w_CAT", "h_CAT"))

  res <- expect_silent(join_adsub_adsl(adam_db = ldb, categorical_var = NULL))
  checkmate::expect_list(res, types = "data.frame", len = 2)
  checkmate::expect_names(names(res$adsl), identical.to = c("USUBJID", "STUDYID", "AGE", "w", "h"))

  res <- expect_silent(join_adsub_adsl(adam_db = ldb, continuous_var = NULL, categorical_var = NULL))
  checkmate::expect_list(res, types = "data.frame", len = 2)
  checkmate::expect_names(names(res$adsl), identical.to = c("USUBJID", "STUDYID", "AGE"))
})

test_that("join_adsub_adsl.list throw a warning when column already exist in adsl.", {
  ldb$adsl <- ldb$adsl %>%
    mutate(h = 160)

  expect_warning(join_adsub_adsl(adam_db = ldb), "h already exist in adsl, the name will default to another values")
})
