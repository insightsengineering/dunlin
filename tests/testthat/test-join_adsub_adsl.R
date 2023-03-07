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

db <- dm::dm(adsl, adsub)

db <- dm_add_pk(db, adsl, STUDYID)
db <- dm_add_fk(db, adsub, STUDYID, adsl)

test_that("join_adsub_adsl works as expected with default values", {
  res <- expect_silent(join_adsub_adsl(adam_db = db))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w", "h", "w_CAT", "h_CAT"))
})

test_that("join_adsub_adsl works as expected when no column is selected", {
  res <- expect_silent(join_adsub_adsl(adam_db = db, continuous_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w_CAT", "h_CAT"))

  res <- expect_silent(join_adsub_adsl(adam_db = db, categorical_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w", "h"))

  res <- expect_silent(join_adsub_adsl(adam_db = db, continuous_var = NULL, categorical_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE"))
})

test_that("join_adsub_adsl throw a warning when column already exist in adsl.", {
  new_db <- db %>%
    dm::dm_zoom_to("adsl") %>%
    mutate(h = 160) %>%
    dm::dm_update_zoomed()

  expect_warning(join_adsub_adsl(adam_db = new_db), "h already exist in adsl, the name will default to another values")
})

test_that("join_adsub_adsl throw a warning when two new columns would have the same name.", {
  expect_warning(join_adsub_adsl(adam_db = db, continuous_suffix = "_x", categorical_suffix = "_x"))
})
