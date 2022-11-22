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
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w", "h", "w_cat", "h_cat"))
})

test_that("join_adsub_adsl works as expected when no column is selected", {
  res <- expect_silent(join_adsub_adsl(adam_db = db, continuous_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w_cat", "h_cat"))

  res <- expect_silent(join_adsub_adsl(adam_db = db, categorial_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE", "w", "h"))

  res <- expect_silent(join_adsub_adsl(adam_db = db, continuous_var = NULL, categorial_var = NULL))
  checkmate::expect_class(res, "dm")
  checkmate::expect_subset(names(res$adsl), c("USUBJID", "STUDYID", "AGE"))
})
