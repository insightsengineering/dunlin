# coreorder_levels ----

test_that("coreorder_levels works as expected.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(coreorder_levels(df, "PARAMCD", "PARAM", levels_primary = c("C", "A", "B")))

  expect_equal(levels(res[, "PARAMCD"]), c("C", "A", "B"))
  expect_equal(levels(res[, "PARAM"]), c("letter C", "letter A", "letter B"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})

test_that("coreorder_levels works as expected when only a subset of levels is used.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(coreorder_levels(df, "PARAMCD", "PARAM", levels_primary = c("B")))

  expect_equal(levels(res[, "PARAMCD"]), c("B", "A", "C"))
  expect_equal(levels(res[, "PARAM"]), c("letter B", "letter A", "letter C"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})

test_that("coreorder_levels works as expected when a supplementary level is used.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(coreorder_levels(df, "PARAMCD", "PARAM", levels_primary = c("K")))

  expect_equal(levels(res[, "PARAMCD"]), c("K", "A", "B", "C"))
  expect_equal(levels(res[, "PARAM"]), c("letter A", "letter B", "letter C"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})
