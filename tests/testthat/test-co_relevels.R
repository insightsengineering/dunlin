# co_relevels ----

test_that("co_relevels works as expected.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("C", "A", "B")))

  expect_equal(levels(res[, "PARAMCD"]), c("C", "A", "B"))
  expect_equal(levels(res[, "PARAM"]), c("letter C", "letter A", "letter B"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})

test_that("co_relevels works as expected when only a subset of levels is used.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("B")))

  expect_equal(levels(res[, "PARAMCD"]), c("B", "A", "C"))
  expect_equal(levels(res[, "PARAM"]), c("letter B", "letter A", "letter C"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})

test_that("co_relevels works as expected when a supplementary level is used.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "B", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  res <- expect_silent(co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("K")))

  expect_equal(levels(res[, "PARAMCD"]), c("K", "A", "B", "C"))
  expect_equal(levels(res[, "PARAM"]), c("letter A", "letter B", "letter C"))

  expect_equal(as.character(df[, "PARAMCD"]), as.character(res[, "PARAMCD"]))
  expect_equal(as.character(df[, "PARAM"]), as.character(res[, "PARAM"]))
})

test_that("co_relevels retuns an error when the matching between the selected column is not unique.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", "A", "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  expect_error(co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("A")),
               "non univoque relation between values in primary and secondary column")
})

test_that("co_relevels retuns an error when NAs are present.", {

  df <- data.frame(SUBJID = 1:3, PARAMCD = factor(c("A", NA, "C")), PARAM = factor(paste("letter", LETTERS[1:3])))
  expect_error(co_relevels(df, "PARAMCD", "PARAM", levels_primary = c("A")),
               "Assertion on 'df[[primary]]' failed: Contains missing values (element 2).", fixed = TRUE)
})
