# dm_explicit_na ----

test_that("dm_explicit_na works as expected with default options.", {

  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm::dm(df1, df2)

  res <- expect_silent(dm_explicit_na(db))

  expect_data_frame(res$df1, types = c("factor", "factor", "logical"), nrows = nrow(df1), ncols = ncol(df1))
  expect_factor(res$df1$char,  
                levels = c("a", "b", "k", "x", "<Missing>"), 
                empty.levels.ok = FALSE, any.missing = FALSE)
  expect_factor(res$df1$fact,  levels = c("f1", "f2", "<Missing>"), empty.levels.ok = FALSE, any.missing = FALSE)
  expect_logical(res$df1$logi, any.missing = TRUE)

  expect_data_frame(res$df2, types = c("factor", "factor", "integer"), nrows = nrow(df2), ncols = ncol(df2))
  expect_factor(res$df2$char,  
                levels = c("a", "b", "k", "x", "<Missing>"),
                empty.levels.ok = FALSE, any.missing = FALSE)
  expect_factor(res$df2$fact,  levels = c("f1", "f2", "<Missing>"), empty.levels.ok = FALSE, any.missing = FALSE)
  expect_integer(res$df2$num, any.missing = FALSE)
})

test_that("dm_explicit_na works as expected with optional arguments.", {

  df1 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "logi" = c(NA, FALSE, TRUE, NA, FALSE, NA),
    "num" = 1:6,
    "logi2" = c(NA, FALSE, TRUE, NA, FALSE, NA)
  )
  df2 <- data.frame(
    "char" = c("a", "b", NA, "a", "k", "x"),
    "fact" = factor(c("f1", "f2", NA, NA, "f1", "f1")),
    "num" = 1:6
  )

  db <- dm::dm(df1, df2)

  res <- expect_silent(dm_explicit_na(db, 
                                      char_as_factor = FALSE, 
                                      logical_as_factor = TRUE, 
                                      omit_tables = "df2",
                                      omit_columns = "logi2",
                                      na_level = "Not Present"))

  expect_data_frame(res$df1, types = c("character", "factor", "logical", "numeric", "logical"), nrows = nrow(df1), ncols = ncol(df1))
  expect_character(res$df1$char, any.missing = TRUE)
  expect_factor(res$df1$fact,  levels = c("f1", "f2", "Not Present"), empty.levels.ok = FALSE, any.missing = FALSE)
  expect_factor(res$df1$logi, levels = c("FALSE", "TRUE", "Not Present"), empty.levels.ok = FALSE, any.missing = TRUE)
  expect_integer(res$df1$num, any.missing = FALSE)
  expect_logical(res$df1$logi2, any.missing = TRUE)

  expect_data_frame(res$df2, types = c("character", "factor", "integer"), nrows = nrow(df2), ncols = ncol(df2))
  expect_character(res$df2$char, any.missing = TRUE)
  expect_factor(res$df2$fact,  levels = c("f1", "f2"), empty.levels.ok = TRUE, any.missing = TRUE)
  expect_integer(res$df2$num, any.missing = FALSE)
})
